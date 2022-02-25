###########################################################################
# Author:  Andrew Ghattas                                                 #
# Program: UFC_Step5.R                                                    #
# Purpose: Fit and use the ML models for simulating the betting strategy  #
###########################################################################

#===================================================================#
# 5a - [Fit and use ML models for simulating the betting strategy]  #
#   Final processing of the dataframe                               #
#===================================================================#

# Read in the data
setwd(dir=paste0(base.dir,"Processed data"))
load(file="4d_All_UFC_Fights.RDATA")
AUF <- All_UFC_Fights
rm(list=ls()[ls() %in% "All_UFC_Fights"])

# Create 1-0 response variable
AUF$Res <- NA
AUF[AUF$Result=="win",]$Res <- 1
AUF[AUF$Result=="loss",]$Res <- 0
AUF$Res <- as.factor(AUF$Res)
table(AUF$Result,useNA="always")
table(AUF$Res,useNA="always")

# Create minimum rank variable, this is the lower rank between Opponent/Fighter for their fight. We'll use this variable later as a hyperparamter
# in the ML fitting, to only consider fights where the minimum rank < X
AUF <-data.frame(AUF,min_rank=apply(X=AUF[,c("F_Rank","O_Rank")],MARGIN=1,FUN=min))

cat("Distribution of min_rank for all fights in data")
windows(10,10)
par(mfrow=c(1,2))
hist(AUF$min_rank,col="green3",main="All fights",breaks=30)
cat("Distribution of min_rank for fights which actually have betting odds")
hist(AUF[is.na(AUF$F_odds)==FALSE,]$min_rank,col="dodgerblue",main="Fights with odds",breaks=30)
dev.off()

# As an additional ML hyper-parameter we'll filter the data to only including tests where the MIN(O/F UFC fights) >= X
AUF$MinFgts <- apply(X=AUF[,c("F_F","O_F")],MARGIN=1,FUN=min)

cat("Distribution of MinFgts for all fights in data")
windows(10,10)
par(mfrow=c(1,2))
hist(AUF$MinFgts,col="green3",main="All fights",breaks=30)
cat("Distribution of MinFgts for fights which actually have betting odds")
hist(AUF[is.na(AUF$F_odds)==FALSE,]$MinFgts,col="dodgerblue",main="Fights with odds",breaks=30)
dev.off()

# Order the fights by date (and within date fight_key so that the 2 entries for the same fight are next to eachother in the data)
AUF <- AUF[order(AUF$Date,AUF$fight_key),]

# Create an indicator for test data so that we can filter these records out for model fitting (note that data was previously sorted so we can just grab the last X records)
AUF$Test <- NA
AUF[(nrow(AUF)-Test.size+1):nrow(AUF),]$Test <- 1
AUF[is.na(AUF$Test)==TRUE,]$Test <- 0
table(AUF$Test,useNA="always")

#-------------------------------------------------------------------------------------------------------#
# Create helper variables which will indicate to the ML function what is response and what is predictor #
#   - Sequentially create smaller x matricies for smaller models considered in the future               #
#-------------------------------------------------------------------------------------------------------#

# Response variable
y <- c("Res")

# Full X
x1 <- c("I5","WCNum","WC1","WC2","WC3","WC4","WC5","WC6","WC7","WC8","WC9","WC10",
        "F_Age","F_Hgt","F_Rch","F_StOr","F_StSw","F_StSp",
        "F_Rank","F_HghRnkFgtLst3","F_HghRnkWinLst3",
        "F_F","F_W","F_L","F_F5","F_W5","F_L5","F_F10","F_W10","F_L10","F_F20","F_W20","F_L20","F_F50","F_W50","F_L50","F_F100","F_W100","F_L100",
        "F_KOTKO_W","F_SUB_W","F_DEC_W","F_KOTKO_L","F_SUB_L","F_DEC_L","F_KOTKO_W5","F_SUB_W5","F_DEC_W5","F_KOTKO_L5","F_SUB_L5","F_DEC_L5","F_KOTKO_W10","F_SUB_W10","F_DEC_W10","F_KOTKO_L10","F_SUB_L10","F_DEC_L10","F_KOTKO_W20","F_SUB_W20","F_DEC_W20","F_KOTKO_L20","F_SUB_L20","F_DEC_L20","F_KOTKO_W50","F_SUB_W50","F_DEC_W50","F_KOTKO_L50","F_SUB_L50","F_DEC_L50","F_KOTKO_W100","F_SUB_W100","F_DEC_W100","F_KOTKO_L100","F_SUB_L100","F_DEC_L100",
        "F_TS_F",
        "F_TS_KOTKOl",
        "F_WinB3",
        "F_WinsP3","F_WinsP5","F_WinsP7","F_WinsP9","F_WinsP11","F_WinsP13","F_WinsP15",
        "F_CtrlSum","F_CtrlPerc",
        "F_Fight_Time",
        "F_KD_p5m","F_Rev_p5m","F_SA_p5m","F_SSL_p5m","F_SST_p5m","F_SSAcc","F_TDL_p5m","F_TDT_p5m","F_TDAcc","F_HeadL_p5m","F_BodyL_p5m","F_LegL_p5m","F_DistL_p5m","F_ClinchL_p5m","F_GrdL_p5m",
        "F_SSL_Beta",
        "F_Opp_KD_p5m","F_Opp_Rev_p5m","F_Opp_SA_p5m","F_Opp_SSL_p5m","F_Opp_SST_p5m","F_Opp_SSAcc","F_Opp_TDL_p5m","F_Opp_TDT_p5m","F_Opp_TDAcc","F_Opp_HeadL_p5m","F_Opp_BodyL_p5m","F_Opp_LegL_p5m","F_Opp_DistL_p5m","F_Opp_ClinchL_p5m","F_Opp_GrdL_p5m",
        "F_Opp_SSL_Beta",
        "O_Age","O_Hgt","O_Rch","O_StOr","O_StSw","O_StSp",
        "O_Rank","O_HghRnkFgtLst3","O_HghRnkWinLst3",
        "O_F","O_W","O_L","O_F5","O_W5","O_L5","O_F10","O_W10","O_L10","O_F20","O_W20","O_L20","O_F50","O_W50","O_L50","O_F100","O_W100","O_L100",
        "O_KOTKO_W","O_SUB_W","O_DEC_W","O_KOTKO_L","O_SUB_L","O_DEC_L","O_KOTKO_W5","O_SUB_W5","O_DEC_W5","O_KOTKO_L5","O_SUB_L5","O_DEC_L5","O_KOTKO_W10","O_SUB_W10","O_DEC_W10","O_KOTKO_L10","O_SUB_L10","O_DEC_L10","O_KOTKO_W20","O_SUB_W20","O_DEC_W20","O_KOTKO_L20","O_SUB_L20","O_DEC_L20","O_KOTKO_W50","O_SUB_W50","O_DEC_W50","O_KOTKO_L50","O_SUB_L50","O_DEC_L50","O_KOTKO_W100","O_SUB_W100","O_DEC_W100","O_KOTKO_L100","O_SUB_L100","O_DEC_L100",
        "O_TS_F",
        "O_TS_KOTKOl",
        "O_WinB3",
        "O_WinsP3","O_WinsP5","O_WinsP7","O_WinsP9","O_WinsP11","O_WinsP13","O_WinsP15",
        "O_CtrlSum","O_CtrlPerc",
        "O_Fight_Time",
        "O_KD_p5m","O_Rev_p5m","O_SA_p5m","O_SSL_p5m","O_SST_p5m","O_SSAcc","O_TDL_p5m","O_TDT_p5m","O_TDAcc","O_HeadL_p5m","O_BodyL_p5m","O_LegL_p5m","O_DistL_p5m","O_ClinchL_p5m","O_GrdL_p5m",
        "O_SSL_Beta",
        "O_Opp_KD_p5m","O_Opp_Rev_p5m","O_Opp_SA_p5m","O_Opp_SSL_p5m","O_Opp_SST_p5m","O_Opp_SSAcc","O_Opp_TDL_p5m","O_Opp_TDT_p5m","O_Opp_TDAcc","O_Opp_HeadL_p5m","O_Opp_BodyL_p5m","O_Opp_LegL_p5m","O_Opp_DistL_p5m","O_Opp_ClinchL_p5m","O_Opp_GrdL_p5m",
        "O_Opp_SSL_Beta")

# Reduced space
x2 <- c("I5","WCNum",
        "F_Age","F_Hgt","F_Rch","F_StOr","F_StSw","F_StSp",
        "F_Rank","F_HghRnkFgtLst3","F_HghRnkWinLst3",
        "F_F","F_W","F_L",
        "F_KOTKO_W","F_SUB_W","F_DEC_W","F_KOTKO_L","F_SUB_L","F_DEC_L",
        "F_TS_F",
        "F_TS_KOTKOl",
        "F_WinB3",
        "F_WinsP3","F_WinsP5","F_WinsP7",
        "F_CtrlSum","F_CtrlPerc",
        "F_Fight_Time",
        "F_KD_p5m","F_Rev_p5m","F_SA_p5m","F_SSL_p5m","F_SST_p5m","F_SSAcc","F_TDL_p5m","F_TDT_p5m","F_TDAcc","F_HeadL_p5m","F_BodyL_p5m","F_LegL_p5m","F_DistL_p5m","F_ClinchL_p5m","F_GrdL_p5m",
        "F_SSL_Beta",
        "F_Opp_KD_p5m","F_Opp_Rev_p5m","F_Opp_SA_p5m","F_Opp_SSL_p5m","F_Opp_SST_p5m","F_Opp_SSAcc","F_Opp_TDL_p5m","F_Opp_TDT_p5m","F_Opp_TDAcc","F_Opp_HeadL_p5m","F_Opp_BodyL_p5m","F_Opp_LegL_p5m","F_Opp_DistL_p5m","F_Opp_ClinchL_p5m","F_Opp_GrdL_p5m",
        "F_Opp_SSL_Beta",
        "O_Age","O_Hgt","O_Rch","O_StOr","O_StSw","O_StSp",
        "O_Rank","O_HghRnkFgtLst3","O_HghRnkWinLst3",
        "O_F","O_W","O_L",
        "O_KOTKO_W","O_SUB_W","O_DEC_W","O_KOTKO_L","O_SUB_L","O_DEC_L",
        "O_TS_F",
        "O_TS_KOTKOl",
        "O_WinB3",
        "O_WinsP3","O_WinsP5","O_WinsP7",
        "O_CtrlSum","O_CtrlPerc",
        "O_Fight_Time",
        "O_KD_p5m","O_Rev_p5m","O_SA_p5m","O_SSL_p5m","O_SST_p5m","O_SSAcc","O_TDL_p5m","O_TDT_p5m","O_TDAcc","O_HeadL_p5m","O_BodyL_p5m","O_LegL_p5m","O_DistL_p5m","O_ClinchL_p5m","O_GrdL_p5m",
        "O_SSL_Beta",
        "O_Opp_KD_p5m","O_Opp_Rev_p5m","O_Opp_SA_p5m","O_Opp_SSL_p5m","O_Opp_SST_p5m","O_Opp_SSAcc","O_Opp_TDL_p5m","O_Opp_TDT_p5m","O_Opp_TDAcc","O_Opp_HeadL_p5m","O_Opp_BodyL_p5m","O_Opp_LegL_p5m","O_Opp_DistL_p5m","O_Opp_ClinchL_p5m","O_Opp_GrdL_p5m",
        "O_Opp_SSL_Beta")

# Combine the xi into a single X
X <- list(x1,x2)
unlist(lapply(X=X,FUN=length))

# Save the processed data so we don't have to keep remaking these helper objects
setwd(dir=paste0(base.dir,"Processed data"))
save(AUF,y,X,file="5a_DataPlusHelperVars.RDATA")
write.csv(AUF,file="5a_AUF.csv",row.names=FALSE)

# Clear the workspace
rm(list = ls()[ !(ls() %in% c(params.and.funcs,"AUF","y","X")) ])

#===================================================================#
# 5b - [Fit and use ML models for simulating the betting strategy]  #
#   Fit the actual ML model                                         #
#===================================================================#

# Save the Machine Learning models here
dir(path=paste0(base.dir,"/Model metadata (temp)"))
dir(path=paste0(base.dir,"/H2O Models"))

#---------------------------------------#
# Gradient Boosted Machine (GBM) models #
#---------------------------------------#

# Start h2o session and disable the progress bar - This computer has 64gb RAM
h2o.init(nthreads=(detectCores()-3), max_mem_size=RAM.to.use.H2O)
h2o.no_progress()

# Hyper-parameter lists
GBM_Hyp <- list()
GBM_Hyp[["Xlist_start_year"]] <- seq(1998,2012,1)
GBM_Hyp[["Xlist_sample_rate"]] <- seq(0.50,1.0,0.05)
GBM_Hyp[["Xlist_col_sample_rate"]] <- seq(0.50,1.0,0.05)
GBM_Hyp[["Xlist_learn_rate"]] <- seq(0.01,0.1,0.01)
GBM_Hyp[["Xlist_max_depth"]] <- seq(2,8,1)
GBM_Hyp[["Xlist_min_rows"]] <- c(25,50,100,200,300,400)
GBM_Hyp[["Xlist_X"]] <- X
GBM_Hyp[["Xlist_ValidSize"]] <- seq(1000,9000,250)
GBM_Hyp[["Xlist_MinFgts"]] <- seq(0,10,1)

# Random grid-search
prog.bar <- txtProgressBar(min=1,max=Rand.iters,style=3)
for(iter in 1:Rand.iters){
  # Initialize random hyper-parameters for this iteration
  X_start_year <- sample(x=GBM_Hyp[["Xlist_start_year"]],size=1)
  X_sample_rate <- sample(x=GBM_Hyp[["Xlist_sample_rate"]],size=1)
  X_col_sample_rate <- sample(x=GBM_Hyp[["Xlist_col_sample_rate"]],size=1)
  X_learn_rate <- sample(x=GBM_Hyp[["Xlist_learn_rate"]],size=1)
  X_max_depth <- sample(x=GBM_Hyp[["Xlist_max_depth"]],size=1)
  X_min_rows <- sample(x=GBM_Hyp[["Xlist_min_rows"]],size=1)
  X_X <- sample(x=GBM_Hyp[["Xlist_X"]],size=1)[[1]]
  X_ValidSize <- sample(x=GBM_Hyp[["Xlist_ValidSize"]],size=1)
  X_MinFgts <- sample(x=GBM_Hyp[["Xlist_MinFgts"]],size=1)
  # Create the dataframes which will eventually be uploaded to H2O
  X_AUF <- AUF[AUF$Test==0,]
  N <- dim(X_AUF)[1]
  X_AUF$Frag <- NA
  X_AUF[(N-X_ValidSize+1):N,]$Frag <- "Valid"
  X_AUF[is.na(X_AUF$Frag),]$Frag <- "Train" # table(X_AUF$Frag,useNA="always")
  X_AUF <- X_AUF[lubridate::year(X_AUF$Date) >= X_start_year &
                 X_AUF$MinFgts >= X_MinFgts,]
  # If resulting training sample is small, or "negative" then skip this iteration
  if(sum(X_AUF$Frag=="Train")<=1000){ next }
  # Load data into h2o
  h2o.removeAll()
  Valid <- as.h2o(X_AUF[X_AUF$Frag=="Valid",], destination_frame="Valid")
  Train <- as.h2o(X_AUF[X_AUF$Frag=="Train",], destination_frame="Train")
  # Model description
  Model_name <- paste0("GBM_Year(",X_start_year,
                       ")_SmpRt(",X_sample_rate,
                       ")_ColSmpRt(",X_col_sample_rate,
                       ")_LrnRt(",X_learn_rate,
                       ")_MxDpt(",X_max_depth,
                       ")_MinRw(",X_min_rows,
                       ")_X(",length(X_X),
                       ")_MinFgts(",X_MinFgts,
                       ")_Vld(",X_ValidSize,")")
  # If this model has already been fit then move on to the next one
  if(file.exists(file1=paste0(base.dir,"/Model metadata (temp)/",Model_name,".csv"))){ 
    setTxtProgressBar(pb=prog.bar,value=iter)
    next }
  # Fit the model 
  #  Note: Model fit within a TryCatch block, if we encounter an Error, its that the dataset is too small to enforce the specified minimum number of rows per split, so the 
  #        random hyper-parameter specification we selected is infeasible so break out of this iteration
  ERROR.flag <- FALSE
  tryCatch(
    { suppressWarnings(
        model <- h2o.gbm(
        x=X_X,y=y,training_frame=Train,validation_frame=Valid,model_id=Model_name,
        score_each_iteration=TRUE,         # Score the model every GBM tree/RF tree/NN epoch and save this data in model object
        stopping_metric="logloss",         # Stop running iterations based on the change in logloss (by default it uses validation_frame if it has one)
        stopping_rounds=5,                 # Instead of comparing logoss for each iteration, compare the 5-moving average logoss so we don't prematuraly stop
        stopping_tolerance=0,              # Moving average logloss must improve else stop the algorithm
        sample_rate=X_sample_rate,         # Each tree is trained on "sample_rate" % of the records
        col_sample_rate=X_col_sample_rate, # Each tree is trained on "col_sample_rate" % of the columns
        learn_rate=X_learn_rate,           # Learning rate
        ntrees=c(10000),                   # Number of trees (choose a large value since we're using early stoppage)
        max_depth=X_max_depth,             # Maximum depth of each tree (in terms of order of interactions)
        min_rows=X_min_rows) )             # Minimum number of records allowed for each tree's split
    },
    error=function(x){
      ERROR.flag <<- TRUE
    } )
  if(ERROR.flag){ next }
  # Save the model (save as local object so it doesn't print out file name to window)
  setwd(dir=paste0(base.dir,"/Model metadata (temp)"))
  metadata <- data.frame(model_name=Model_name,
                         model_type="GBM",
                         X_start_year=X_start_year,
                         X_sample_rate=X_sample_rate,
                         X_col_sample_rate=X_col_sample_rate,
                         X_learn_rate=X_learn_rate,
                         X_max_depth=X_max_depth,
                         X_min_rows=X_min_rows,
                         X_X=length(X_X),
                         X_ValidSize=X_ValidSize,
                         X_MinFgts=X_MinFgts,
                         TrainLL=h2o.logloss(object=model,train=TRUE,valid=FALSE),
                         ValidLL=h2o.logloss(object=model,train=FALSE,valid=TRUE))
  write.csv(x=metadata, file=paste0(Model_name,".csv"), row.names=FALSE)
  # Save the model (save as local object so it doesn't print out file name to window)
  temp <- h2o.saveModel(object=model, path=paste0(base.dir,"/H2O Models"))
  rm(list="temp")
  # Output Progress Bar
  setTxtProgressBar(pb=prog.bar,value=iter) }

# Clear the workspace
rm(list = ls()[ !(ls() %in% c(params.and.funcs,"AUF","y","X")) ])

#----------------------------------#
# Random Forest (RF) models models #
#----------------------------------#

# Read in the data
setwd(dir=paste0(base.dir,"Processed data"))
load(file="5a_DataPlusHelperVars.RDATA")

# Start h2o session and disable the progress bar - This computer has 64gb RAM
h2o.init(nthreads=(detectCores()-1), max_mem_size=RAM.to.use.H2O)
h2o.no_progress()

# Hyper-parameter lists
RF_Hyp <- list()
RF_Hyp[["Xlist_start_year"]] <- seq(1998,2012,1)
RF_Hyp[["Xlist_sample_rate"]] <- seq(0.50,1.0,0.05)
RF_Hyp[["Xlist_mtries"]] <- seq(0.40,0.95,0.05) # Set this as a proportion so it can be proportion of length(x1 or x2)
RF_Hyp[["Xlist_max_depth"]] <- seq(6,24,4)
RF_Hyp[["Xlist_min_rows"]] <- c(seq(10,100,10),seq(150,400,50))
RF_Hyp[["Xlist_X"]] <- X
RF_Hyp[["Xlist_ValidSize"]] <- seq(1000,9000,250)
RF_Hyp[["Xlist_MinFgts"]] <- seq(0,8,1)

# Random grid-search
prog.bar <- txtProgressBar(min=1,max=Rand.iters,style=3)
for(iter in 1:Rand.iters){
  # Initialize random hyper-parameters for this iteration
  X_start_year <- sample(x=RF_Hyp[["Xlist_start_year"]],size=1)
  X_sample_rate <- sample(x=RF_Hyp[["Xlist_sample_rate"]],size=1)
  X_X <- sample(x=RF_Hyp[["Xlist_X"]],size=1)[[1]]
  XX_mtries <- sample(x=RF_Hyp[["Xlist_mtries"]],size=1)
    X_mtries <- floor(XX_mtries*length(X_X))
  X_max_depth <- sample(x=RF_Hyp[["Xlist_max_depth"]],size=1)
  X_min_rows <- sample(x=RF_Hyp[["Xlist_min_rows"]],size=1)
  X_ValidSize <- sample(x=RF_Hyp[["Xlist_ValidSize"]],size=1)
  X_MinFgts <- sample(x=RF_Hyp[["Xlist_MinFgts"]],size=1)
  # Create the dataframes which will eventually be uploaded to H2O
  X_AUF <- AUF[AUF$Test==0,]
  N <- dim(X_AUF)[1]
  X_AUF$Frag <- NA
  X_AUF[(N-X_ValidSize+1):N,]$Frag <- "Valid"
  X_AUF[is.na(X_AUF$Frag),]$Frag <- "Train" # table(X_AUF$Frag,useNA="always")
  X_AUF <- X_AUF[lubridate::year(X_AUF$Date) >= X_start_year &
                   X_AUF$MinFgts >= X_MinFgts,]
  # If resulting training sample is small, or "negative" then skip this iteration
  if(sum(X_AUF$Frag=="Train")<=1000){ next }
  # Load data into h2o
  h2o.removeAll()
  Valid <- as.h2o(X_AUF[X_AUF$Frag=="Valid",], destination_frame="Valid")
  Train <- as.h2o(X_AUF[X_AUF$Frag=="Train",], destination_frame="Train")
  # Model description
  Model_name <- paste0("RF_Year(",X_start_year,
                       ")_SmpRt(",X_sample_rate,
                       ")_Mtry(",X_mtries,
                       ")_MxDpt(",X_max_depth,
                       ")_MinRw(",X_min_rows,
                       ")_X(",length(X_X),
                       ")_MinFgts(",X_MinFgts,
                       ")_Vld(",X_ValidSize,")")
  # If this model has already been fit then move on to the next one
  if(file.exists(file1=paste0(base.dir,"Model metadata (temp)/",Model_name,".csv"))){ 
    setTxtProgressBar(pb=prog.bar,value=iter)
    next }
  # Fit the model 
  #  Note: Model fit within a TryCatch block, if we encounter an Error, its that the dataset is too small to enforce the specified minimum number of rows per split, so the 
  #        random hyper-parameter specification we selected is infeasible so break out of this iteration
  ERROR.flag <- FALSE
  tryCatch(
    { suppressWarnings(
        model <- h2o.randomForest(
        x=X_X,y=y,training_frame=Train,validation_frame=Valid,model_id=Model_name,
        score_each_iteration=TRUE, # Score the model every GBM tree/RF tree/NN epoch and save this data in model object
        stopping_metric="logloss", # Stop running iterations based on the change in logloss (by default it uses validation_frame if it has one)
        stopping_rounds=5,         # Instead of comparing logoss for each iteration, compare the 5-moving average logoss so we don't prematuraly stop
        stopping_tolerance=0,      # Moving average logloss must improve else stop the algorithm
        sample_rate=X_sample_rate, # Each tree is trained on "sample_rate" % of the records
        mtries=X_mtries,           # Each tree is trained on a subset of "mtries" of the columns
        ntrees=c(10000),           # Number of trees (choose a large value since we're using early stoppage)
        max_depth=X_max_depth,     # Maximum depth of each tree (in terms of order of interactions)
        min_rows=X_min_rows) )     # Minimum number of records allowed for each tree's split
    },
    error=function(x){
      ERROR.flag <<- TRUE
    } )
  if(ERROR.flag){ next }
  # Save the model (save as local object so it doesn't print out file name to window)
  setwd(dir=paste0(base.dir,"Model metadata (temp)"))
  metadata <- data.frame(model_name=Model_name,
                         model_type="RF",
                         X_start_year=X_start_year,
                         X_sample_rate=X_sample_rate,
                         X_X=length(X_X),
                         X_mtries=X_mtries,
                         X_max_depth=X_max_depth,
                         X_min_rows=X_min_rows,
                         X_ValidSize=X_ValidSize,
                         X_MinFgts=X_MinFgts,
                         TrainLL=h2o.logloss(object=model,train=TRUE,valid=FALSE),
                         ValidLL=h2o.logloss(object=model,train=FALSE,valid=TRUE))
  write.csv(x=metadata, file=paste0(Model_name,".csv"), row.names=FALSE)
  # Save the model (save as local object so it doesn't print out file name to window)
  temp <- h2o.saveModel(object=model, path=paste0(base.dir,"H2O Models"))
  rm(list="temp")
  # Output Progress Bar
  setTxtProgressBar(pb=prog.bar,value=iter) }

# Clear the workspace
rm(list = ls()[ !(ls() %in% params.and.funcs) ])

#-----------------------------------#
# General Linear Model (GLM) models #
#-----------------------------------#

# Read in the data
setwd(dir=paste0(base.dir,"Processed data"))
load(file="5a_DataPlusHelperVars.RDATA")

# Start h2o session and disable the progress bar - This computer has 64gb RAM
h2o.init(nthreads=(detectCores()-1), max_mem_size=RAM.to.use.H2O)
h2o.no_progress()

# Hyper-parameter lists
GLM_Hyp <- list()
GLM_Hyp[["Xlist_start_year"]] <- seq(1998,2012,1)
GLM_Hyp[["Xlist_MaxActPred"]] <- seq(0.40,1.0,0.10) # Set this as a proportion so it can be proportion of length(x1 or x2)
GLM_Hyp[["Xlist_alpha"]] <- c(seq(0,0.09,0.01),seq(0.10,1,0.05))
GLM_Hyp[["Xlist_MissVal"]] <- c("MeanImputation")
GLM_Hyp[["Xlist_Stdize"]] <- c(TRUE,FALSE)
GLM_Hyp[["Xlist_X"]] <- X
GLM_Hyp[["Xlist_ValidSize"]] <- seq(1000,9000,250)
GLM_Hyp[["Xlist_MinFgts"]] <- seq(0,8,1)

# Random grid-search
prog.bar <- txtProgressBar(min=1,max=Rand.iters,style=3)
for(iter in 1:Rand.iters){
  # Initialize random hyper-parameters for this iteration
  X_start_year <- sample(x=GLM_Hyp[["Xlist_start_year"]],size=1)
  X_X <- sample(x=GLM_Hyp[["Xlist_X"]],size=1)[[1]]
  XX_MaxActPred <- sample(x=GLM_Hyp[["Xlist_MaxActPred"]],size=1)
    X_MaxActPred <- floor(XX_MaxActPred*length(X_X))
  X_alpha <- sample(x=GLM_Hyp[["Xlist_alpha"]],size=1)
  X_MissVal <- sample(x=GLM_Hyp[["Xlist_MissVal"]],size=1)
  X_Stdize <- sample(x=GLM_Hyp[["Xlist_Stdize"]],size=1)
  X_ValidSize <- sample(x=GLM_Hyp[["Xlist_ValidSize"]],size=1)
  X_MinFgts <- sample(x=GLM_Hyp[["Xlist_MinFgts"]],size=1)
  # Create the dataframes which will eventually be uploaded to H2O
  X_AUF <- AUF[AUF$Test==0,]
  N <- dim(X_AUF)[1]
  X_AUF$Frag <- NA
  X_AUF[(N-X_ValidSize+1):N,]$Frag <- "Valid"
  X_AUF[is.na(X_AUF$Frag),]$Frag <- "Train" # table(X_AUF$Frag,useNA="always")
  X_AUF <- X_AUF[lubridate::year(X_AUF$Date) >= X_start_year &
                   X_AUF$MinFgts >= X_MinFgts,]
  # If resulting training sample is small, or "negative" then skip this iteration
  if(sum(X_AUF$Frag=="Train")<=1000){ next }
  # Load data into h2o
  h2o.removeAll()
  Valid <- as.h2o(X_AUF[X_AUF$Frag=="Valid",], destination_frame="Valid")
  Train <- as.h2o(X_AUF[X_AUF$Frag=="Train",], destination_frame="Train")
  # Model description
  Model_name <- paste0("GLM_Year(",X_start_year,
                       ")_MaxActPred(",X_MaxActPred,
                       ")_Alph(",X_alpha,
                       ")_Miss(",substr(X_MissVal,1,4),
                       ")_Std(",substr(X_Stdize,1,1),
                       ")_X(",length(X_X),
                       ")_MinFgts(",X_MinFgts,
                       ")_Vld(",X_ValidSize,")")
  # If this model has already been fit then move on to the next one
  if(file.exists(file1=paste0(base.dir,"Model metadata (temp)/",Model_name,".csv"))){ 
    setTxtProgressBar(pb=prog.bar,value=iter)
    next }
  # Fit the model 
  #  Note: Model fit within a TryCatch block, if we encounter an Error, its that the dataset is too small to enforce the specified minimum number of rows per split, so the 
  #        random hyper-parameter specification we selected is infeasible so break out of this iteration
  ERROR.flag <- FALSE
  tryCatch(
    { suppressWarnings(
      model <- h2o.glm(
        x=X_X,y=y,training_frame=Train,validation_frame=Valid,model_id=Model_name,family="binomial",link="logit",
        standardize=X_Stdize,                # Standardize the X (since we are using L1/L2-regularization)
        lambda_search=TRUE,                  # Automatic h2o search for the optimal lambda parameter
        nlambdas=-1,                         # Set to default, tries 100 lambdas (from min-max lambda), alpha=0 then only tries 30
        max_active_predictors=X_MaxActPred,  # Maximum number of predictors to include in the model
        alpha=X_alpha,                       # Use alpha L1-regularization, and (1-alpha) L2-regularization (fitting elastic net)
        missing_values_handling=X_MissVal) ) # Either skip or mean impute missing values
    },
    error=function(x){
      ERROR.flag <<- TRUE
    } )
  if(ERROR.flag){ next }
  # Save the model (save as local object so it doesn't print out file name to window)
  setwd(dir=paste0(base.dir,"Model metadata (temp)"))
  metadata <- data.frame(model_name=Model_name,
                         model_type="GLM",
                         X_start_year=X_start_year,
                         X_X=length(X_X),
                         X_MaxActPred=X_MaxActPred,
                         X_alpha=X_alpha,
                         X_MissVal=X_MissVal,
                         X_Stdize=X_Stdize,
                         X_ValidSize=X_ValidSize,
                         X_MinFgts=X_MinFgts,
                         TrainLL=h2o.logloss(object=model,train=TRUE,valid=FALSE),
                         ValidLL=h2o.logloss(object=model,train=FALSE,valid=TRUE))
  write.csv(x=metadata, file=paste0(Model_name,".csv"), row.names=FALSE)
  # Save the model (save as local object so it doesn't print out file name to window)
  temp <- h2o.saveModel(object=model, path=paste0(base.dir,"H2O Models"))
  rm(list="temp")
  # Output Progress Bar
  setTxtProgressBar(pb=prog.bar,value=iter) }

# Clear the workspace
rm(list = ls()[ !(ls() %in% params.and.funcs) ])

#-------------------------------------#
# Naive Bayes Classifier (NBC) models #
#-------------------------------------#

# Read in the data
setwd(dir=paste0(base.dir,"Processed data"))
load(file="5a_DataPlusHelperVars.RDATA")

# Start h2o session and disable the progress bar - This computer has 64gb RAM
h2o.init(nthreads=(detectCores()-1), max_mem_size=RAM.to.use.H2O)
h2o.no_progress()

# Hyper-parameter lists
NB_Hyp <- list()
NB_Hyp[["Xlist_start_year"]] <- seq(1998,2012,1)
NB_Hyp[["Xlist_X"]] <- X
NB_Hyp[["Xlist_ValidSize"]] <- seq(1000,9000,250)
NB_Hyp[["Xlist_MinFgts"]] <- seq(0,8,1)

# Random grid-search
prog.bar <- txtProgressBar(min=1,max=Rand.iters,style=3)
for(iter in 1:Rand.iters){
  # Initialize random hyper-parameters for this iteration
  X_start_year <- sample(x=NB_Hyp[["Xlist_start_year"]],size=1)
  X_X <- sample(x=NB_Hyp[["Xlist_X"]],size=1)[[1]]
  X_ValidSize <- sample(x=NB_Hyp[["Xlist_ValidSize"]],size=1)
  X_MinFgts <- sample(x=NB_Hyp[["Xlist_MinFgts"]],size=1)
  # Create the dataframes which will eventually be uploaded to H2O
  X_AUF <- AUF[AUF$Test==0,]
  N <- dim(X_AUF)[1]
  X_AUF$Frag <- NA
  X_AUF[(N-X_ValidSize+1):N,]$Frag <- "Valid"
  X_AUF[is.na(X_AUF$Frag),]$Frag <- "Train" # table(X_AUF$Frag,useNA="always")
  X_AUF <- X_AUF[lubridate::year(X_AUF$Date) >= X_start_year &
                   X_AUF$MinFgts >= X_MinFgts,]
  # If resulting training sample is small, or "negative" then skip this iteration
  if(sum(X_AUF$Frag=="Train")<=1000){ next }
  # Load data into h2o
  h2o.removeAll()
  Valid <- as.h2o(X_AUF[X_AUF$Frag=="Valid",], destination_frame="Valid")
  Train <- as.h2o(X_AUF[X_AUF$Frag=="Train",], destination_frame="Train")
  # Model description
  Model_name <- paste0("NBC_Year(",X_start_year,
                       ")_X(",length(X_X),
                       ")_MinFgts(",X_MinFgts,
                       ")_Vld(",X_ValidSize,")")
  # If this model has already been fit then move on to the next one
  if(file.exists(file1=paste0(base.dir,"Model metadata (temp)/",Model_name,".csv"))){ 
    setTxtProgressBar(pb=prog.bar,value=iter)
    next }
  # Fit the model 
  #  Note: Model fit within a TryCatch block, if we encounter an Error, its that the dataset is too small to enforce the specified minimum number of rows per split, so the 
  #        random hyper-parameter specification we selected is infeasible so break out of this iteration
  ERROR.flag <- FALSE
  tryCatch(
    { suppressWarnings( model <- h2o.naiveBayes(x=X_X,y=y,training_frame=Train,validation_frame=Valid,model_id=Model_name) )
    },
    error=function(x){
      ERROR.flag <<- TRUE
    } )
  if(ERROR.flag){ next }
  # Save the model (save as local object so it doesn't print out file name to window)
  setwd(dir=paste0(base.dir,"Model metadata (temp)"))
  metadata <- data.frame(model_name=Model_name,
                         model_type="NBC",
                         X_start_year=X_start_year,
                         X_X=length(X_X),
                         X_ValidSize=X_ValidSize,
                         X_MinFgts=X_MinFgts,
                         TrainLL=h2o.logloss(object=model,train=TRUE,valid=FALSE),
                         ValidLL=h2o.logloss(object=model,train=FALSE,valid=TRUE))
  write.csv(x=metadata, file=paste0(Model_name,".csv"), row.names=FALSE)
  # Save the model (save as local object so it doesn't print out file name to window)
  temp <- h2o.saveModel(object=model, path=paste0(base.dir,"H2O Models"))
  rm(list="temp")
  # Output Progress Bar
  setTxtProgressBar(pb=prog.bar,value=iter) }

# Clear the workspace
rm(list = ls()[ !(ls() %in% params.and.funcs) ])

#----------------------------#
# Neural Network (NN) models #
#----------------------------#

# Read in the data
setwd(dir=paste0(base.dir,"Processed data"))
load(file="5a_DataPlusHelperVars.RDATA")

# Start h2o session and disable the progress bar - This computer has 64gb RAM
h2o.init(nthreads=(detectCores()-1), max_mem_size=RAM.to.use.H2O)
h2o.no_progress()

# Hyper-parameter lists
NN_Hyp <- list()
Xlist_HiddenLayers <- list()
for(layersX in seq(2,15,1)){
  for(nodesX in c(5,seq(10,100,10))){
    Xlist_HiddenLayers <- c( Xlist_HiddenLayers,list(rep(nodesX, layersX)) ) } }
NN_Hyp[["Xlist_HiddenLayers"]] <- Xlist_HiddenLayers
NN_Hyp[["Xlist_HiddenDropRt"]] <- seq(0,0.90,0.05)
NN_Hyp[["Xlist_start_year"]] <- seq(1998,2012,1)
NN_Hyp[["Xlist_input_dropout_ratio"]] <- seq(0,0.80,0.05)
NN_Hyp[["Xlist_MissVal"]] <- c("MeanImputation")
NN_Hyp[["Xlist_L1reg"]] <- seq(0,0.000200,0.000025)
NN_Hyp[["Xlist_L2reg"]] <- seq(0,0.000200,0.000025)
NN_Hyp[["Xlist_Activ"]] <- c("RectifierWithDropout","TanhWithDropout","MaxoutWithDropout")
NN_Hyp[["Xlist_Stdize"]] <- c(TRUE,FALSE)
NN_Hyp[["Xlist_X"]] <- X
NN_Hyp[["Xlist_ValidSize"]] <- seq(1000,9000,250)
NN_Hyp[["Xlist_MinFgts"]] <- seq(0,8,1)

# Random grid-search
prog.bar <- txtProgressBar(min=1,max=Rand.iters,style=3)
for(iter in 1:Rand.iters){
  # Initialize random hyper-parameters for this iteration
  X_HiddenLayers <- sample(x=NN_Hyp[["Xlist_HiddenLayers"]],size=1)[[1]]
  # If using dropout, use the dropout rate specified or close to it for early layers, but phase out the percent dropped for later layers which are closer to prediction. And
  # for the last layer set dropout = 0, since this is used directly before prediction
  X_HiddenDropRt <- sample(x=NN_Hyp[["Xlist_HiddenDropRt"]],size=1)
  X_HiddenDropRt <- X_HiddenDropRt*(1/2^(0:(length(X_HiddenLayers)-1)))
  X_HiddenDropRt[length(X_HiddenDropRt)] <- 0
  X_start_year <- sample(x=NN_Hyp[["Xlist_start_year"]],size=1)
  X_input_dropout_ratio <- sample(x=NN_Hyp[["Xlist_input_dropout_ratio"]],size=1)
  X_MissVal <- sample(x=NN_Hyp[["Xlist_MissVal"]],size=1)
  X_L1reg <- sample(x=NN_Hyp[["Xlist_L1reg"]],size=1)
  X_L2reg <- sample(x=NN_Hyp[["Xlist_L2reg"]],size=1)
  # NN models seem to do much better for Tanh activation function, so weight the random hyperparameter selection to choose Tanh more often
  X_Activ <- sample(x=NN_Hyp[["Xlist_Activ"]],prob=c(0.05,0.90,0.05),size=1)
  X_Stdize <- sample(x=NN_Hyp[["Xlist_Stdize"]],size=1)
  X_X <- sample(x=NN_Hyp[["Xlist_X"]],size=1)[[1]]
  X_ValidSize <- sample(x=NN_Hyp[["Xlist_ValidSize"]],size=1)
  X_MinFgts <- sample(x=NN_Hyp[["Xlist_MinFgts"]],size=1)
  # Create the dataframes which will eventually be uploaded to H2O
  X_AUF <- AUF[AUF$Test==0,]
  N <- dim(X_AUF)[1]
  X_AUF$Frag <- NA
  X_AUF[(N-X_ValidSize+1):N,]$Frag <- "Valid"
  X_AUF[is.na(X_AUF$Frag),]$Frag <- "Train" # table(X_AUF$Frag,useNA="always")
  X_AUF <- X_AUF[lubridate::year(X_AUF$Date) >= X_start_year &
                   X_AUF$MinFgts >= X_MinFgts,]
  # If resulting training sample is small, or "negative" then skip this iteration
  if(sum(X_AUF$Frag=="Train")<=1000){ next }
  # Load data into h2o
  h2o.removeAll()
  Valid <- as.h2o(X_AUF[X_AUF$Frag=="Valid",], destination_frame="Valid")
  Train <- as.h2o(X_AUF[X_AUF$Frag=="Train",], destination_frame="Train")
  # Model description
  Model_name <- paste0("NN_Y(",X_start_year,
                       ")_HL(",X_HiddenLayers[1],"x",length(X_HiddenLayers),
                       ")_HDR(",X_HiddenDropRt[1],
                       ")_IDR(",X_input_dropout_ratio,
                       ")_M(",substr(X_MissVal,1,4),
                       ")_L1(",X_L1reg,
                       ")_L2(",X_L2reg,
                       ")_A(",substr(X_Activ,1,3),
                       ")_Std(",substr(X_Stdize,1,1),
                       ")_X(",length(X_X),
                       ")_MinFgts(",X_MinFgts,
                       ")_V(",X_ValidSize,")")
  # If this model has already been fit then move on to the next one
  if(file.exists(file1=paste0(base.dir,"Model metadata (temp)/",Model_name,".csv"))){ 
    setTxtProgressBar(pb=prog.bar,value=iter)
    next }
  # Fit the model 
  #  Note: Model fit within a TryCatch block, if we encounter an Error, its that the dataset is too small to enforce the specified minimum number of rows per split, so the 
  #        random hyper-parameter specification we selected is infeasible so break out of this iteration
  ERROR.flag <- FALSE
  tryCatch(
    { suppressWarnings(
      model <- h2o.deeplearning(
        x=X_X,y=y,training_frame=Train,validation_frame=Valid,model_id=Model_name,
        variable_importances=TRUE, # Calculate information needed to compute variable importances
        score_each_iteration=TRUE, # Score the model every GBM tree/RF tree/NN epoch and save this data in model object
        stopping_metric="logloss", # Stop running iterations based on the change in logloss (by default it uses validation_frame if it has one)
        stopping_rounds=5,         # Instead of comparing logoss for each iteration, compare the 5-moving average logoss so we don't prematuraly stop
        stopping_tolerance=0,      # Moving average logloss must improve else stop the algorithm
        standardize=X_Stdize,      # Standardize the X (which improves gradient descent searching and needed since using L1/L2-regularization)
        hidden=X_HiddenLayers,                     # Number of hidden layers and nodes within them
        hidden_dropout_ratios=X_HiddenDropRt,      # Proportion of activation nodes to zero out for each hidden layer
        input_dropout_ratio=X_input_dropout_ratio, # Number of inputs to zero for each record
        missing_values_handling=X_MissVal,         # Either skip or mean impute missing values
        loss=c("CrossEntropy"),                    # Loss function to perform gradient descent over (Cross entropy = loss from MLE)
        adaptive_rate=TRUE,                        # Instead of simple gradient descent learn rate use adaptive learning, controlled by learning annealing (epsilon) and momentum (rho)
        rho=0.99,                                  # Apaptive learning rate momentum (related to the memory of prior weight updates) - default value used here
        epsilon=0.00000001,                        # Adaptive learning rate annealing - default value used here
        l1=X_L1reg,                                # L1-regularization
        l2=X_L2reg,                                # L2-regularization
        activation=X_Activ) )                      # Activation function (which all allow for specification of "hidden_dropout_ratios)
    },
    error=function(x){
      ERROR.flag <<- TRUE
    } )
  if(ERROR.flag){ next }
  # Save the model (save as local object so it doesn't print out file name to window)
  setwd(dir=paste0(base.dir,"Model metadata (temp)"))
  metadata <- data.frame(model_name=Model_name,
                         model_type="NN",
                         X_HiddenLayers=paste0(X_HiddenLayers[1],"x",length(X_HiddenLayers)),
                         X_HiddenDropRt=X_HiddenDropRt[1],
                         X_start_year=X_start_year,
                         X_input_dropout_ratio=X_input_dropout_ratio,
                         X_MissVal=X_MissVal,
                         X_L1reg=X_L1reg,
                         X_L2reg=X_L2reg,
                         X_Activ=X_Activ,
                         X_Stdize=X_Stdize,
                         X_X=length(X_X),
                         X_ValidSize=X_ValidSize,
                         X_MinFgts=X_MinFgts,
                         TrainLL=h2o.logloss(object=model,train=TRUE,valid=FALSE),
                         ValidLL=h2o.logloss(object=model,train=FALSE,valid=TRUE))
  write.csv(x=metadata, file=paste0(Model_name,".csv"), row.names=FALSE)
  # Save the model (save as local object so it doesn't print out file name to window)
  temp <- h2o.saveModel(object=model, path=paste0(base.dir,"H2O Models"))
  rm(list="temp")
  # Output Progress Bar
  setTxtProgressBar(pb=prog.bar,value=iter) }

# Clear the workspace
rm(list = ls()[ !(ls() %in% params.and.funcs) ])

#===================================================================#
# 5c - [Fit and use ML models for simulating the betting strategy]  #
#   Assess hyper-parameter choice                                   #
#===================================================================#

# Read in the data
setwd(dir=paste0(base.dir,"Processed data"))
load(file="5a_DataPlusHelperVars.RDATA")

# Read all the model metadata files
setwd(dir=paste0(base.dir,"Model metadata (temp)"))
all_files_in_dir <- list.files(path=paste0(base.dir,"Model metadata (temp)"))
prog.bar <- txtProgressBar(min=1,max=length(all_files_in_dir),style=3)

MM_GBM <- MM_GLM <- MM_RF <- MM_NBC <- MM_NN <- list()
for(file in all_files_in_dir){
  if(gregexpr(text=file, pattern="GBM_")[[1]][1]>=1){ MM_GBM[[length(MM_GBM)+1]] <- read.csv(file=file) }
  if(gregexpr(text=file, pattern="GLM_")[[1]][1]>=1){ MM_GLM[[length(MM_GLM)+1]] <- read.csv(file=file) }
  if(gregexpr(text=file, pattern="RF_" )[[1]][1]>=1){ MM_RF[[ length(MM_RF) +1]] <- read.csv(file=file) }
  if(gregexpr(text=file, pattern="NBC_")[[1]][1]>=1){ MM_NBC[[length(MM_NBC)+1]] <- read.csv(file=file) }
  if(gregexpr(text=file, pattern="NN_" )[[1]][1]>=1){ MM_NN[[ length(MM_NN) +1]] <- read.csv(file=file) }
  setTxtProgressBar(pb=prog.bar,value=which(all_files_in_dir==file)) }

MM_GBM <- list.stack(MM_GBM); MM_GBM <- MM_GBM[order(MM_GBM$ValidLL),]
MM_GLM <- list.stack(MM_GLM); MM_GLM <- MM_GLM[order(MM_GLM$ValidLL),]
MM_RF  <- list.stack(MM_RF);  MM_RF  <- MM_RF[ order(MM_RF$ValidLL) ,]
MM_NBC <- list.stack(MM_NBC); MM_NBC <- MM_NBC[order(MM_NBC$ValidLL),]
MM_NN  <- list.stack(MM_NN);  MM_NN  <- MM_NN[ order(MM_NN$ValidLL) ,]

# Look at the top models
top.x <- 1:5
MM_GBM[top.x,]
MM_GLM[top.x,]
MM_RF[top.x,]
MM_NBC[top.x,]
MM_NN[top.x,]

# Function to create lineplot by hyperparameter level
hyper_plots <- function(XXX_ModelObj=MM_NN,XXX_HypParam="X_Stdize",XXX_title="NN (Hyper-parameter - Standardization)",XXX_col="dodgerblue"){
  
  # Plot the averaged model results by hyper-parameter
  dir(path=paste0(base.dir,"HypParam Plots"))
  setwd(dir=paste0(base.dir,"HypParam Plots"))
  jpeg(file=paste0(XXX_title,".jpeg"),height=800,width=1400)
  
  # Hyper-parameter plots for numeric hyper-parameters
  if(!(XXX_HypParam %in% c("X_MissVal","X_Stdize","X_HiddenLayers","X_Activ"))){
    # Unique hyper-parameter values
    HypVals <- sort(unique(XXX_ModelObj[,XXX_HypParam]))
    # Average the model results
    averaged_results <- data.frame(HyperParam=HypVals,Mean=NA)
    for(HypVal in HypVals){ averaged_results[which(HypVals==HypVal),]$Mean <- mean(XXX_ModelObj[XXX_ModelObj[,XXX_HypParam]==HypVal,]$ValidLL) }
    if(XXX_HypParam %in% c("X_start_year","X_max_depth","X_MinFgts")){ 
      XXX_ModelObj$JittVar <- XXX_ModelObj[,XXX_HypParam]+rnorm(n=dim(XXX_ModelObj)[1],mean=0,sd=0.05) 
      xlim <- c(range(averaged_results$HyperParam)[1]-1, range(averaged_results$HyperParam)[2]+1) }
    if(XXX_HypParam %in% c("X_col_sample_rate","X_sample_rate","X_col_sample_rate","X_ValidSize","X_mtries","X_MaxActPred","X_alpha","X_HiddenDropRt","X_input_dropout_ratio")){ 
      XXX_ModelObj$JittVar <- XXX_ModelObj[,XXX_HypParam]+rnorm(n=dim(XXX_ModelObj)[1],mean=0,sd=0.002) 
      xlim <- c(range(averaged_results$HyperParam)[1]-0.05, range(averaged_results$HyperParam)[2]+0.05) }
    if(XXX_HypParam %in% c("X_learn_rate")){ 
      XXX_ModelObj$JittVar <- XXX_ModelObj[,XXX_HypParam]+rnorm(n=dim(XXX_ModelObj)[1],mean=0,sd=0.001) 
      xlim <- c(range(averaged_results$HyperParam)[1]-0.01, range(averaged_results$HyperParam)[2]+0.01) }
    if(XXX_HypParam %in% c("X_min_rows","X_min_rank","X_X","X_MinOrgFgts")){ 
      XXX_ModelObj$JittVar <- XXX_ModelObj[,XXX_HypParam]+rnorm(n=dim(XXX_ModelObj)[1],mean=0,sd=1) 
      xlim <- c(range(averaged_results$HyperParam)[1]-5, range(averaged_results$HyperParam)[2]+5) }
    if(XXX_HypParam %in% c("X_L1reg","X_L2reg")){
      XXX_ModelObj$JittVar <- XXX_ModelObj[,XXX_HypParam]+rnorm(n=dim(XXX_ModelObj)[1],mean=0,sd=0.000001) 
      xlim <- c(range(averaged_results$HyperParam)[1]-0.00001, range(averaged_results$HyperParam)[2]+0.00001) }
    ylim <- c(min(XXX_ModelObj$ValidLL),min(max(XXX_ModelObj$ValidLL),1.20*max(averaged_results$Mean)))
    ylab <- "Validation Logloss"
    xlab <- gsub(pattern="X_",replacement="",x=XXX_HypParam)
    plot(0~1,typ="n",main=XXX_title,ylab=ylab,xlab=xlab,xlim=xlim,ylim=ylim)
    if(XXX_col=="green3"){ rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col=rgb(0,1,0,0.15)) }
    if(XXX_col=="black"){ rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col=rgb(0,0,0,0.15)) }
    if(XXX_col=="red"){ rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col=rgb(1,0,0,0.15)) }
    if(XXX_col=="darkgoldenrod1"){ rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col=rgb(1,1,0,0.15)) }
    if(XXX_col=="dodgerblue"){ rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col=rgb(0,0,1,0.15)) }
    par(new=TRUE)
    plot(XXX_ModelObj$ValidLL~JittVar,data=XXX_ModelObj,type="p",main=XXX_title,ylab=ylab,xlab=xlab,xlim=xlim,ylim=ylim,col=XXX_col,lwd=5)
    par(new=TRUE)
    plot(Mean~HyperParam,data=averaged_results,type="b",main=XXX_title,ylab=ylab,xlab=xlab,xlim=xlim,ylim=ylim,cex=3,lwd=4,pch=24,col="grey53",bg=XXX_col)
    grid(nx=10,ny=10,col="grey45",lty=2)
    dev.off() }
  
  # Hyper-parameter plots for non-numeric hyper-parameters
  if(XXX_HypParam %in% c("X_MissVal","X_Stdize","X_HiddenLayers","X_Activ")){
    # Unique hyper-parameter values
    HypVals <- sort(unique(XXX_ModelObj[,XXX_HypParam]))
    # Average the model results
    averaged_results <- data.frame(HyperParam=HypVals,Mean=NA)
    for(HypVal in HypVals){ averaged_results[which(HypVals==HypVal),]$Mean <- mean(XXX_ModelObj[XXX_ModelObj[,XXX_HypParam]==HypVal,]$ValidLL) }
    averaged_results$Idx <- 1:dim(averaged_results)[1]
    XXX_ModelObj <- merge(x=XXX_ModelObj, by.x=XXX_HypParam, y=averaged_results[,c("HyperParam","Idx")], by.y="HyperParam")
    XXX_ModelObj$JittVar <- XXX_ModelObj$Idx+rnorm(n=dim(XXX_ModelObj)[1],mean=0,sd=0.02)
    xlim <- c(range(averaged_results$Idx)[1]-0.25, range(averaged_results$Idx)[2]+0.25)
    ylim <- c(min(XXX_ModelObj$ValidLL),min(max(XXX_ModelObj$ValidLL),1.20*max(averaged_results$Mean)))#range(XXX_ModelObj$ValidLL)
    ylab <- "Validation Logloss"
    xlab <- gsub(pattern="X_",replacement="",x=XXX_HypParam)
    plot(0~1,typ="n",main=XXX_title,ylab=ylab,xlab=xlab,xlim=xlim,xaxt="n",ylim=ylim)
    if(XXX_col=="green3"){ rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col=rgb(0,1,0,0.15)) }
    if(XXX_col=="black"){ rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col=rgb(0,0,0,0.15)) }
    if(XXX_col=="red"){ rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col=rgb(1,0,0,0.15)) }
    if(XXX_col=="darkgoldenrod1"){ rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col=rgb(1,1,0,0.15)) }
    if(XXX_col=="dodgerblue"){ rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col=rgb(0,0,1,0.15)) }
    par(new=TRUE)
    plot(XXX_ModelObj$ValidLL~JittVar,data=XXX_ModelObj,type="p",main=XXX_title,ylab=ylab,xlab=xlab,xlim=xlim,xaxt="n",ylim=ylim,col=XXX_col,lwd=5)
    par(new=TRUE)
    plot(Mean~Idx,data=averaged_results,type="b",main=XXX_title,ylab=ylab,xlab=xlab,xlim=xlim,xaxt="n",ylim=ylim,cex=3,lwd=4,pch=24,col="grey53",bg=XXX_col)
    axis(1,labels=as.character(averaged_results$HyperParam),at=c(1:dim(averaged_results)[1]))
    grid(nx=10,ny=10,col="grey45",lty=2)
    dev.off() } }

# GBM models
hyper_plots(XXX_ModelObj=MM_GBM, XXX_HypParam="X_start_year",      XXX_title="GBM (Hyper-parameter - Year)",                    XXX_col="black")
hyper_plots(XXX_ModelObj=MM_GBM, XXX_HypParam="X_sample_rate",     XXX_title="GBM (Hyper-parameter - Record Sample Rate)",      XXX_col="black")
hyper_plots(XXX_ModelObj=MM_GBM, XXX_HypParam="X_col_sample_rate", XXX_title="GBM (Hyper-parameter - Column Sample Rate)",      XXX_col="black")
hyper_plots(XXX_ModelObj=MM_GBM, XXX_HypParam="X_learn_rate",      XXX_title="GBM (Hyper-parameter - Learn Rate)",              XXX_col="black")
hyper_plots(XXX_ModelObj=MM_GBM, XXX_HypParam="X_max_depth",       XXX_title="GBM (Hyper-parameter - Max Tree Depth)",          XXX_col="black")
hyper_plots(XXX_ModelObj=MM_GBM, XXX_HypParam="X_min_rows",        XXX_title="GBM (Hyper-parameter - Min Rows in Tree Splits)", XXX_col="black")
hyper_plots(XXX_ModelObj=MM_GBM, XXX_HypParam="X_X",               XXX_title="GBM (Hyper-parameter - X Dimension)",             XXX_col="black")
hyper_plots(XXX_ModelObj=MM_GBM, XXX_HypParam="X_ValidSize",       XXX_title="GBM (Hyper-parameter - Test set size)",           XXX_col="black")
hyper_plots(XXX_ModelObj=MM_GBM, XXX_HypParam="X_MinFgts",         XXX_title="GBM (Hyper-parameter - min(F,O) number fights)",  XXX_col="black")
# RF models
hyper_plots(XXX_ModelObj=MM_RF, XXX_HypParam="X_start_year",  XXX_title="RF (Hyper-parameter - Year)",                    XXX_col="green3")
hyper_plots(XXX_ModelObj=MM_RF, XXX_HypParam="X_sample_rate", XXX_title="RF (Hyper-parameter - Record Sample Rate)",      XXX_col="green3")
hyper_plots(XXX_ModelObj=MM_RF, XXX_HypParam="X_mtries",      XXX_title="RF (Hyper-parameter - Column Sample Rate)",      XXX_col="green3")
hyper_plots(XXX_ModelObj=MM_RF, XXX_HypParam="X_max_depth",   XXX_title="RF (Hyper-parameter - Max Tree Depth)",          XXX_col="green3")
hyper_plots(XXX_ModelObj=MM_RF, XXX_HypParam="X_min_rows",    XXX_title="RF (Hyper-parameter - Min Rows in Tree Splits)", XXX_col="green3")
hyper_plots(XXX_ModelObj=MM_RF, XXX_HypParam="X_X",           XXX_title="RF (Hyper-parameter - X Dimension)",             XXX_col="green3")
hyper_plots(XXX_ModelObj=MM_RF, XXX_HypParam="X_ValidSize",   XXX_title="RF (Hyper-parameter - Test set size)",           XXX_col="green3")
hyper_plots(XXX_ModelObj=MM_RF, XXX_HypParam="X_MinFgts",     XXX_title="RF (Hyper-parameter - min(F,O) number fights)",  XXX_col="green3")
# GLM models
hyper_plots(XXX_ModelObj=MM_GLM, XXX_HypParam="X_start_year",  XXX_title="GLM (Hyper-parameter - Year)",                                 XXX_col="red")
hyper_plots(XXX_ModelObj=MM_GLM, XXX_HypParam="X_X",           XXX_title="GLM (Hyper-parameter - X Dimension)",                          XXX_col="red")
hyper_plots(XXX_ModelObj=MM_GLM, XXX_HypParam="X_MaxActPred",  XXX_title="GLM (Hyper-parameter - Proportion maximum active predictors)", XXX_col="red")
hyper_plots(XXX_ModelObj=MM_GLM, XXX_HypParam="X_alpha",       XXX_title="GLM (Hyper-parameter - Alpha)",                                XXX_col="red")
hyper_plots(XXX_ModelObj=MM_GLM, XXX_HypParam="X_MissVal",     XXX_title="GLM (Hyper-parameter - Missing handling)",                     XXX_col="red")
hyper_plots(XXX_ModelObj=MM_GLM, XXX_HypParam="X_Stdize",      XXX_title="GLM (Hyper-parameter - Standardization)",                      XXX_col="red")
hyper_plots(XXX_ModelObj=MM_GLM, XXX_HypParam="X_ValidSize",   XXX_title="GLM (Hyper-parameter - Test set size)",                        XXX_col="red")
hyper_plots(XXX_ModelObj=MM_GLM, XXX_HypParam="X_MinFgts",     XXX_title="GLM (Hyper-parameter - min(F,O) number fights)",               XXX_col="red")
# NBC models
hyper_plots(XXX_ModelObj=MM_NBC, XXX_HypParam="X_start_year",  XXX_title="NBC (Hyper-parameter - Year)",                   XXX_col="darkgoldenrod1")
hyper_plots(XXX_ModelObj=MM_NBC, XXX_HypParam="X_X",           XXX_title="NBC (Hyper-parameter - X Dimension)",            XXX_col="darkgoldenrod1")
hyper_plots(XXX_ModelObj=MM_NBC, XXX_HypParam="X_ValidSize",   XXX_title="NBC (Hyper-parameter - Test set size)",          XXX_col="darkgoldenrod1")
hyper_plots(XXX_ModelObj=MM_NBC, XXX_HypParam="X_MinFgts",     XXX_title="NBC (Hyper-parameter - min(F,O) number fights)", XXX_col="darkgoldenrod1")
# NN models
hyper_plots(XXX_ModelObj=MM_NN[MM_NN$X_Activ=="TanhWithDropout",], XXX_HypParam="X_start_year",          XXX_title="NN (Hyper-parameter - Year)",                      XXX_col="dodgerblue")
hyper_plots(XXX_ModelObj=MM_NN[MM_NN$X_Activ=="TanhWithDropout",], XXX_HypParam="X_X",                   XXX_title="NN (Hyper-parameter - X Dimension)",               XXX_col="dodgerblue")
hyper_plots(XXX_ModelObj=MM_NN[MM_NN$X_Activ=="TanhWithDropout",], XXX_HypParam="X_ValidSize",           XXX_title="NN (Hyper-parameter - Test set size)",             XXX_col="dodgerblue")
hyper_plots(XXX_ModelObj=MM_NN[MM_NN$X_Activ=="TanhWithDropout",], XXX_HypParam="X_HiddenLayers",        XXX_title="NN (Hyper-parameter - Topology)",                  XXX_col="dodgerblue")
hyper_plots(XXX_ModelObj=MM_NN[MM_NN$X_Activ=="TanhWithDropout",], XXX_HypParam="X_HiddenDropRt",        XXX_title="NN (Hyper-parameter - Hidden Layer Dropout rate)", XXX_col="dodgerblue")
hyper_plots(XXX_ModelObj=MM_NN[MM_NN$X_Activ=="TanhWithDropout",], XXX_HypParam="X_input_dropout_ratio", XXX_title="NN (Hyper-parameter - Input Layer Dropout rate)",  XXX_col="dodgerblue")
hyper_plots(XXX_ModelObj=MM_NN[MM_NN$X_Activ=="TanhWithDropout",], XXX_HypParam="X_L1reg",               XXX_title="NN (Hyper-parameter - L1 Regularization)",         XXX_col="dodgerblue")
hyper_plots(XXX_ModelObj=MM_NN[MM_NN$X_Activ=="TanhWithDropout",], XXX_HypParam="X_L2reg",               XXX_title="NN (Hyper-parameter - L2 Regularization)",         XXX_col="dodgerblue")
hyper_plots(XXX_ModelObj=MM_NN[MM_NN$X_Activ=="TanhWithDropout",], XXX_HypParam="X_Activ",               XXX_title="NN (Hyper-parameter - Activation function)",       XXX_col="dodgerblue")
hyper_plots(XXX_ModelObj=MM_NN[MM_NN$X_Activ=="TanhWithDropout",], XXX_HypParam="X_Stdize",              XXX_title="NN (Hyper-parameter - Standardization)",           XXX_col="dodgerblue")
hyper_plots(XXX_ModelObj=MM_NN[MM_NN$X_Activ=="TanhWithDropout",], XXX_HypParam="X_MissVal",             XXX_title="NN (Hyper-parameter - Missing handling)",          XXX_col="dodgerblue")
hyper_plots(XXX_ModelObj=MM_NN[MM_NN$X_Activ=="TanhWithDropout",], XXX_HypParam="X_MinFgts",             XXX_title="NN (Hyper-parameter - min(F,O) number fights)",    XXX_col="dodgerblue")

# Save the metadata dataframes as .RDATA and as concatenated excel files
setwd(dir=paste0(base.dir,"Processed data"))
save(MM_GBM,MM_GLM,MM_RF,MM_NBC,MM_NN,file="5c_ModelMetadata.RDATA")

dir(path=paste0(base.dir,"Model metadata"))
setwd(dir=paste0(base.dir,"Model metadata"))
write.csv(x=MM_GBM, file="ModelMetadata_GBM.csv", row.names=FALSE)
write.csv(x=MM_GLM, file="ModelMetadata_GLM.csv", row.names=FALSE)
write.csv(x=MM_RF,  file="ModelMetadata_RF.csv" , row.names=FALSE)
write.csv(x=MM_NBC, file="ModelMetadata_NBC.csv", row.names=FALSE)
write.csv(x=MM_NN,  file="ModelMetadata_NN.csv" , row.names=FALSE)

# Clear contents of the temporary directory
removeDirectory(path=paste0(base.dir,"Model metadata (temp)"), recursive=TRUE, mustExist=TRUE)

# Clear the workspace
rm(list = ls()[ !(ls() %in% params.and.funcs) ])

#===================================================================#
# 5d - [Fit ML models using random grid search]                     #
#   Create final Simulated Strategy plots for best/ensemble models  #
#===================================================================#

# Load data
setwd(dir=paste0(base.dir,"Processed data"))
load(file="5a_DataPlusHelperVars.RDATA")
load(file="5c_ModelMetaData.RDATA")

# Set up h2o so the best models can be used to predict the hold-out set
h2o.init(nthreads=(detectCores()-1), max_mem_size=RAM.to.use.H2O)
h2o.no_progress()
h2o.removeAll()

# Create the betting strategy simulation function for the best models and ensembles (similar to the previous one but slightly modified to accomodate
# ensembles, output results to different places,...etc.)
sim.strat <- function(XXX_Model=MM_GBM[1:5,],
                       XXX_EnsName="Top5_GBM"){
  # Create the dataframes which will eventually be uploaded to H2O
  #   Note that we don't need to filter by year anymore because we'll only be using the models to score the test dataset
  #   Note that we will filter based on the maximum MinFgts value used in the ensemble of models
  X_AUF <- AUF[AUF$Test==1,]
  # Load data into h2o
  h2o.removeAll()
  Test <- as.h2o(X_AUF, destination_frame="Test")
  # Append model predicted probabilities to this dataframe
  pred <- as.data.frame(Test)
  pred <- pred[,c("Fighter","Opponent","Date","F_odds","O_odds","Res","fight_key")]
  # Score the holdout dataframe using each of the specified models
  for(mod_name in XXX_Model$model_name){
    model <- h2o.loadModel(paste0(base.dir,"H2O Models/",mod_name))
    model_pred <- as.data.frame(h2o.predict(object=model,newdata=Test))[,c("p1")]
    pred <- data.frame(pred,model_pred)
    names(pred)[names(pred)=="model_pred"] <- paste0("Q",which(XXX_Model$model_name==mod_name))
    # Clear R workspace
    rm(list="model")
    # Clear the h2o workspace (but keep the data)
    h2o_keys <- as.vector(h2o.ls()$key)
    h2o.rm(ids=h2o_keys[!(h2o_keys %in% c("Test"))]) }
  # Combine the probabilities to create an ensemble based probability estimate
  prob_IDs <- c(1:length(XXX_Model$model_name))
  pred$p1 <- apply(X=as.data.frame(pred[,paste0("Q",prob_IDs)]),FUN=mean,MARGIN=1)
  # Drop individual model probabilities
  pred <- pred[,!(colnames(pred) %in% paste0("Q",prob_IDs))]
  # Collapse the 2 predictions for each fight | fgt_key=pred$fight_key[2]
  for(fgt_key in unique(pred$fight_key)){
    Y <- pred[pred$fight_key==fgt_key,]
    if(dim(Y)[1]==1){ next }
    a <- 0.50*Y[1,]$p1 + 0.50*(1-Y[2,]$p1)
    b <- 0.50*(1-Y[1,]$p1) + 0.50*Y[2,]$p1
    pred[pred$fight_key==fgt_key,]$p1 <- c(a,b) }
  # Filter out records with bad/missing odds data
  pred <- pred[!((pred$F_odds >= -99 & pred$F_odds<=99) | (pred$O_odds >= -99 & pred$O_odds<=99)),]
  pred <- pred[is.na(pred$F_odds)==FALSE & is.na(pred$O_odds)==FALSE,]
  # Only keep unique fights
  pred <- pred[!duplicated(pred$fight_key),]
  # Simulate the betting strategy
  pred$Res <- as.numeric(as.vector(pred$Res))
  bet <- 100
  pred$FbetFwin <- NA; pred$ObetOwin <- NA; pred$EgainF <- NA; pred$EgainO <- NA; pred$better_bet <- NA; pred$AgainF <- NA; pred$AgainO <- NA
  for(i in 1:dim(pred)[1]){ # i <- 463
    # Winnings given a bet on the fighter and the fighter wins
    if(pred[i,]$F_odds<0){ pred[i,]$FbetFwin <- bet*(100/(-1*pred[i,]$F_odds)) }
    if(pred[i,]$F_odds>0){ pred[i,]$FbetFwin <- bet*(pred[i,]$F_odds/100)  }
    # Winnings given a bet on the opponent and the opponent wins
    if(pred[i,]$O_odds<0){ pred[i,]$ObetOwin <- bet*(100/(-1*pred[i,]$O_odds)) }
    if(pred[i,]$O_odds>0){ pred[i,]$ObetOwin <- bet*(pred[i,]$O_odds/100)  }
    # Expected gain from a bet on the fighter
    pred[i,]$EgainF <- (pred[i,]$p1)*pred[i,]$FbetFwin + (1-pred[i,]$p1)*-bet
    # Expected gain from a bet on the opponent
    pred[i,]$EgainO <- (pred[i,]$p1)*-bet + (1-pred[i,]$p1)*pred[i,]$ObetOwin
    # Which is the better bet?
    if(pred[i,]$EgainF>=pred[i,]$EgainO){ pred[i,]$better_bet <- "F" }
    if(pred[i,]$EgainF< pred[i,]$EgainO){ pred[i,]$better_bet <- "O" }
    # Actual gain from a bet on the fighter
    pred[i,]$AgainF <- (pred[i,]$Res)*pred[i,]$FbetFwin + (1-pred[i,]$Res)*-bet
    pred[i,]$AgainO <- (1-pred[i,]$Res)*pred[i,]$ObetOwin + (pred[i,]$Res)*-bet }
  # Implement betting strategies
  thresholds <- seq(0,100,1)
  names(thresholds) <- thresholds
  for(thresh in thresholds){
    pred[,paste0("S",thresh)] <- 0
    pred[pred$better_bet=="F" & pred$EgainF>=thresh,paste0("S",thresh)] <- pred[pred$better_bet=="F" & pred$EgainF>=thresh,]$AgainF
    pred[pred$better_bet=="O" & pred$EgainO>=thresh,paste0("S",thresh)] <- pred[pred$better_bet=="O" & pred$EgainO>=thresh,]$AgainO }
  # Cumulative money lost/gained
  for(thresh in thresholds){ pred[,paste0("C",thresh)] <- cumsum(pred[,paste0("S",thresh)]) }
  # Moving averages of money lost/gained for each cutoff threshold
  CumSums <- unlist(pred[dim(pred)[1],paste0("C",thresholds)])
  MAs <- c(1,3,5,10,15,20)
  MA.list <- list()
  for(MA in MAs){
    temp.list <- NULL
    for(thresh in thresholds){
      MA.idx <- sort(as.numeric(names(sort(abs(thresh-thresholds))[1:MA])))
      temp.list <- c(temp.list,mean(CumSums[paste0("C",MA.idx)])) }
    MA.list[[which(MAs==MA)]] <- temp.list
    names(MA.list)[which(MAs==MA)] <- paste0("MA",MA)
    names(MA.list[[paste0("MA",MA)]]) <- thresholds  }
  # Plot the simulated strategies for this model
  opt.thresh <<- which(unlist(pred[dim(pred)[1],paste0("C",thresholds)])==max(unlist(pred[dim(pred)[1],grepl(pattern="C",x=names(pred))])))[1]-1
  opt.thresh.value <<- max(unlist(pred[dim(pred)[1],grepl(pattern="C",x=names(pred))]))
  M <- round(opt.thresh,digits=-1)
  thresholds.to.plot <- c(0,1,2,3,4,5,10,15,20,30,40,50,70,90,opt.thresh)
  thresholds.to.plot <- thresholds.to.plot[thresholds.to.plot>=0 & thresholds.to.plot<=100]
  ylim <- range(pred[,colnames(pred) %in% paste0("C",thresholds.to.plot)])
  if(ylim[2]<1000){ ylim[2] <- 1000 }
  if(ylim[1]>-1000){ ylim[1] <- -1000 }
  cex.lab <- 2; cex.main <- 2; cex.axis <- 2; lwd <- 3
  dir(path=paste0(base.dir,"Strategy graphics"))
  setwd(dir=paste0(base.dir,"Strategy graphics"))
  jpeg(file=paste0(XXX_EnsName,".jpeg"),height=1000,width=2000)
  par(mar=c(5,5,2,2),mfrow=c(1,2))
  plot(pred$C0~c(1:dim(pred)[1]),type="l",ylim=ylim,ylab="Simulated gain",xlab="Number of fights that have taken place so far in time",main="",xaxt="n",lwd=2,cex.lab=cex.lab,cex.main=cex.main,cex.axis=cex.axis)
  par(new=TRUE)
  rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "gray75"); par(new=TRUE)
  abline(v=seq(0,10000,50),lty=1,col="white",lwd=2); par(new=TRUE)
  abline(h=seq(ylim[1],ylim[2],(ylim[2]-ylim[1])/10),lty=1,lwd=2,col="white"); par(new=TRUE)
  abline(h=0,lty=1,col="black",lwd=4); par(new=TRUE)
  box(col="black"); par(new=TRUE)
  cols <- rainbow(length(thresholds.to.plot))
  for(thresh in thresholds.to.plot){
    # Overall Return on investment (ROI) = (investment final value - amount invested) / amount invested = Final balance change / amount invested
    ROI <- round(100*(pred[dim(pred)[1],paste0("C",thresh)] / (sum(pred[,paste0("S",thresh)]!=0)*bet)),2)
    plot(pred[,paste0("C",thresh)]~c(1:dim(pred)[1]),type="l",ylim=ylim,ylab="Simulated gain",xlab="Number of fights that have taken place so far in time",main="",lwd=lwd,cex.lab=cex.lab,cex.main=cex.main,cex.axis=cex.axis,col=cols[which(thresholds.to.plot==thresh)])
    par(new=TRUE) }
  # Overall Return on investment (ROI) = Final balance change / invested
  ROI <- NULL
  Net <- NULL
  for(thresh in thresholds.to.plot){
    ROI <- c(ROI,round(100*(pred[dim(pred)[1],paste0("C",thresh)] / (sum(pred[,paste0("S",thresh)]!=0)*bet)),2))
    Net <- c(Net,round(pred[dim(pred)[1],paste0("C",thresh)],2)) }
  best_strat_flag <- as.numeric(Net==max(Net))
  legend("bottomleft",paste0("[",best_strat_flag,"] S($",thresholds.to.plot,") ROI[",ROI,"%] - $",Net),col=cols,lty=1,lwd=8,cex=1.25,box.col=1,bg="white")
  # Plot the simulated strategies profile for  for this model
  ylim <- range(CumSums)
  if(ylim[2]<1000 ){ ylim[2] <- 1000 }
  if(ylim[1]>-1000){ ylim[1] <- -1000 }
  cex.lab <- 2; cex.main <- 2; cex.axis <- 2; lwd <- 3
  plot(CumSums~thresholds,type="l",ylim=ylim,ylab="Simulated gain",xlab="Decision Threshold ($)",main="",lwd=2,cex.lab=cex.lab,cex.main=cex.main,cex.axis=cex.axis)
  par(new=TRUE)
  rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "gray75"); par(new=TRUE)
  abline(v=seq(0,100,10),lty=1,col="white",lwd=2); par(new=TRUE)
  abline(h=seq(ylim[1],ylim[2],(ylim[2]-ylim[1])/10),lty=1,lwd=2,col="white"); par(new=TRUE)
  abline(h=0,lty=1,col="black",lwd=4); par(new=TRUE)
  cols <- rainbow(length(MAs))
  for(MA in MAs){
    plot(MA.list[[paste0("MA",MA)]]~as.numeric(names(MA.list[[paste0("MA",MA)]])),type="l",ylim=ylim,col=cols[which(MAs==MA)],ylab="Simulated gain",xlab="Decision Threshold ($)",main="",lwd=4,cex.lab=cex.lab,cex.main=cex.main,cex.axis=cex.axis)
    par(new=TRUE) }
  legend(x=65,y=1.025*ylim[2],paste0("MA",MAs," (Max=$",round(unlist(lapply(X=MA.list,FUN=max)),0),")"),col=cols,lty=1,lwd=8,cex=1.25,box.col=1,bg="white")
  dev.off() }

# Create Ensembles combining the top x models
EnsembleSizes <- c(1,3,5,10,25,50,100)
ModTypes <- c("Any","GLM","GBM","RF","NN")

# Create concatenated dataframe containing all model types
keep.vars <- c("model_type","model_name","ValidLL")
MM <- rbind(MM_GBM[, keep.vars],
            MM_GLM[, keep.vars],
            MM_NN[ , keep.vars],
            MM_RF[ , keep.vars])
MM <- MM[order(MM$ValidLL),]

# Create the Ensemble strategies
prog.bar <- txtProgressBar(min=1,max=length(EnsembleSizes)*length(ModTypes),style=3)
counter <- 0
for(x in EnsembleSizes){
  for(ModType in ModTypes){
    # Increment counter
    counter <- counter + 1
    # Select appropriate models
    if(ModType %in% c("GLM","GBM","RF","NN")){
      M <- MM[MM$model_type==ModType,]
      M <- M[1:x,] }
    if(ModType %in% c("Any")){
      M <- MM[1:x,] }
    # Create the strategy graphic
    sim.strat( XXX_Model=M, XXX_EnsName=paste0("Top",x,"_",ModType) )
    # Output Progress Bar
    setTxtProgressBar(pb=prog.bar,value=counter) } }

# Clear the workspace
rm(list = ls()[ !(ls() %in% params.and.funcs) ])