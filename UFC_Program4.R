#####################################################################################################
# Author:  Andrew Ghattas                                                                           #
# Program: UFC_Step4.R                                                                              #
# Purpose: Combine the fights, rankings, and odds data then feature engineer/process the dataframe  #
#####################################################################################################

#=========================================================#
# 4a - [Combine data sources and process final dataframe] #
#   Process raw web-scraped data and feature engineer     #
#=========================================================#

# Read in data
setwd(dir=paste0(base.dir,"Processed data"))
load(file="3d_All_UFC_Fights.RDATA")
AUF <- All_UFC_Fights
rm(list=ls()[ls() %in% "All_UFC_Fights"])

# Rename some variables
colnames(AUF)[colnames(AUF)=="Outcome"] <- "Result"
colnames(AUF)[colnames(AUF)=="Round"] <- "R"

# Lubridate the fight date
AUF$Date <- ymd(AUF$Date)

# Lubridate fighter DOB
AUF$DOB <- as.vector(AUF$DOB)
AUF[AUF$DOB=="--",]$DOB <- ""
AUF$DOB <- mdy(AUF$DOB)

# Fighter's age at time of fight
AUF$Age <- round(as.numeric(AUF$Date - AUF$DOB)/365,1)

# Change height as feet'inches" just to straight inches (and recode 0 feet to missing)
AUF$Height <- as.vector(AUF$Height)
AUF[AUF$Height=="--",]$Height <- ""
AUF$tmp <- gsub('"',"",gsub("'","-",AUF$Height))
AUF$Hgt <- as.numeric(substr(AUF$tmp,1,1))*12 + as.numeric(substr(AUF$tmp,unlist(gregexpr(pattern="-",AUF$tmp))+1,nchar(AUF$tmp)))
table(AUF$Height,useNA="always")
table(AUF$Hgt,useNA="always")
AUF <- select(AUF, -c(Height,tmp))

# Process Reach variable
table(AUF$Reach,useNA="always")
AUF$Rch <- as.vector(AUF$Reach)
AUF[AUF$Rch=="--",]$Rch <- NA
AUF$Rch <- as.numeric(AUF$Rch)
table(AUF$Reach,useNA="always")
table(AUF$Rch,useNA="always")
AUF <- select(AUF, -c(Reach))

# Process Weight variable
table(AUF$Weight,useNA="always")
AUF$Wt <- as.vector(AUF$Weight)
AUF[AUF$Wt=="--",]$Wt <- NA
AUF$Wt <- as.numeric(gsub(pattern=" lbs.",replacement="",x=AUF$Wt))
table(AUF$Weight,useNA="always")
table(AUF$Wt,useNA="always")
AUF <- select(AUF, -c(Weight))

# Create stance indicators
AUF$Stance <- as.vector(AUF$Stance)
table(AUF$Stance,useNA="always")
AUF$StSp <- AUF$StSw <- AUF$StOr <- NA

AUF[is.na(AUF$Stance)==FALSE & AUF$Stance=="Orthodox",]$StOr <- 1
AUF[is.na(AUF$Stance)==FALSE & (AUF$Stance=="Open Stance" | AUF$Stance=="Sideways" | AUF$Stance=="Southpaw" | AUF$Stance=="Switch"),]$StOr <- 0
table(AUF$StOr,useNA="always")

AUF[is.na(AUF$Stance)==FALSE & AUF$Stance=="Southpaw",]$StSp <- 1
AUF[is.na(AUF$Stance)==FALSE & (AUF$Stance=="Open Stance" | AUF$Stance=="Sideways" | AUF$Stance=="Orthodox" | AUF$Stance=="Switch"),]$StSp <- 0
table(AUF$StSp,useNA="always")

AUF[is.na(AUF$Stance)==FALSE & AUF$Stance=="Switch",]$StSw <- 1
AUF[is.na(AUF$Stance)==FALSE & (AUF$Stance=="Open Stance" | AUF$Stance=="Sideways" | AUF$Stance=="Southpaw" | AUF$Stance=="Orthodox"),]$StSw <- 0
table(AUF$StSw,useNA="always")

AUF <- select(AUF, -c(Stance))

# Create fight finish indicators
AUF$I_DEC <- 0; AUF$I_KOTKO <- 0; AUF$I_SUB <- 0
AUF$I_DEC <- as.numeric(AUF$method %in% c("Decision","Decision - Majority","Decision - Split","Decision - Unanimous"))
AUF$I_KOTKO <- as.numeric(AUF$method %in% c("KO/TKO","TKO - Doctor's Stoppage"))
AUF$I_SUB <- as.numeric(AUF$method %in% c("Submission"))

table(AUF$I_DEC,useNA="always")
table(AUF$I_KOTKO,useNA="always")
table(AUF$I_SUB,useNA="always")

AUF <- AUF[,!(names(AUF) %in% c("method"))]

# Adjust the variable which states the number of rounds the fight was scheduled for
#   - There can be a lot of different round/round time combinations due to evolving rules
#   - Basically, recode these so that they are either 3 round or 5 round fights
AUF$I5 <- 0
AUF[AUF$timeformat %in% c("4 Rnd (10-10-10-10)","5 Rnd (5-5-5-5-5)","5 Rnd (4-4-4-4-4)","5 Rnd (3-3-3-3-3)"),]$I5 <- 1
table(AUF$I5,useNA="always")
AUF <- AUF[,!(colnames(AUF) %in% c("timeformat"))]

# We could extract more information via text-processing for type of finish "details" but this is not straight-forward to know how to feature engineer this, and its use
# would probably be very limited given all the other information we've scraped, so skip this
AUF$details <- as.vector(AUF$details)
AUF <- AUF[,!(colnames(AUF) %in% c("details"))]

# Reorder columns
AUF <- AUF[,c("Fighter","Opponent","Date","Result","R","Time","I_DEC","I_KOTKO","I_SUB","I5","referee","Event","fight_page",
              
              "DOB","Age","Hgt","Rch","Wt","StOr","StSw","StSp","F_Rank","O_Rank","F_WC","O_WC","F_Gender","O_Gender","F_odds","O_odds",
              
              "KD","SS","TD","SubAtt","Ctrl","Rev",
              "RD1_SS","RD2_SS","RD3_SS","RD4_SS","RD5_SS","RD6_SS",
              "Head","Body","Leg","Distance","Clinch","Ground",
              
              "Opp_KD","Opp_SS","Opp_TD","Opp_SubAtt","Opp_Ctrl","Opp_Rev",
              "Opp_RD1_SS","Opp_RD2_SS","Opp_RD3_SS","Opp_RD4_SS","Opp_RD5_SS","Opp_RD6_SS",
              "Opp_Head","Opp_Body","Opp_Leg","Opp_Distance","Opp_Clinch","Opp_Ground")]

# Create variables (by parsing) for Knockdowns (KD), Significant strikes (SS) landed and thrown, Take-downs (TD) landed and thrown, Submission attempts (SubAtt),
# times passing guard (Pass), Reversals (Rev), head/body/leg/distance/clinch/ground strikes. Repeat for the opponent as well so that we can aggregate to get total
# stats of all opponents cumulatively combined vs fighter

# Fighter
AUF$KD <- AUF$KD

AUF$SS <- as.vector(AUF$SS)
AUF$SSL <- as.numeric(substr(AUF$SS,1,unlist(gregexpr(pattern=" of ",AUF$SS))-1))
AUF$SST <- as.numeric(substr(AUF$SS,unlist(gregexpr(pattern=" of ",AUF$SS))+4,nchar(AUF$SS)))

AUF$TD <- as.vector(AUF$TD)
AUF$TDL <- as.numeric(substr(AUF$TD,1,unlist(gregexpr(pattern=" of ",AUF$TD))-1))
AUF$TDT <- as.numeric(substr(AUF$TD,unlist(gregexpr(pattern=" of ",AUF$TD))+4,nchar(AUF$TD)))

AUF$SA <- AUF$SubAtt

AUF$Ctrl <- trimws(as.character(AUF$Ctrl))
AUF[AUF$Ctrl=="--" & is.na(AUF$Ctrl)==FALSE,]$Ctrl <- "0:00"

AUF$Rev <- AUF$Rev

AUF$Head <- as.vector(AUF$Head)
AUF$HeadL <- as.numeric(substr(AUF$Head,1,unlist(gregexpr(pattern=" of ",AUF$Head))-1))
AUF$HeadT <- as.numeric(substr(AUF$Head,unlist(gregexpr(pattern=" of ",AUF$Head))+4,nchar(AUF$Head)))

AUF$Body <- as.vector(AUF$Body)
AUF$BodyL <- as.numeric(substr(AUF$Body,1,unlist(gregexpr(pattern=" of ",AUF$Body))-1))
AUF$BodyT <- as.numeric(substr(AUF$Body,unlist(gregexpr(pattern=" of ",AUF$Body))+4,nchar(AUF$Body)))

AUF$Leg <- as.vector(AUF$Leg)
AUF$LegL <- as.numeric(substr(AUF$Leg,1,unlist(gregexpr(pattern=" of ",AUF$Leg))-1))
AUF$LegT <- as.numeric(substr(AUF$Leg,unlist(gregexpr(pattern=" of ",AUF$Leg))+4,nchar(AUF$Leg)))

AUF$Distance <- as.vector(AUF$Distance)
AUF$DistL <- as.numeric(substr(AUF$Distance,1,unlist(gregexpr(pattern=" of ",AUF$Distance))-1))
AUF$DistT <- as.numeric(substr(AUF$Distance,unlist(gregexpr(pattern=" of ",AUF$Distance))+4,nchar(AUF$Distance)))

AUF$Clinch <- as.vector(AUF$Clinch)
AUF$ClinchL <- as.numeric(substr(AUF$Clinch,1,unlist(gregexpr(pattern=" of ",AUF$Clinch))-1))
AUF$ClinchT <- as.numeric(substr(AUF$Clinch,unlist(gregexpr(pattern=" of ",AUF$Clinch))+4,nchar(AUF$Clinch)))

AUF$Ground <- as.vector(AUF$Ground)
AUF$GrdL <- as.numeric(substr(AUF$Ground,1,unlist(gregexpr(pattern=" of ",AUF$Ground))-1))
AUF$GrdT <- as.numeric(substr(AUF$Ground,unlist(gregexpr(pattern=" of ",AUF$Ground))+4,nchar(AUF$Ground)))

AUF$RD1_SS <- as.vector(AUF$RD1_SS)
AUF$RD2_SS <- as.vector(AUF$RD2_SS)
AUF$RD3_SS <- as.vector(AUF$RD3_SS)
AUF$RD4_SS <- as.vector(AUF$RD4_SS)
AUF$RD5_SS <- as.vector(AUF$RD5_SS)
AUF$RD6_SS <- as.vector(AUF$RD6_SS)
AUF$RD1SSL <- as.numeric(substr(AUF$RD1_SS,1,unlist(gregexpr(pattern=" of ",AUF$RD1_SS))-1))
AUF$RD2SSL <- as.numeric(substr(AUF$RD2_SS,1,unlist(gregexpr(pattern=" of ",AUF$RD2_SS))-1))
AUF$RD3SSL <- as.numeric(substr(AUF$RD3_SS,1,unlist(gregexpr(pattern=" of ",AUF$RD3_SS))-1))
AUF$RD4SSL <- as.numeric(substr(AUF$RD4_SS,1,unlist(gregexpr(pattern=" of ",AUF$RD4_SS))-1))
AUF$RD5SSL <- as.numeric(substr(AUF$RD5_SS,1,unlist(gregexpr(pattern=" of ",AUF$RD5_SS))-1))
AUF$RD6SSL <- as.numeric(substr(AUF$RD6_SS,1,unlist(gregexpr(pattern=" of ",AUF$RD6_SS))-1))

# Fighter's opponents
AUF$Opp_KD <- AUF$Opp_KD

AUF$Opp_SS <- as.vector(AUF$Opp_SS)
AUF$Opp_SSL <- as.numeric(substr(AUF$Opp_SS,1,unlist(gregexpr(pattern=" of ",AUF$Opp_SS))-1))
AUF$Opp_SST <- as.numeric(substr(AUF$Opp_SS,unlist(gregexpr(pattern=" of ",AUF$Opp_SS))+4,nchar(AUF$Opp_SS)))

AUF$Opp_TD <- as.vector(AUF$Opp_TD)
AUF$Opp_TDL <- as.numeric(substr(AUF$Opp_TD,1,unlist(gregexpr(pattern=" of ",AUF$Opp_TD))-1))
AUF$Opp_TDT <- as.numeric(substr(AUF$Opp_TD,unlist(gregexpr(pattern=" of ",AUF$Opp_TD))+4,nchar(AUF$Opp_TD)))

AUF$Opp_SA <- AUF$Opp_SubAtt

AUF$Opp_Ctrl <- trimws(as.character(AUF$Opp_Ctrl))
AUF[AUF$Opp_Ctrl=="--" & is.na(AUF$Opp_Ctrl)==FALSE,]$Opp_Ctrl <- "0:00"

AUF$Opp_Rev <- AUF$Opp_Rev

AUF$Opp_Head <- as.vector(AUF$Opp_Head)
AUF$Opp_HeadL <- as.numeric(substr(AUF$Opp_Head,1,unlist(gregexpr(pattern=" of ",AUF$Opp_Head))-1))
AUF$Opp_HeadT <- as.numeric(substr(AUF$Opp_Head,unlist(gregexpr(pattern=" of ",AUF$Opp_Head))+4,nchar(AUF$Opp_Head)))

AUF$Opp_Body <- as.vector(AUF$Opp_Body)
AUF$Opp_BodyL <- as.numeric(substr(AUF$Opp_Body,1,unlist(gregexpr(pattern=" of ",AUF$Opp_Body))-1))
AUF$Opp_BodyT <- as.numeric(substr(AUF$Opp_Body,unlist(gregexpr(pattern=" of ",AUF$Opp_Body))+4,nchar(AUF$Opp_Body)))

AUF$Opp_Leg <- as.vector(AUF$Opp_Leg)
AUF$Opp_LegL <- as.numeric(substr(AUF$Opp_Leg,1,unlist(gregexpr(pattern=" of ",AUF$Opp_Leg))-1))
AUF$Opp_LegT <- as.numeric(substr(AUF$Opp_Leg,unlist(gregexpr(pattern=" of ",AUF$Opp_Leg))+4,nchar(AUF$Opp_Leg)))

AUF$Opp_Distance <- as.vector(AUF$Opp_Distance)
AUF$Opp_DistL <- as.numeric(substr(AUF$Opp_Distance,1,unlist(gregexpr(pattern=" of ",AUF$Opp_Distance))-1))
AUF$Opp_DistT <- as.numeric(substr(AUF$Opp_Distance,unlist(gregexpr(pattern=" of ",AUF$Opp_Distance))+4,nchar(AUF$Opp_Distance)))

AUF$Opp_Clinch <- as.vector(AUF$Opp_Clinch)
AUF$Opp_ClinchL <- as.numeric(substr(AUF$Opp_Clinch,1,unlist(gregexpr(pattern=" of ",AUF$Opp_Clinch))-1))
AUF$Opp_ClinchT <- as.numeric(substr(AUF$Opp_Clinch,unlist(gregexpr(pattern=" of ",AUF$Opp_Clinch))+4,nchar(AUF$Opp_Clinch)))

AUF$Opp_Ground <- as.vector(AUF$Opp_Ground)
AUF$Opp_GrdL <- as.numeric(substr(AUF$Opp_Ground,1,unlist(gregexpr(pattern=" of ",AUF$Opp_Ground))-1))
AUF$Opp_GrdT <- as.numeric(substr(AUF$Opp_Ground,unlist(gregexpr(pattern=" of ",AUF$Opp_Ground))+4,nchar(AUF$Opp_Ground)))

AUF$Opp_RD1_SS <- as.vector(AUF$Opp_RD1_SS)
AUF$Opp_RD2_SS <- as.vector(AUF$Opp_RD2_SS)
AUF$Opp_RD3_SS <- as.vector(AUF$Opp_RD3_SS)
AUF$Opp_RD4_SS <- as.vector(AUF$Opp_RD4_SS)
AUF$Opp_RD5_SS <- as.vector(AUF$Opp_RD5_SS)
AUF$Opp_RD6_SS <- as.vector(AUF$Opp_RD6_SS)
AUF$Opp_RD1SSL <- as.numeric(substr(AUF$Opp_RD1_SS,1,unlist(gregexpr(pattern=" of ",AUF$Opp_RD1_SS))-1))
AUF$Opp_RD2SSL <- as.numeric(substr(AUF$Opp_RD2_SS,1,unlist(gregexpr(pattern=" of ",AUF$Opp_RD2_SS))-1))
AUF$Opp_RD3SSL <- as.numeric(substr(AUF$Opp_RD3_SS,1,unlist(gregexpr(pattern=" of ",AUF$Opp_RD3_SS))-1))
AUF$Opp_RD4SSL <- as.numeric(substr(AUF$Opp_RD4_SS,1,unlist(gregexpr(pattern=" of ",AUF$Opp_RD4_SS))-1))
AUF$Opp_RD5SSL <- as.numeric(substr(AUF$Opp_RD5_SS,1,unlist(gregexpr(pattern=" of ",AUF$Opp_RD5_SS))-1))
AUF$Opp_RD6SSL <- as.numeric(substr(AUF$Opp_RD6_SS,1,unlist(gregexpr(pattern=" of ",AUF$Opp_RD6_SS))-1))

# If control time is listed as NA then set it as 0:00
AUF[is.na(AUF$Ctrl),]$Ctrl <- "0:00"
AUF[is.na(AUF$Opp_Ctrl),]$Opp_Ctrl <- "0:00"

# Drop some variables
AUF <- AUF[,!(colnames(AUF) %in% c("referee","Event","fight_page",
                                
                                   "SS","TD","SubAtt",
                                   "Head","Body","Leg","Distance","Clinch","Ground",
                                   "RD1_SS","RD2_SS","RD3_SS","RD4_SS","RD5_SS","RD6_SS",
                                   
                                   "Opp_SS","Opp_TD","Opp_SubAtt",
                                   "Opp_Head","Opp_Body","Opp_Leg","Opp_Distance","Opp_Clinch","Opp_Ground",
                                   "Opp_RD1_SS","Opp_RD2_SS","Opp_RD3_SS","Opp_RD4_SS","Opp_RD5_SS","Opp_RD6_SS"))]

# Order columns
AUF <- AUF[,c("Fighter","Opponent","Date","Result","R","Time","I_DEC","I_KOTKO","I_SUB","I5",
              "F_WC","O_WC","F_Gender","O_Gender","F_odds","O_odds","F_Rank","O_Rank",
              
              "DOB","Age","Hgt","Rch","Wt","StOr","StSw","StSp",
              
              "KD","Ctrl","Rev","SSL","SST","TDL","TDT","SA",
              "HeadL","HeadT","BodyL","BodyT","LegL","LegT","DistL","DistT","ClinchL","ClinchT","GrdL","GrdT",
              "RD1SSL","RD2SSL","RD3SSL","RD4SSL","RD5SSL","RD6SSL",
              
              "Opp_KD","Opp_Ctrl","Opp_Rev","Opp_SSL","Opp_SST","Opp_TDL","Opp_TDT","Opp_SA",
              "Opp_HeadL","Opp_HeadT","Opp_BodyL","Opp_BodyT","Opp_LegL","Opp_LegT","Opp_DistL","Opp_DistT","Opp_ClinchL","Opp_ClinchT","Opp_GrdL","Opp_GrdT",
              "Opp_RD1SSL","Opp_RD2SSL","Opp_RD3SSL","Opp_RD4SSL","Opp_RD5SSL","Opp_RD6SSL")]

# Store final fighter results in this directory
dir(path=paste0(base.dir,"Processed data/temp"))
setwd(dir=paste0(base.dir,"Processed data/temp"))

# All unique fighters in the "All_UFC_Fights" dataset
all_fighters <- unique(as.vector(AUF$Fighter))

# Progress bar
prog.bar <- txtProgressBar(min=1,max=length(all_fighters),style=3)

for(fighter in all_fighters){ # fighter<-"DANIEL CORMIER"; fighter="ANTHONY SMITH"; fighter="GEGARD MOUSASI"
  
  # Fighter specific data
  X <- as.data.frame(AUF %>% filter(Fighter==fighter)) #  %>% map_df(rev)
  X <- X[X$Result %in% c("win","loss"),]
  X <- X[order(X$Date),]
  
  # Skip this fighter if they have no non-NC/draw fights
  if(dim(X)[1]==0){ next }
  
  #-----------------------------------------------------------------------------------#
  # Initialize predictors: Cumulative counts of fight stats prior to the given fight  #
  #-----------------------------------------------------------------------------------#
  
  # Highest rank Fighter has fought the last 3 fights
  X$HghRnkFgtLst3 <- 250
  X$HghRnkWinLst3 <- 250
  
  # Total number of <fights,wins,loses> aggregated <total,top5rank,top10rank,top20rank,top50rank,top100rank>
  X$F <- NA;    X$W <- NA;    X$L <- NA
  X$F5 <- NA;   X$W5 <- NA;   X$L5 <- NA
  X$F10 <- NA;  X$W10 <- NA;  X$L10 <- NA
  X$F20 <- NA;  X$W20 <- NA;  X$L20 <- NA
  X$F50 <- NA;  X$W50 <- NA;  X$L50 <- NA
  X$F100 <- NA; X$W100 <- NA; X$L100 <- NA
  
  # Over previous 10 fights the number of <KOTKO,SUB,DEC> <wins,losses> aggregated <total,top5rank,top10rank,top20rank,top50rank,top100rank>
  X$KOTKO_W <- NA; X$SUB_W <- NA; X$DEC_W <- NA; X$KOTKO_L <- NA; X$SUB_L <- NA; X$DEC_L <- NA
  X$KOTKO_W5 <- NA;   X$SUB_W5 <- NA;   X$DEC_W5 <- NA;   X$KOTKO_L5 <- NA;   X$SUB_L5 <- NA;   X$DEC_L5 <- NA
  X$KOTKO_W10 <- NA;  X$SUB_W10 <- NA;  X$DEC_W10 <- NA;  X$KOTKO_L10 <- NA;  X$SUB_L10 <- NA;  X$DEC_L10 <- NA
  X$KOTKO_W20 <- NA;  X$SUB_W20 <- NA;  X$DEC_W20 <- NA;  X$KOTKO_L20 <- NA;  X$SUB_L20 <- NA;  X$DEC_L20 <- NA
  X$KOTKO_W50 <- NA;  X$SUB_W50 <- NA;  X$DEC_W50 <- NA;  X$KOTKO_L50 <- NA;  X$SUB_L50 <- NA;  X$DEC_L50 <- NA
  X$KOTKO_W100 <- NA; X$SUB_W100 <- NA; X$DEC_W100 <- NA; X$KOTKO_L100 <- NA; X$SUB_L100 <- NA; X$DEC_L100 <- NA
  
  # Cumulative number of days since last <fight,KOTKO loss> at time of fight
  X$TS_F <- NA; X$TS_KOTKOl <- NA
  
  # Was their 3+ winning streak broken last fight (excluding no contests/draws)
  X$WinB3 <- NA
  
  # Number of wins the previous <3,5,7,9,11,13,15> fights (excluding no contests/draws)
  X$WinsP3 <- NA; X$WinsP5 <- NA; X$WinsP7 <- NA; X$WinsP9 <- NA; X$WinsP11 <- NA; X$WinsP13 <- NA; X$WinsP15 <- NA
  
  # Fighter total cumulative fighting time (in minutes)
  X$Fight_Time <- 0
  
  # Percentage of total time actions
  X$CtrlSum <- X$CtrlPerc <- 0;
  X$Opp_CtrlSum <- X$Opp_CtrlPerc <- 0;
  
  # Number of actions per 5 minutes
  X$KD_p5m <- 0; X$Rev_p5m <- 0; X$SA_p5m <- 0
  X$SSL_p5m <- 0; X$SST_p5m <- 0; X$SSAcc <- 0
  X$TDL_p5m <- 0; X$TDT_p5m <- 0; X$TDAcc <- 0
  X$HeadL_p5m <- 0; X$BodyL_p5m <- 0; X$LegL_p5m <- 0; X$DistL_p5m <- 0; X$ClinchL_p5m <- 0; X$GrdL_p5m <- 0
  X$SSL_Beta <- NA
  
  X$Opp_KD_p5m <- 0; X$Opp_Rev_p5m <- 0; X$Opp_SA_p5m <- 0
  X$Opp_SSL_p5m <- 0; X$Opp_SST_p5m <- 0; X$Opp_SSAcc <- 0
  X$Opp_TDL_p5m <- 0; X$Opp_TDT_p5m <- 0; X$Opp_TDAcc <- 0
  X$Opp_HeadL_p5m <- 0; X$Opp_BodyL_p5m <- 0; X$Opp_LegL_p5m <- 0; X$Opp_DistL_p5m <- 0; X$Opp_ClinchL_p5m <- 0; X$Opp_GrdL_p5m <- 0
  X$Opp_SSL_Beta <- NA
  
  for(i in 1:dim(X)[1]){
    
    # If this is the fighters first fight then initialize all values to 0 (except for "HghRnkFgtLst3" and "HghRnkWinLst3" which will stay as default value 250)
    if(i==1){ 
      X[i,]$F <-    X[i,]$W <-    X[i,]$L <- 0
      X[i,]$F5 <-   X[i,]$W5 <-   X[i,]$L5 <- 0
      X[i,]$F10 <-  X[i,]$W10 <-  X[i,]$L10 <- 0
      X[i,]$F20 <-  X[i,]$W20 <-  X[i,]$L20 <- 0
      X[i,]$F50 <-  X[i,]$W50 <-  X[i,]$L50 <- 0
      X[i,]$F100 <- X[i,]$W100 <- X[i,]$L100 <- 0
      X[i,]$KOTKO_W <-    X[i,]$SUB_W <-    X[i,]$DEC_W <-    X[i,]$KOTKO_L <-    X[i,]$SUB_L <-    X[i,]$DEC_L <- 0
      X[i,]$KOTKO_W5 <-   X[i,]$SUB_W5 <-   X[i,]$DEC_W5 <-   X[i,]$KOTKO_L5 <-   X[i,]$SUB_L5 <-   X[i,]$DEC_L5 <- 0
      X[i,]$KOTKO_W10 <-  X[i,]$SUB_W10 <-  X[i,]$DEC_W10 <-  X[i,]$KOTKO_L10 <-  X[i,]$SUB_L10 <-  X[i,]$DEC_L10 <- 0
      X[i,]$KOTKO_W20 <-  X[i,]$SUB_W20 <-  X[i,]$DEC_W20 <-  X[i,]$KOTKO_L20 <-  X[i,]$SUB_L20 <-  X[i,]$DEC_L20 <- 0
      X[i,]$KOTKO_W50 <-  X[i,]$SUB_W50 <-  X[i,]$DEC_W50 <-  X[i,]$KOTKO_L50 <-  X[i,]$SUB_L50 <-  X[i,]$DEC_L50 <- 0
      X[i,]$KOTKO_W100 <- X[i,]$SUB_W100 <- X[i,]$DEC_W100 <- X[i,]$KOTKO_L100 <- X[i,]$SUB_L100 <- X[i,]$DEC_L100 <- 0
      X[i,]$TS_F <- X[i,]$TS_KOTKOl <- 365 # Arbitrary default value for fighter's first fight
      X[i,]$WinB3 <- 0
      X[i,]$WinsP3 <- X[i,]$WinsP5 <- X[i,]$WinsP7 <- X[i,]$WinsP9 <- X[i,]$WinsP11 <- X[i,]$WinsP13 <- X[i,]$WinsP15 <- 0
      X[i,]$Fight_Time <- 0
      X[i,]$CtrlSum <- X[i,]$CtrlPerc <- 0
      X[i,]$KD_p5m <- X[i,]$Rev_p5m <- X[i,]$SA_p5m <- 0
      X[i,]$SSL_p5m <- X[i,]$SST_p5m <- X[i,]$SSAcc <- 0
      X[i,]$TDL_p5m <- X[i,]$TDT_p5m <- X[i,]$TDAcc <- 0
      X[i,]$HeadL_p5m <- X[i,]$BodyL_p5m <- X[i,]$LegL_p5m <- X[i,]$DistL_p5m <- X[i,]$ClinchL_p5m <- X[i,]$GrdL_p5m <- 0
      X[i,]$SSL_Beta <- 0
      X[i,]$Opp_CtrlSum <- X[i,]$Opp_CtrlPerc <- 0
      X[i,]$Opp_KD_p5m <- X[i,]$Opp_Rev_p5m <- X[i,]$Opp_SA_p5m <- 0
      X[i,]$Opp_SSL_p5m <- X[i,]$Opp_SST_p5m <- X[i,]$Opp_SSAcc <- 0
      X[i,]$Opp_TDL_p5m <- X[i,]$Opp_TDT_p5m <- X[i,]$Opp_TDAcc <- 0
      X[i,]$Opp_HeadL_p5m <- X[i,]$Opp_BodyL_p5m <- X[i,]$Opp_LegL_p5m <- X[i,]$Opp_DistL_p5m <- X[i,]$Opp_ClinchL_p5m <- X[i,]$Opp_GrdL_p5m <- 0
      X[i,]$Opp_SSL_Beta <- 0
      next }
    
    ####### Highest rank Fighter has fought the last 3 fights
    
    # Fighter's previous fights
    prev_fighter_fights <- X[1:(i-1),]
    
    # Extract the highest rank fought in previous 3 fights at this point in time
    if(dim(prev_fighter_fights)[1]<=3){ prev_3_fights <- prev_fighter_fights }
    if(dim(prev_fighter_fights)[1]>=4){ prev_3_fights <- prev_fighter_fights[(dim(prev_fighter_fights)[1]-2):dim(prev_fighter_fights)[1],] }
    X[i,]$HghRnkFgtLst3 <- min(prev_3_fights$O_Rank)
    
    # Extract the highest rank beaten in previous 3 fights at this point in time
    prev_3_fights_win <- prev_3_fights[prev_3_fights$Result=="win",]
    if(dim(prev_3_fights_win)[1]==0){ X[i,]$HghRnkWinLst3 <- 250 }
    if(dim(prev_3_fights_win)[1]>=1){ X[i,]$HghRnkWinLst3 <- min(prev_3_fights_win$O_Rank) }
    
    ####### Total number of fights/wins/losses total
    prev_fights <- X[1:(i-1),]
    X[i,]$F <- dim(prev_fights)[1]
    X[i,]$W <- dim(prev_fights[prev_fights$Result=="win",])[1]
    X[i,]$L <- dim(prev_fights[prev_fights$Result=="loss",])[1]
    
    ####### Total number of <fights,wins,loses> aggregated <top5rank,top10rank,top20rank,top50rank,top100rank> in past 5 fights
    
    # Previous 5 fights
    if(dim(prev_fights)[1]<=5){ p5f <- prev_fights }
    if(dim(prev_fights)[1]>=6){ p5f <- X[(i-5):(i-1),] }

    X[i,]$F5 <- dim(p5f[p5f$O_Rank<=5,])[1]
    X[i,]$W5 <- dim(p5f[p5f$O_Rank<=5 & p5f$Result=="win",])[1]
    X[i,]$L5 <- dim(p5f[p5f$O_Rank<=5 & p5f$Result=="loss",])[1]
    
    X[i,]$F10 <- dim(p5f[p5f$O_Rank<=10,])[1]
    X[i,]$W10 <- dim(p5f[p5f$O_Rank<=10 & p5f$Result=="win",])[1]
    X[i,]$L10 <- dim(p5f[p5f$O_Rank<=10 & p5f$Result=="loss",])[1]
    
    X[i,]$F20 <- dim(p5f[p5f$O_Rank<=20,])[1]
    X[i,]$W20 <- dim(p5f[p5f$O_Rank<=20 & p5f$Result=="win",])[1]
    X[i,]$L20 <- dim(p5f[p5f$O_Rank<=20 & p5f$Result=="loss",])[1]
    
    X[i,]$F50 <- dim(p5f[p5f$O_Rank<=50,])[1]
    X[i,]$W50 <- dim(p5f[p5f$O_Rank<=50 & p5f$Result=="win",])[1]
    X[i,]$L50 <- dim(p5f[p5f$O_Rank<=50 & p5f$Result=="loss",])[1]
    
    X[i,]$F100 <- dim(p5f[p5f$O_Rank<=100,])[1]
    X[i,]$W100 <- dim(p5f[p5f$O_Rank<=100 & p5f$Result=="win",])[1]
    X[i,]$L100 <- dim(p5f[p5f$O_Rank<=100 & p5f$Result=="loss",])[1]
    
    ####### Over previous 10 fights the number of <KOTKO,SUB,DEC> <wins,losses> aggregated <total,top5rank,top10rank,top20rank,top50rank,top100rank>
    
    # Previous 10 fights
    if(i<=10){ p10f <- X[1:(i-1),] }
    if(i>=11){ p10f <- X[(i-10):(i-1),] }
    
    # Calculate the predictors
    X[i,]$KOTKO_W <- dim(p10f[p10f$Result=="win" & p10f$I_KOTKO==1,])[1]
    X[i,]$SUB_W <- dim(p10f[p10f$Result=="win" & p10f$I_SUB==1,])[1]
    X[i,]$DEC_W <- dim(p10f[p10f$Result=="win" & p10f$I_DEC==1,])[1]
    X[i,]$KOTKO_L <- dim(p10f[p10f$Result=="loss" & p10f$I_KOTKO==1,])[1]
    X[i,]$SUB_L <- dim(p10f[p10f$Result=="loss" & p10f$I_SUB==1,])[1]
    X[i,]$DEC_L <- dim(p10f[p10f$Result=="loss" & p10f$I_DEC==1,])[1]
    
    X[i,]$KOTKO_W5 <- dim(p10f[p10f$O_Rank<=5 & p10f$Result=="win" & p10f$I_KOTKO==1,])[1]
    X[i,]$SUB_W5 <- dim(p10f[p10f$O_Rank<=5 & p10f$Result=="win" & p10f$I_SUB==1,])[1]
    X[i,]$DEC_W5 <- dim(p10f[p10f$O_Rank<=5 & p10f$Result=="win" & p10f$I_DEC==1,])[1]
    X[i,]$KOTKO_L5 <- dim(p10f[p10f$O_Rank<=5 & p10f$Result=="loss" & p10f$I_KOTKO==1,])[1]
    X[i,]$SUB_L5 <- dim(p10f[p10f$O_Rank<=5 & p10f$Result=="loss" & p10f$I_SUB==1,])[1]
    X[i,]$DEC_L5 <- dim(p10f[p10f$O_Rank<=5 & p10f$Result=="loss" & p10f$I_DEC==1,])[1]
    
    X[i,]$KOTKO_W10 <- dim(p10f[p10f$O_Rank<=10 & p10f$Result=="win" & p10f$I_KOTKO==1,])[1]
    X[i,]$SUB_W10 <- dim(p10f[p10f$O_Rank<=10 & p10f$Result=="win" & p10f$I_SUB==1,])[1]
    X[i,]$DEC_W10 <- dim(p10f[p10f$O_Rank<=10 & p10f$Result=="win" & p10f$I_DEC==1,])[1]
    X[i,]$KOTKO_L10 <- dim(p10f[p10f$O_Rank<=10 & p10f$Result=="loss" & p10f$I_KOTKO==1,])[1]
    X[i,]$SUB_L10 <- dim(p10f[p10f$O_Rank<=10 & p10f$Result=="loss" & p10f$I_SUB==1,])[1]
    X[i,]$DEC_L10 <- dim(p10f[p10f$O_Rank<=10 & p10f$Result=="loss" & p10f$I_DEC==1,])[1]
    
    X[i,]$KOTKO_W20 <- dim(p10f[p10f$O_Rank<=20 & p10f$Result=="win" & p10f$I_KOTKO==1,])[1]
    X[i,]$SUB_W20 <- dim(p10f[p10f$O_Rank<=20 & p10f$Result=="win" & p10f$I_SUB==1,])[1]
    X[i,]$DEC_W20 <- dim(p10f[p10f$O_Rank<=20 & p10f$Result=="win" & p10f$I_DEC==1,])[1]
    X[i,]$KOTKO_L20 <- dim(p10f[p10f$O_Rank<=20 & p10f$Result=="loss" & p10f$I_KOTKO==1,])[1]
    X[i,]$SUB_L20 <- dim(p10f[p10f$O_Rank<=20 & p10f$Result=="loss" & p10f$I_SUB==1,])[1]
    X[i,]$DEC_L20 <- dim(p10f[p10f$O_Rank<=20 & p10f$Result=="loss" & p10f$I_DEC==1,])[1]
    
    X[i,]$KOTKO_W50 <- dim(p10f[p10f$O_Rank<=50 & p10f$Result=="win" & p10f$I_KOTKO==1,])[1]
    X[i,]$SUB_W50 <- dim(p10f[p10f$O_Rank<=50 & p10f$Result=="win" & p10f$I_SUB==1,])[1]
    X[i,]$DEC_W50 <- dim(p10f[p10f$O_Rank<=50 & p10f$Result=="win" & p10f$I_DEC==1,])[1]
    X[i,]$KOTKO_L50 <- dim(p10f[p10f$O_Rank<=50 & p10f$Result=="loss" & p10f$I_KOTKO==1,])[1]
    X[i,]$SUB_L50 <- dim(p10f[p10f$O_Rank<=50 & p10f$Result=="loss" & p10f$I_SUB==1,])[1]
    X[i,]$DEC_L50 <- dim(p10f[p10f$O_Rank<=50 & p10f$Result=="loss" & p10f$I_DEC==1,])[1]
    
    X[i,]$KOTKO_W100 <- dim(p10f[p10f$O_Rank<=100 & p10f$Result=="win" & p10f$I_KOTKO==1,])[1]
    X[i,]$SUB_W100 <- dim(p10f[p10f$O_Rank<=100 & p10f$Result=="win" & p10f$I_SUB==1,])[1]
    X[i,]$DEC_W100 <- dim(p10f[p10f$O_Rank<=100 & p10f$Result=="win" & p10f$I_DEC==1,])[1]
    X[i,]$KOTKO_L100 <- dim(p10f[p10f$O_Rank<=100 & p10f$Result=="loss" & p10f$I_KOTKO==1,])[1]
    X[i,]$SUB_L100 <- dim(p10f[p10f$O_Rank<=100 & p10f$Result=="loss" & p10f$I_SUB==1,])[1]
    X[i,]$DEC_L100 <- dim(p10f[p10f$O_Rank<=100 & p10f$Result=="loss" & p10f$I_DEC==1,])[1]
  
    ####### Fighter total cumulative time of fighting (in minutes)
    time_semi_colon_location <- unlist(gregexpr(pattern=":",X[i-1,]$Time))
    time_num_characters <- nchar(as.vector(X[i-1,]$Time))
    X[i,]$Fight_Time <- (X[i-1,]$R-1)*300 + 
      as.numeric(substr(X[i-1,]$Time,1,time_semi_colon_location-1))*60 + 
      as.numeric(substr(X[i-1,]$Time,time_semi_colon_location+1,time_num_characters))
    X[i,]$Fight_Time <- round(X[i,]$Fight_Time/60,2)
    X[i,]$Fight_Time <- X[i,]$Fight_Time + X[i-1,]$Fight_Time
  
    ####### Fighter total cumulative control time (in minutes)
    time_semi_colon_location <- unlist(gregexpr(pattern=":",X[i-1,]$Ctrl))
    time_num_characters <- nchar(as.vector(X[i-1,]$Ctrl))
    X[i,]$CtrlSum <- as.numeric(substr(X[i-1,]$Ctrl,1,time_semi_colon_location-1))*60 + 
                     as.numeric(substr(X[i-1,]$Ctrl,time_semi_colon_location+1,time_num_characters))
    X[i,]$CtrlSum <- round(X[i,]$CtrlSum/60,2)
    X[i,]$CtrlSum <- X[i,]$CtrlSum + X[i-1,]$CtrlSum
    X[i,]$CtrlPerc <- round(X[i,]$CtrlSum / X[i,]$Fight_Time,2)
    
    time_semi_colon_location <- unlist(gregexpr(pattern=":",X[i-1,]$Opp_Ctrl))
    time_num_characters <- nchar(as.vector(X[i-1,]$Opp_Ctrl))
    X[i,]$Opp_CtrlSum <- as.numeric(substr(X[i-1,]$Opp_Ctrl,1,time_semi_colon_location-1))*60 + 
                         as.numeric(substr(X[i-1,]$Opp_Ctrl,time_semi_colon_location+1,time_num_characters))
    X[i,]$Opp_CtrlSum <- round(X[i,]$Opp_CtrlSum/60,2)
    X[i,]$Opp_CtrlSum <- X[i,]$Opp_CtrlSum + X[i-1,]$Opp_CtrlSum
    X[i,]$Opp_CtrlPerc <- round(X[i,]$Opp_CtrlSum / X[i,]$Fight_Time,2)
    
    ####### Cumulative number of days since last <fight,KOTKO loss> at time of fight
    
    # Second or later fight
    prev_fights <- as.data.frame(X[1:(i-1),])
    # Time since last fight
    X[i,]$TS_F <- min(X[i,]$Date - prev_fights$Date)
    # Time since KO loss
    if(length(prev_fights[prev_fights$Result=="loss" & prev_fights$I_KOTKO==1,]$Date)==0){ X[i,]$TS_KOTKOl <- X[i,]$Date - (X[1,]$Date - 365) }
    if(length(prev_fights[prev_fights$Result=="loss" & prev_fights$I_KOTKO==1,]$Date)>=1){ X[i,]$TS_KOTKOl <- min(X[i,]$Date - prev_fights[prev_fights$Result=="loss" & prev_fights$I_KOTKO==1,]$Date) }
    
    ####### Was their 3+ winning streak broken last fight (excluding no contests/draws)
    
    # Extract the previous fights
    prev_fights <- as.data.frame(X[1:(i-1),])
    # If there are 3 (or fewer) fights then set to 0 otherwise grab the appropriate ones
    if(dim(prev_fights)[1]<=3){ X[i,]$WinB3 <- 0 }
    if(dim(prev_fights)[1] >3){ X[i,]$WinB3 <- as.numeric(sum(prev_fights[(dim(prev_fights)[1]-3):(dim(prev_fights)[1]),]$Result==c(rep("win",3),"loss"))==4) }
    
    ####### Number of wins the previous <3,5,7,9,11,13,15> fights (excluding no contests/draws)
    
    # Number of wins in previous 3 fights
    if(i<=3){ prev_3fights <- X[1:(i-1),] }
    if(i>=4){ prev_3fights <- X[(i-3):(i-1),] }
    X[i,]$WinsP3 <- dim(prev_3fights[prev_3fights$Result=="win",])[1]
    
    # Number of wins in previous 5 fights
    if(i<=5){ prev_5fights <- X[1:(i-1),] }
    if(i>=6){ prev_5fights <- X[(i-5):(i-1),] }
    X[i,]$WinsP5 <- dim(prev_5fights[prev_5fights$Result=="win",])[1]
    
    # Number of wins in previous 7 fights
    if(i<=7){ prev_7fights <- X[1:(i-1),] }
    if(i>=8){ prev_7fights <- X[(i-7):(i-1),] }
    X[i,]$WinsP7 <- dim(prev_7fights[prev_7fights$Result=="win",])[1]
    
    # Number of wins in previous 9 fights
    if(i<=9){  prev_9fights <- X[1:(i-1),] }
    if(i>=10){ prev_9fights <- X[(i-9):(i-1),] }
    X[i,]$WinsP9 <- dim(prev_9fights[prev_9fights$Result=="win",])[1]
    
    # Number of wins in previous 11 fights
    if(i<=11){ prev_11fights <- X[1:(i-1),] }
    if(i>=12){ prev_11fights <- X[(i-11):(i-1),] }
    X[i,]$WinsP11 <- dim(prev_11fights[prev_11fights$Result=="win",])[1]
    
    # Number of wins in previous 13 fights
    if(i<=13){ prev_13fights <- X[1:(i-1),] }
    if(i>=14){ prev_13fights <- X[(i-13):(i-1),] }
    X[i,]$WinsP13 <- dim(prev_13fights[prev_13fights$Result=="win",])[1]
    
    # Number of wins in previous 15 fights
    if(i<=15){ prev_15fights <- X[1:(i-1),] }
    if(i>=16){ prev_15fights <- X[(i-15):(i-1),] }
    X[i,]$WinsP15 <- dim(prev_15fights[prev_15fights$Result=="win",])[1]
    
    ####### Number of actions which occur every 5 minutes within past 5 fights
      
    # Previous 5 fights and associated cumulative fight-time within those fights
    if(i<=5){ 
      p5f <- X[1:(i-1),] 
      prev_5fight_time <- X[i,]$Fight_Time }
    if(i>=6){ 
      p5f <- X[(i-5):(i-1),] 
      prev_5fight_time <- X[i,]$Fight_Time - X[i-5,]$Fight_Time }
    
    # Fighter number of actions per 5 minutes
    X[i,]$KD_p5m <-   round( sum(p5f$KD,na.rm=TRUE)   / prev_5fight_time, 2)*5
    X[i,]$SA_p5m <-   round( sum(p5f$SA,na.rm=TRUE)   / prev_5fight_time, 2)*5
    X[i,]$Rev_p5m <-  round( sum(p5f$Rev,na.rm=TRUE)  / prev_5fight_time, 2)*5
    
    X[i,]$SSL_p5m <-  round( sum(p5f$SSL,na.rm=TRUE)  / prev_5fight_time, 2)*5
    X[i,]$SST_p5m <-  round( sum(p5f$SST,na.rm=TRUE)  / prev_5fight_time, 2)*5
    if(sum(p5f$SST,na.rm=TRUE)>=1){ X[i,]$SSAcc <-  sum(p5f$SSL,na.rm=TRUE) / sum(p5f$SST,na.rm=TRUE) }
    
    X[i,]$TDL_p5m <-  round( sum(p5f$TDL,na.rm=TRUE)  / prev_5fight_time, 2)*5
    X[i,]$TDT_p5m <-  round( sum(p5f$TDT,na.rm=TRUE)  / prev_5fight_time, 2)*5
    if(sum(p5f$TDT,na.rm=TRUE)>=1){ X[i,]$TDAcc <- sum(p5f$TDL,na.rm=TRUE) / sum(p5f$TDT,na.rm=TRUE) }
    
    X[i,]$HeadL_p5m <-   round( sum(p5f$HeadL,na.rm=TRUE)     / prev_5fight_time, 2)*5
    X[i,]$BodyL_p5m <-   round( sum(p5f$BodyL,na.rm=TRUE)     / prev_5fight_time, 2)*5
    X[i,]$LegL_p5m <-    round( sum(p5f$LegL,na.rm=TRUE)      / prev_5fight_time, 2)*5
    X[i,]$DistL_p5m <-   round( sum(p5f$DistL,na.rm=TRUE) / prev_5fight_time, 2)*5
    X[i,]$ClinchL_p5m <- round( sum(p5f$ClinchL,na.rm=TRUE)   / prev_5fight_time, 2)*5
    X[i,]$GrdL_p5m <-    round( sum(p5f$GrdL,na.rm=TRUE)   / prev_5fight_time, 2)*5
    
    # Fighter number of actions per 5 minutes
    X[i,]$Opp_KD_p5m <-   round( sum(p5f$Opp_KD,na.rm=TRUE)   / prev_5fight_time, 2)*5
    X[i,]$Opp_SA_p5m <-   round( sum(p5f$Opp_SA,na.rm=TRUE)   / prev_5fight_time, 2)*5
    X[i,]$Opp_Rev_p5m <-  round( sum(p5f$Opp_Rev,na.rm=TRUE)  / prev_5fight_time, 2)*5
    
    X[i,]$Opp_SSL_p5m <-  round( sum(p5f$Opp_SSL,na.rm=TRUE)  / prev_5fight_time, 2)*5
    X[i,]$Opp_SST_p5m <-  round( sum(p5f$Opp_SST,na.rm=TRUE)  / prev_5fight_time, 2)*5
    if(sum(p5f$Opp_SST,na.rm=TRUE)>=1){ X[i,]$Opp_SSAcc <-  sum(p5f$Opp_SSL,na.rm=TRUE) / sum(p5f$Opp_SST,na.rm=TRUE) }
    
    X[i,]$Opp_TDL_p5m <-  round( sum(p5f$Opp_TDL,na.rm=TRUE)  / prev_5fight_time, 2)*5
    X[i,]$Opp_TDT_p5m <-  round( sum(p5f$Opp_TDT,na.rm=TRUE)  / prev_5fight_time, 2)*5
    if(sum(p5f$Opp_TDT,na.rm=TRUE)>=1){ X[i,]$Opp_TDAcc <- sum(p5f$Opp_TDL,na.rm=TRUE) / sum(p5f$Opp_TDT,na.rm=TRUE) }
    
    X[i,]$Opp_HeadL_p5m <-   round( sum(p5f$Opp_HeadL,na.rm=TRUE)     / prev_5fight_time, 2)*5
    X[i,]$Opp_BodyL_p5m <-   round( sum(p5f$Opp_BodyL,na.rm=TRUE)     / prev_5fight_time, 2)*5
    X[i,]$Opp_LegL_p5m <-    round( sum(p5f$Opp_LegL,na.rm=TRUE)      / prev_5fight_time, 2)*5
    X[i,]$Opp_DistL_p5m <-   round( sum(p5f$Opp_DistL,na.rm=TRUE) / prev_5fight_time, 2)*5
    X[i,]$Opp_ClinchL_p5m <- round( sum(p5f$Opp_ClinchL,na.rm=TRUE)   / prev_5fight_time, 2)*5
    X[i,]$Opp_GrdL_p5m <-    round( sum(p5f$Opp_GrdL,na.rm=TRUE)   / prev_5fight_time, 2)*5
    
    ####### Significant strikes thrown Beta in last 10 fights
    
    # Previous 10 fights
    if(i<=10){ p10f <- X[1:(i-1),c("Time","R","RD1SSL","RD2SSL","RD3SSL","RD4SSL","RD5SSL","Opp_RD1SSL","Opp_RD2SSL","Opp_RD3SSL","Opp_RD4SSL","Opp_RD5SSL")] }
    if(i>=11){ p10f <- X[(i-10):(i-1),c("Time","R","RD1SSL","RD2SSL","RD3SSL","RD4SSL","RD5SSL","Opp_RD1SSL","Opp_RD2SSL","Opp_RD3SSL","Opp_RD4SSL","Opp_RD5SSL")] }
    
    # Each fight's individual time in seconds and proportion of time of the round fought (use this later to scale SSL per round)
    time_semi_colon_locations <- unlist(gregexpr(pattern=":",p10f$Time))
    time_num_characters <- nchar(as.vector(p10f$Time))
    p10f$This_Fights_Time <- (p10f$R-1)*300 + 
                                as.numeric(substr(p10f$Time,1,time_semi_colon_locations-1))*60 + 
                                as.numeric(substr(p10f$Time,time_semi_colon_locations+1,time_num_characters))
    p10f$Max_Fights_Time <- p10f$R*300
    p10f$PropRoundFought <- p10f$This_Fights_Time / p10f$Max_Fights_Time
    
    # Scaled SSL per round
    for(k in 1:dim(p10f)[1]){
      if(p10f[k,]$R==6){ next }
      p10f[k,paste0("RD",p10f[k,]$R,"SSL")] <- round(p10f[k,paste0("RD",p10f[k,]$R,"SSL")] / p10f[k,]$PropRoundFought,1)
      p10f[k,paste0("Opp_RD",p10f[k,]$R,"SSL")] <- round(p10f[k,paste0("Opp_RD",p10f[k,]$R,"SSL")] / p10f[k,]$PropRoundFought,1) }
    
    # Dataframe needed for calculating Beta
    FgtDF <- NULL
    OppDF <- NULL
    for(kk in 1:dim(p10f)[1]){
      FgtDF <- rbind(FgtDF, data.frame(R=1:5,SSL=unlist(p10f[kk,c("RD1SSL","RD2SSL","RD3SSL","RD4SSL","RD5SSL")])))
      OppDF <- rbind(OppDF, data.frame(R=1:5,SSL=unlist(p10f[kk,c("Opp_RD1SSL","Opp_RD2SSL","Opp_RD3SSL","Opp_RD4SSL","Opp_RD5SSL")]))) }
    FgtDF <- FgtDF[is.na(FgtDF$SSL)==FALSE,]
    OppDF <- OppDF[is.na(OppDF$SSL)==FALSE,]
    
    # Calculate the beta terms
    #   plot(SSL~R,data=FgtDF)
    #   reg <- lm(SSL~R,data=FgtDF)
    #   abline(reg)
    #   plot(SSL~R,data=OppDF)
    #   reg <- lm(SSL~R,data=OppDF)
    #   abline(reg)
    #   dev.off()
    
    # Calculate the slope of SSL across all the rounds they've fought in. Only try to estimate this if,
    #   (1) there's at least 3 points of data (3 different rounds fought in)
    #   (2) all data points don't correspond to the 1st round (all 1st round stoppages) otherwise the predictor is the same for each output and no line can be fit
    if(dim(FgtDF)[1]<=2 | length(unique(FgtDF$R))<=1){ 
      X[i,]$SSL_Beta     <- 0
      X[i,]$Opp_SSL_Beta <- 0 }
    if(dim(FgtDF)[1]>=3 & length(unique(FgtDF$R))>1){
      X[i,]$SSL_Beta     <- lm(SSL~R,data=FgtDF)$coef["R"]
      X[i,]$Opp_SSL_Beta <- lm(SSL~R,data=OppDF)$coef["R"] }
  }
    
  # Save the resulting dataframes
  write.csv(X, file=paste0(as.vector(as.vector(X[1,]$Fighter))," (i=",which(all_fighters==fighter),").csv"),row.names=FALSE)
  
  # Output Progress Bar
  setTxtProgressBar(pb=prog.bar,value=which(all_fighters==fighter))
}

# Create a master Final_Sherdog dataset
setwd(dir=paste0(base.dir,"Processed data/temp"))
Final_UFC <- list()
all_files_in_dir <- list.files(path=paste0(base.dir,"Processed data/temp/"))
prog.bar <- txtProgressBar(min=1,max=length(all_files_in_dir),style=3)

for(file in all_files_in_dir){
  Final_UFC[[which(all_files_in_dir==file)]] <- read.csv(file=file)
  setTxtProgressBar(pb=prog.bar,value=which(all_files_in_dir==file)) }
Final_UFC <- list.stack(Final_UFC)

# Save the results
setwd(dir=paste0(base.dir,"Processed data"))
All_UFC_Fights <- Final_UFC
save(All_UFC_Fights, file="4a_All_UFC_Fights.RDATA")
write.csv(x=All_UFC_Fights, file="4a_All_UFC_Fights.csv", row.names=FALSE)

# Clear contents of the temporary directory
removeDirectory(path=paste0(base.dir,"Processed data/temp"), recursive=TRUE, mustExist=TRUE)

# Clear the workspace
rm(list = ls()[ !(ls() %in% params.and.funcs) ])

#=========================================================#
# 4b - [Combine data sources and process final dataframe] #
#         Merge master dataset with itself                #
#=========================================================#

# Read in the data
setwd(dir=paste0(base.dir,"Processed data"))
load(file="4a_All_UFC_Fights.RDATA")

# Shorter name dataframe
AUF <- All_UFC_Fights
rm(list="All_UFC_Fights")

# Store temporary results here
dir(path=paste0(base.dir,"Processed data/temp"))
setwd(dir=paste0(base.dir,"Processed data/temp"))

# Stupid factor variables
AUF$Fighter <- as.vector(AUF$Fighter)
AUF$Opponent <- as.vector(AUF$Opponent)
AUF$Date <- ymd(AUF$Date)

# Combine WC=f(F_WC,O_WC) and Gender=f(F_Gender,O_Gender) to combine them into a single variable
AUF$F_WC <- as.vector(AUF$F_WC)
AUF$O_WC <- as.vector(AUF$O_WC)
AUF$WC <- NA
AUF[which(AUF$F_WC==AUF$O_WC),]$WC <- AUF[which(AUF$F_WC==AUF$O_WC),]$F_WC
AUF[is.na(AUF$F_WC)==FALSE & is.na(AUF$O_WC)==TRUE,]$WC <- AUF[is.na(AUF$F_WC)==FALSE & is.na(AUF$O_WC)==TRUE,]$F_WC
AUF[is.na(AUF$F_WC)==TRUE & is.na(AUF$O_WC)==FALSE,]$WC <- AUF[is.na(AUF$F_WC)==TRUE & is.na(AUF$O_WC)==FALSE,]$O_WC
table(AUF$WC,useNA="always")
AUF <- AUF[,!(colnames(AUF) %in% c("F_WC","O_WC"))]

AUF$F_Gender <- as.vector(AUF$F_Gender)
AUF$O_Gender <- as.vector(AUF$O_Gender)
AUF$Gender <- NA
AUF[which(AUF$F_Gender==AUF$O_Gender),]$Gender <- AUF[which(AUF$F_Gender==AUF$O_Gender),]$F_Gender
AUF[is.na(AUF$F_Gender)==FALSE & is.na(AUF$O_Gender)==TRUE,]$Gender <- AUF[is.na(AUF$F_Gender)==FALSE & is.na(AUF$O_Gender)==TRUE,]$F_Gender
AUF[is.na(AUF$F_Gender)==TRUE & is.na(AUF$O_Gender)==FALSE,]$Gender <- AUF[is.na(AUF$F_Gender)==TRUE & is.na(AUF$O_Gender)==FALSE,]$O_Gender
AUF[which(AUF$Gender=="Male"),]$Gender <- "Men"
table(AUF$Gender,useNA="always")
AUF <- AUF[,!(colnames(AUF) %in% c("F_Gender","O_Gender"))]

# Progress bar
prog.bar <- txtProgressBar(min=1,max=dim(AUF)[1],style=3)

for(i in 1:dim(AUF)[1]){ # i <- 3397
  
  # Fighter, Opponent, Date of fight
  fighter <- AUF[i,]$Fighter
  opponent <- AUF[i,]$Opponent
  date <- AUF[i,]$Date
  
  # Fighter Sherdog data cumulative up to this fight
  UFC_F <- AUF[i,c("Fighter","Date","Result","R","Time","I_DEC","I_KOTKO","I_SUB","I5",
                   "WC","Gender","DOB","Age","Hgt","Rch","Wt","StOr","StSw","StSp",
                   "F_Rank","F_odds",
                   "HghRnkFgtLst3","HghRnkWinLst3",
                   "F","W","L","F5","W5","L5","F10","W10","L10","F20","W20","L20","F50","W50","L50","F100","W100","L100",
                   "KOTKO_W","SUB_W","DEC_W","KOTKO_L","SUB_L","DEC_L","KOTKO_W5","SUB_W5","DEC_W5","KOTKO_L5","SUB_L5","DEC_L5","KOTKO_W10","SUB_W10","DEC_W10","KOTKO_L10","SUB_L10","DEC_L10",
                   "KOTKO_W20","SUB_W20","DEC_W20","KOTKO_L20","SUB_L20","DEC_L20","KOTKO_W50","SUB_W50","DEC_W50","KOTKO_L50","SUB_L50","DEC_L50","KOTKO_W100","SUB_W100","DEC_W100","KOTKO_L100","SUB_L100","DEC_L100",
                   "TS_F","TS_KOTKOl",
                   "WinB3",
                   "WinsP3","WinsP5","WinsP7","WinsP9","WinsP11","WinsP13","WinsP15",
                   "Fight_Time",
                   "CtrlSum","CtrlPerc",
                   "KD_p5m","Rev_p5m","SA_p5m","SSL_p5m","SST_p5m","SSAcc","TDL_p5m","TDT_p5m","TDAcc","HeadL_p5m","BodyL_p5m","LegL_p5m","DistL_p5m","ClinchL_p5m","GrdL_p5m",
                   "SSL_Beta",
                   "Opp_KD_p5m","Opp_Rev_p5m","Opp_SA_p5m","Opp_SSL_p5m","Opp_SST_p5m","Opp_SSAcc","Opp_TDL_p5m","Opp_TDT_p5m","Opp_TDAcc","Opp_HeadL_p5m","Opp_BodyL_p5m","Opp_LegL_p5m","Opp_DistL_p5m","Opp_ClinchL_p5m","Opp_GrdL_p5m",
                   "Opp_SSL_Beta")]
  names(UFC_F)[names(UFC_F)=="F_Rank"] <- "Rank"
  names(UFC_F)[names(UFC_F)=="F_odds"] <- "odds"
  names(UFC_F)[10:dim(UFC_F)[2]] <- paste0("F_",names(UFC_F)[10:dim(UFC_F)[2]])
  
  # Opponent Sherdog data cumulative up to this fight
  UFC_O <- AUF[AUF$Fighter==opponent & AUF$Opponent==fighter & AUF$Date==date,
               c("Fighter",
                 "WC","Gender","DOB","Age","Hgt","Rch","Wt","StOr","StSw","StSp",
                 "F_Rank","F_odds",
                 "HghRnkFgtLst3","HghRnkWinLst3",
                 "F","W","L","F5","W5","L5","F10","W10","L10","F20","W20","L20","F50","W50","L50","F100","W100","L100",
                 "KOTKO_W","SUB_W","DEC_W","KOTKO_L","SUB_L","DEC_L","KOTKO_W5","SUB_W5","DEC_W5","KOTKO_L5","SUB_L5","DEC_L5","KOTKO_W10","SUB_W10","DEC_W10","KOTKO_L10","SUB_L10","DEC_L10",
                 "KOTKO_W20","SUB_W20","DEC_W20","KOTKO_L20","SUB_L20","DEC_L20","KOTKO_W50","SUB_W50","DEC_W50","KOTKO_L50","SUB_L50","DEC_L50","KOTKO_W100","SUB_W100","DEC_W100","KOTKO_L100","SUB_L100","DEC_L100",
                 "TS_F","TS_KOTKOl",
                 "WinB3",
                 "WinsP3","WinsP5","WinsP7","WinsP9","WinsP11","WinsP13","WinsP15",
                 "Fight_Time",
                 "CtrlSum","CtrlPerc",
                 "KD_p5m","Rev_p5m","SA_p5m","SSL_p5m","SST_p5m","SSAcc","TDL_p5m","TDT_p5m","TDAcc","HeadL_p5m","BodyL_p5m","LegL_p5m","DistL_p5m","ClinchL_p5m","GrdL_p5m",
                 "SSL_Beta",
                 "Opp_KD_p5m","Opp_Rev_p5m","Opp_SA_p5m","Opp_SSL_p5m","Opp_SST_p5m","Opp_SSAcc","Opp_TDL_p5m","Opp_TDT_p5m","Opp_TDAcc","Opp_HeadL_p5m","Opp_BodyL_p5m","Opp_LegL_p5m","Opp_DistL_p5m","Opp_ClinchL_p5m","Opp_GrdL_p5m",
                 "Opp_SSL_Beta")]
  names(UFC_O)[names(UFC_O)=="Fighter"] <- "Opponent"
  names(UFC_O)[names(UFC_O)=="F_Rank"] <- "Rank"
  names(UFC_O)[names(UFC_O)=="F_odds"] <- "odds"
  names(UFC_O)[2:dim(UFC_O)[2]] <- paste0("O_",names(UFC_O)[2:dim(UFC_O)[2]])
  
  # Combine the data from all sources
  AUF_merged_i <- cbind(UFC_F, UFC_O, row.names=NULL)
  
  # Append all fighter data to the master dataframe
  write.csv(x=AUF_merged_i, file=paste0("AUF_merged_i_",i,".csv"), row.names=FALSE)
  
  # Output Progress Bar
  setTxtProgressBar(pb=prog.bar,value=i) }

# Create a master dataset appending all the .csv files
setwd(dir=paste0(base.dir,"Processed data/temp"))

all_files_in_dir <- list.files(path=paste0(base.dir,"Processed data/temp"))
prog.bar <- txtProgressBar(min=1,max=length(all_files_in_dir),style=3)

AUF_merged <- list()
for(file in all_files_in_dir){
  AUF_merged[[which(all_files_in_dir==file)]] <- read.csv(file=file)
  setTxtProgressBar(pb=prog.bar,value=which(all_files_in_dir==file)) }
AUF_merged <- list.stack(AUF_merged)

# Save the results
All_UFC_Fights <- AUF_merged
setwd(dir=paste0(base.dir,"Processed data"))
save(All_UFC_Fights, file="4b_All_UFC_Fights.RDATA")
write.csv(x=All_UFC_Fights, file="4b_All_UFC_Fights.csv", row.names=FALSE)

# Clear contents of the temporary directory
removeDirectory(path=paste0(base.dir,"Processed data/temp"), recursive=TRUE, mustExist=TRUE)

# Clear the workspace
rm(list = ls()[ !(ls() %in% params.and.funcs) ])

#=========================================================#
# 4c - [Combine data sources and process final dataframe] #
#   Impute/combine/transform/drop some variables          #
#=========================================================#

# Read in the data
setwd(dir=paste0(base.dir,"Processed data"))
load(file="4b_All_UFC_Fights.RDATA")
AUF <- All_UFC_Fights
rm(list=ls()[ls() %in% "All_UFC_Fights"])

#--------#
# Gender #
#--------#

# Impute Gender
AUF$AEG_key <- 1:dim(AUF)[1]
AUF$Fighter <- as.vector(AUF$Fighter)
AUF$Opponent <- as.vector(AUF$Opponent)

AUF_temp_a <- AUF
AUF_temp_b <- AUF[,c("Fighter","Opponent","Date","AEG_key","F_Gender","O_Gender")]

prog.bar <- txtProgressBar(min=1,max=length(unique(AUF_temp_b$Fighter)),style=3)
for(j in 1:3){
  # Progress prior to this iteration
  cat("\nAfter iteration ",j-1," the number of missing Fighter/Opponent genders is,\n",sep="")
  cat("  AUF_temp_b$F_Gender:\t",sum(is.na(AUF_temp_b$F_Gender)),"\n")
  cat("  AUF_temp_b$O_Gender:\t",sum(is.na(AUF_temp_b$O_Gender)),"\n")
  # Reset iteration counter
  counter <- 0
  for(fighter in unique(AUF_temp_b$Fighter)){ # fighter=unique(AUF_temp_b$Fighter)[1]
    # Increase iteration counter
    counter <- counter + 1
    # For a given fighter set all F_Gender/O_Gender to be the most frequent Gender. We don't include the missing values (NA) when we create the
    # distribution of gender values, so we don't have to worry about the most frequent value being NA, and thus being used to recode other values
    gender_vals <- sort(x=table(unlist(AUF_temp_b[(AUF_temp_b$Fighter==fighter & is.na(AUF_temp_b$Fighter==fighter)==FALSE) | (AUF_temp_b$Opponent==fighter & is.na(AUF_temp_b$Opponent)==FALSE),c("F_Gender","O_Gender")])),decreasing=TRUE)
    if(length(gender_vals)>=1){ 
      AUF_temp_b[(AUF_temp_b$Fighter==fighter & is.na(AUF_temp_b$Fighter==fighter)==FALSE) | (AUF_temp_b$Opponent==fighter & is.na(AUF_temp_b$Opponent)==FALSE),c("F_Gender","O_Gender")] <- names(gender_vals)[1] }
    # Output Progress Bar
    setTxtProgressBar(pb=prog.bar,value=counter) } }

# Progress after last iteration
cat("\nAfter iteration ",j," the number of missing Fighter/Opponent genders is,\n",sep="")
cat("  AUF_temp_b$F_Gender:\t",sum(is.na(AUF_temp_b$F_Gender)),"\n")
cat("  AUF_temp_b$O_Gender:\t",sum(is.na(AUF_temp_b$O_Gender)),"\n")

# Update the master dataset with the imputed values
AUF_temp_c <- merge(x=AUF_temp_a[,!(colnames(AUF_temp_a) %in% c("F_Gender","O_Gender"))],y=AUF_temp_b[,c("AEG_key","F_Gender","O_Gender")],by="AEG_key")
AUF <- AUF_temp_c

# Create a single Gender variable for a given fight
AUF$F_Gender <- as.vector(AUF$F_Gender)
AUF$O_Gender <- as.vector(AUF$O_Gender)
AUF$Gender <- NA
AUF[which(AUF$F_Gender==AUF$O_Gender),]$Gender <- AUF[which(AUF$F_Gender==AUF$O_Gender),]$F_Gender
AUF[is.na(AUF$F_Gender)==FALSE & is.na(AUF$O_Gender)==TRUE,]$Gender <- AUF[is.na(AUF$F_Gender)==FALSE & is.na(AUF$O_Gender)==TRUE,]$F_Gender
AUF[is.na(AUF$F_Gender)==TRUE & is.na(AUF$O_Gender)==FALSE,]$Gender <- AUF[is.na(AUF$F_Gender)==TRUE & is.na(AUF$O_Gender)==FALSE,]$O_Gender
table(AUF$Gender,useNA="always")
AUF <- AUF[,!(colnames(AUF) %in% c("F_Gender","O_Gender"))]

# Clear workspace of temporary objects
rm(list = ls()[ !(ls() %in% c(params.and.funcs,"AUF")) ])

#------------------#
# Weightclass (WC) #
#------------------#

# We have 2 versions of weightclass, "WC" from the rankings website and "WeightClass" from the Sherdog website. We'll use the Rankings versions 
# since these reflect the different weightclasses a fighter may fight at over time, whereas the Sherdog version just gives the most recent
# weightclass that the fighter is currently fighting in. Fighters intially start out unranked, so they'll be missing Weightclass/gender information
# for their initial fights, use the fighter's more recent values for these fields (from later fights) to impute the values for the earlier fights 
# (when the values are missing). Or look at values of ranked people they've fought. At the very end if we have no weightclass information then we'll
# impute with the Sherdog weightclass information of either the fighter or their opponent.

# Impute WC then create a single WC variable for a given fight
AUF_temp_a <- AUF
AUF_temp_b <- AUF[,c("Fighter","Opponent","Date","AEG_key","F_WC","O_WC")]

prog.bar <- txtProgressBar(min=1,max=length(unique(AUF_temp_b$Fighter)),style=3)
for(j in 1:4){
  # Progress prior to this iteration
  cat("\nAfter iteration 0 the number of missing Fighter/Opponent WC is,\n",sep="")
  cat("  AUF_temp_b$F_WC:\t",sum(is.na(AUF_temp_b$F_WC)),"\n")
  cat("  AUF_temp_b$O_WC:\t",sum(is.na(AUF_temp_b$O_WC)),"\n")
  # Reset iteration counter
  counter <- 0
  for(fighter in unique(AUF_temp_b$Fighter)){ # fighter <- "Conor McGregor"
    # Increase iteration counter
    counter <- counter + 1
    # For a given fighter set missing F_WC/O_WC to be the most frequent WC
    WC_vals <- sort(x=table(unlist(AUF_temp_b[(AUF_temp_b$Fighter==fighter & is.na(AUF_temp_b$Fighter==fighter)==FALSE) | (AUF_temp_b$Opponent==fighter & is.na(AUF_temp_b$Opponent)==FALSE),c("F_WC","O_WC")])),decreasing=TRUE)
    if(length(WC_vals)>=1){
      if(length(AUF_temp_b[((AUF_temp_b$Fighter==fighter & is.na(AUF_temp_b$Fighter==fighter)==FALSE) | (AUF_temp_b$Opponent==fighter & is.na(AUF_temp_b$Opponent)==FALSE)) & is.na(AUF_temp_b$F_WC),]$F_WC)>0){
        AUF_temp_b[((AUF_temp_b$Fighter==fighter & is.na(AUF_temp_b$Fighter==fighter)==FALSE) | (AUF_temp_b$Opponent==fighter & is.na(AUF_temp_b$Opponent)==FALSE)) & is.na(AUF_temp_b$F_WC),]$F_WC <- names(WC_vals)[1] }
      if(length(AUF_temp_b[((AUF_temp_b$Fighter==fighter & is.na(AUF_temp_b$Fighter==fighter)==FALSE) | (AUF_temp_b$Opponent==fighter & is.na(AUF_temp_b$Opponent)==FALSE)) & is.na(AUF_temp_b$O_WC),]$O_WC)>0){
        AUF_temp_b[((AUF_temp_b$Fighter==fighter & is.na(AUF_temp_b$Fighter==fighter)==FALSE) | (AUF_temp_b$Opponent==fighter & is.na(AUF_temp_b$Opponent)==FALSE)) & is.na(AUF_temp_b$O_WC),]$O_WC <- names(WC_vals)[1] } }
  # Output Progress Bar
  setTxtProgressBar(pb=prog.bar,value=counter) } }

# Progress after last iteration
cat("\nAfter iteration ",j," the number of missing Fighter/Opponent WC is,\n",sep="")
cat("  AUF_temp_b$F_WC:\t",sum(is.na(AUF_temp_b$F_WC)),"\n")
cat("  AUF_temp_b$O_WC:\t",sum(is.na(AUF_temp_b$O_WC)),"\n")

# Update the master dataset with the imputed values
AUF_temp_c <- merge(x=AUF_temp_a[,!(colnames(AUF_temp_a) %in% c("F_WC","O_WC"))],y=AUF_temp_b[,c("AEG_key","F_WC","O_WC")],by="AEG_key")
AUF <- AUF_temp_c

# Create a single WC variable for a given fight
AUF$F_WC <- as.vector(AUF$F_WC)
AUF$O_WC <- as.vector(AUF$O_WC)
AUF$WC <- NA
AUF[which(AUF$F_WC==AUF$O_WC),]$WC <- AUF[which(AUF$F_WC==AUF$O_WC),]$F_WC
AUF[is.na(AUF$F_WC)==FALSE & is.na(AUF$O_WC)==TRUE,]$WC <- AUF[is.na(AUF$F_WC)==FALSE & is.na(AUF$O_WC)==TRUE,]$F_WC
AUF[is.na(AUF$F_WC)==TRUE & is.na(AUF$O_WC)==FALSE,]$WC <- AUF[is.na(AUF$F_WC)==TRUE & is.na(AUF$O_WC)==FALSE,]$O_WC
table(AUF$WC,useNA="always")
AUF <- AUF[,!(colnames(AUF) %in% c("F_WC","O_WC"))]

# Make sure that all the levels of WC are aligned with standard UFC 10 weightclasses
table(AUF$WC,useNA="always")

AUF[which(AUF$WC=="Atomweight (Women)"),]$WC <- "Atomweight"
AUF[which(AUF$WC=="Bantamweight (Women)"),]$WC <- "Bantamweight"
AUF[which(AUF$WC=="Featherweight (Women)"),]$WC <- "Featherweight"
AUF[which(AUF$WC=="Flyweight (Women)"),]$WC <- "Flyweight"
AUF[which(AUF$WC=="Strawweight (Women)"),]$WC <- "Strawweight"

AUF[which(AUF$WC=="Bantamweight (Men)"),]$WC <- "Bantamweight"
AUF[which(AUF$WC=="Featherweight (Men)"),]$WC <- "Featherweight"
AUF[which(AUF$WC=="Flyweight (Men)"),]$WC <- "Flyweight"
AUF[which(AUF$WC=="Heavyweight (Men)"),]$WC <- "Heavyweight"
AUF[which(AUF$WC=="LightHeavyweight (Men)"),]$WC <- "LightHeavyweight"
AUF[which(AUF$WC=="Lightweight (Men)"),]$WC <- "Lightweight"
AUF[which(AUF$WC=="Middleweight (Men)"),]$WC <- "Middleweight"
#AUF[which(AUF$WC=="Strawweight (Men)"),]$WC <- "Strawweight"
AUF[which(AUF$WC=="Welterweight (Men)"),]$WC <- "Welterweight"

table(AUF$WC,useNA="always")

# Make weightclass a numeric variable (Atomweight - Heavyweight) <=> (1,10)
AUF$WCNum <- NA
AUF$WC10 <- AUF$WC9 <- AUF$WC8 <- AUF$WC7 <- AUF$WC6 <- AUF$WC5 <- AUF$WC4 <- AUF$WC3 <- AUF$WC2 <- AUF$WC1 <- 0

AUF[AUF$WC=="Atomweight" & is.na(AUF$WC)==FALSE,]$WCNum <- 1
AUF[AUF$WC=="Atomweight" & is.na(AUF$WC)==FALSE,]$WC1 <- 1

AUF[AUF$WC=="Strawweight" & is.na(AUF$WC)==FALSE,]$WCNum <- 2
AUF[AUF$WC=="Strawweight" & is.na(AUF$WC)==FALSE,]$WC2 <- 1

AUF[AUF$WC=="Flyweight" & is.na(AUF$WC)==FALSE,]$WCNum <- 3
AUF[AUF$WC=="Flyweight" & is.na(AUF$WC)==FALSE,]$WC3 <- 1

AUF[AUF$WC=="Bantamweight" & is.na(AUF$WC)==FALSE,]$WCNum <- 4
AUF[AUF$WC=="Bantamweight" & is.na(AUF$WC)==FALSE,]$WC4 <- 1

AUF[AUF$WC=="Featherweight" & is.na(AUF$WC)==FALSE,]$WCNum <- 5
AUF[AUF$WC=="Featherweight" & is.na(AUF$WC)==FALSE,]$WC5 <- 1

AUF[AUF$WC=="Lightweight" & is.na(AUF$WC)==FALSE,]$WCNum <- 6
AUF[AUF$WC=="Lightweight" & is.na(AUF$WC)==FALSE,]$WC6 <- 1

AUF[AUF$WC=="Welterweight" & is.na(AUF$WC)==FALSE,]$WCNum <- 7
AUF[AUF$WC=="Welterweight" & is.na(AUF$WC)==FALSE,]$WC7 <- 1

AUF[AUF$WC=="Middleweight" & is.na(AUF$WC)==FALSE,]$WCNum <- 8
AUF[AUF$WC=="Middleweight" & is.na(AUF$WC)==FALSE,]$WC8 <- 1

AUF[AUF$WC=="LightHeavyweight" & is.na(AUF$WC)==FALSE,]$WCNum <- 9
AUF[AUF$WC=="LightHeavyweight" & is.na(AUF$WC)==FALSE,]$WC9 <- 1

AUF[AUF$WC=="Heavyweight" & is.na(AUF$WC)==FALSE,]$WCNum <- 10
AUF[AUF$WC=="Heavyweight" & is.na(AUF$WC)==FALSE,]$WC10 <- 1

table(AUF$WC,useNA="always")
table(AUF$WCNum,useNA="always")

table(AUF$WC1,useNA="always")
table(AUF$WC2,useNA="always")
table(AUF$WC3,useNA="always")
table(AUF$WC4,useNA="always")
table(AUF$WC5,useNA="always")
table(AUF$WC6,useNA="always")
table(AUF$WC7,useNA="always")
table(AUF$WC8,useNA="always")
table(AUF$WC9,useNA="always")
table(AUF$WC10,useNA="always")

#-----#
# Age #
#-----#

# We scrape DOB off the UFC stats page to calculate age at time of fight. If we don't have this, then we uniformally don't have it for all fights corresponding to a given
# fighter. In that case simply impute the age to be the mean age across all fights
table(c(AUF$F_Age,AUF$O_Age),useNA="always")
ImpVal <- round(mean(c(AUF$F_Age,AUF$O_Age),na.rm=TRUE),1)
AUF[is.na(AUF$F_Age),]$F_Age <- ImpVal
AUF[is.na(AUF$O_Age),]$O_Age <- ImpVal
table(c(AUF$F_Age,AUF$O_Age),useNA="always")

#--------#
# Height #
#--------#

# We scrape height off the UFC stats page, so if we don't have this, then we uniformally don't have it for all fights corresponding to a given fighter. In that case we will
# simply impute the height, but in a slightly different way, imputing height based on average height in the given weightclass for the given gender
table(c(AUF$F_Hgt,AUF$O_Hgt),useNA="always")
idx_with_missing <- which(is.na(AUF$F_Hgt) | is.na(AUF$O_Hgt))
prog.bar <- txtProgressBar(min=1,max=length(idx_with_missing),style=3)
for(i in idx_with_missing){
  WC <- AUF[i,]$WC
  Gender <- AUF[i,]$Gender
  if(is.na(AUF[i,]$F_Hgt)==TRUE){ AUF[i,]$F_Hgt <- round(mean(unlist(AUF[AUF$WC==WC & AUF$Gender==Gender,c("F_Hgt","O_Hgt")]),na.rm=TRUE),0) }
  if(is.na(AUF[i,]$O_Hgt)==TRUE){ AUF[i,]$O_Hgt <- round(mean(unlist(AUF[AUF$WC==WC & AUF$Gender==Gender,c("F_Hgt","O_Hgt")]),na.rm=TRUE),0) }
  setTxtProgressBar(pb=prog.bar,value=which(i==idx_with_missing)) }
AUF[is.nan(AUF$F_Hgt)==TRUE,]$F_Hgt <- NA
AUF[is.nan(AUF$O_Hgt)==TRUE,]$O_Hgt <- NA
table(c(AUF$F_Hgt,AUF$O_Hgt),useNA="always")

#-------#
# Reach #
#-------#

# We scrape height off the UFC stats page, so if we don't have this, then we uniformally don't have it for all fights corresponding to a given fighter. In that case we will
# simply impute the reach, but in a slightly different way, imputing reach based on average reach in the given weightclass for the given gender
table(c(AUF$F_Rch,AUF$O_Rch),useNA="always")
idx_with_missing <- which(is.na(AUF$F_Rch) | is.na(AUF$O_Rch))
prog.bar <- txtProgressBar(min=1,max=length(idx_with_missing),style=3)
for(i in idx_with_missing){
  WC <- AUF[i,]$WC
  Gender <- AUF[i,]$Gender
  if(is.na(AUF[i,]$F_Rch)==TRUE){ AUF[i,]$F_Rch <- round(mean(unlist(AUF[AUF$WC==WC & AUF$Gender==Gender,c("F_Rch","O_Rch")]),na.rm=TRUE),0) }
  if(is.na(AUF[i,]$O_Rch)==TRUE){ AUF[i,]$O_Rch <- round(mean(unlist(AUF[AUF$WC==WC & AUF$Gender==Gender,c("F_Rch","O_Rch")]),na.rm=TRUE),0) }
  setTxtProgressBar(pb=prog.bar,value=which(i==idx_with_missing)) }
AUF[is.nan(AUF$F_Rch)==TRUE,]$F_Rch <- NA
AUF[is.nan(AUF$O_Rch)==TRUE,]$O_Rch <- NA
table(c(AUF$F_Rch,AUF$O_Rch),useNA="always")

#-------------------#
# Stance Indicators #
#-------------------#

# We scrape height off the UFC stats page, so if we don't have this, then we uniformally don't have it for all fights corresponding to a given fighter. In that case we will
# just assume they are Orthodox stance since that's by far the most popular
table(c(AUF$F_StOr,AUF$O_StOr),useNA="always")
table(c(AUF$F_StSp,AUF$O_StSp),useNA="always")
table(c(AUF$F_StSw,AUF$O_StSw),useNA="always")

AUF[is.na(AUF$F_StOr),]$F_StOr <- 1
AUF[is.na(AUF$O_StOr),]$O_StOr <- 1

AUF[is.na(AUF$F_StSp),]$F_StSp <- 0
AUF[is.na(AUF$O_StSp),]$O_StSp <- 0

AUF[is.na(AUF$F_StSw),]$F_StSw <- 0
AUF[is.na(AUF$O_StSw),]$O_StSw <- 0

table(c(AUF$F_StOr,AUF$O_StOr),useNA="always")
table(c(AUF$F_StSp,AUF$O_StSp),useNA="always")
table(c(AUF$F_StSw,AUF$O_StSw),useNA="always")

# Drop un-needed variables
AUF <- AUF[,!(names(AUF) %in% c("F_Wt","O_Wt","AEG_key"))]

# Look at the 5-number summary of all variables
vars <- names(AUF)[!(names(AUF) %in% c("Fighter","Opponent","Date","Time","F_DOB","O_DOB"))]
cat.vars <- c("Gender","WC","Result")
for(var in vars){
  cat("\nSummary for variable (",var,")\n",sep="")
  if(!(var %in% cat.vars)){ print(round(summary(AUF[,var]),2)) }
  if(var %in% cat.vars){ print(table(AUF[,var],useNA="always")) } }

windows(10,10)
ggplot(AUF) + geom_bar(aes(x=Gender))
ggplot(AUF) + geom_bar(aes(x=WC))
ggplot(AUF) + geom_bar(aes(x=Result))

par(mfrow=c(5,5))
vars <- vars[!(vars %in% cat.vars)]
for(var in vars[1:25] ){ hist(AUF[,var],main=var,col="dodgerblue") }
for(var in vars[26:50]){ hist(AUF[,var],main=var,col="dodgerblue") }
for(var in vars[51:75]){ hist(AUF[,var],main=var,col="dodgerblue") }
for(var in vars[76:100]){ hist(AUF[,var],main=var,col="dodgerblue") }
for(var in vars[101:125]){ hist(AUF[,var],main=var,col="dodgerblue") }
for(var in vars[126:150]){ hist(AUF[,var],main=var,col="dodgerblue") }
for(var in vars[151:175]){ hist(AUF[,var],main=var,col="dodgerblue") }
for(var in vars[176:200]){ hist(AUF[,var],main=var,col="dodgerblue") }
for(var in vars[201:225]){ hist(AUF[,var],main=var,col="dodgerblue") }
for(var in vars[226:234]){ hist(AUF[,var],main=var,col="dodgerblue") }
dev.off()

# Note: Why are there a lot of zeros, like for F_SSAcc? Its because for the fighter's 1st fight in the UFC, a lot of values are set to 0 because they
# haven't had a UFC fight yet to inform these rolling averages

# Save the resulting dataframe
setwd(dir=paste0(base.dir,"Processed data"))
All_UFC_Fights <- AUF
save(All_UFC_Fights, file="4c_All_UFC_Fights.RDATA")
write.csv(All_UFC_Fights, file="4c_All_UFC_Fights.csv",row.names=FALSE)

# Clear the workspace
rm(list = ls()[ !(ls() %in% params.and.funcs) ])

#=========================================================#
# 4d - [Combine data sources and process final dataframe] #
#   Impose the final data filters prior to modeling       #
#=========================================================#

# Read in the data
setwd(dir=paste0(base.dir,"Processed data"))
load(file="4c_All_UFC_Fights.RDATA")
AUF <- All_UFC_Fights
rm(list=ls()[ls() %in% "All_UFC_Fights"])

# Confirm appropriate structure of important variables
AUF$Fighter <- as.vector(AUF$Fighter)
AUF$Opponent <- as.vector(AUF$Opponent)
AUF$Date <- ymd(AUF$Date)

#------------------------------------------------------------------------------------------------------------------------------------------#
# [Filter 0] Remove fights where fighter/opponent missing, or web scrape screwed up one of their names, Ex. Fighter=<U+0410>natoliy Krecu  #
#------------------------------------------------------------------------------------------------------------------------------------------#
temp0 <- AUF[!(is.na(AUF$Fighter)==TRUE | is.na(AUF$Opponent)==TRUE),]
temp0 <- temp0[!grepl(pattern=">",x=temp0$Fighter) & !grepl(pattern="<",x=temp0$Fighter),]

#-----------------------------------------------------------------------------------------------------------------------------------------------#
# [Filter 1] Each fight may appear twice, once where competitor A is the fighter and competitor B is the opponent, and once where competitor A  # 
#            is the fighter and competitor B is the opponent. Use this key to drop fights which don't appear exactly 2 times in the data, once  #
#            as (Fighter=X, Opponent=Y) and once as (Fighter=Y, Opponent=X)                                                                     #
#-----------------------------------------------------------------------------------------------------------------------------------------------#

# Create the fight key
temp0$AEG_key <- 1:dim(temp0)[1]
temp1a <- temp0
temp1b <- temp0[,c("Fighter","Opponent","Date","AEG_key")]
temp1b$fight_key <- NA
prog.bar <- txtProgressBar(min=1,max=dim(temp1b)[1],style=3)
for(i in 1:dim(temp1b)[1]){ # i=2; i=349334
  X <- temp1b[i,]
  temp1b[i,]$fight_key <- gsub(" ","",paste(c(sort(c(X$Fighter,X$Opponent)),as.vector(X$Date)),collapse=""))
  setTxtProgressBar(pb=prog.bar,value=i) }

# Merge in the fight key with the main data
temp1 <- merge(x=temp1a,y=temp1b[,c("AEG_key","fight_key")],by="AEG_key")
temp1 <- temp1[,!(colnames(temp1) %in% "AEG_key")]

# Drop fights which don't appear exactly 2 times in the data, once as (Fighter=X, Opponent=Y) and once as (Fighter=Y, Opponent=X)
temp1c <- data.frame(table(temp1$fight_key))
names(temp1c) <- c("fight_key","Freq")
temp1 <- temp1[!(temp1$fight_key %in% temp1c[temp1c$Freq!=2,]$fight_key),]

#----------------------------------------------------------------------------#
# [Filter 2] Note that we already filtered to only wins/losses in program 4b #
#----------------------------------------------------------------------------#
temp2 <- temp1

#--------------------------------------#
# [Filter 3] Only include Men's fights #
#--------------------------------------#
temp3 <- temp2
temp3 <- temp3[is.na(temp3$Gender)==FALSE & temp3$Gender=="Men",]

#-----------------------------------------------------------#
# Number records at each step of the data filtering process #
#-----------------------------------------------------------#
cat("\nThe number of records prior to filtering is: ",dim(AUF)[1],"\n",sep="")
cat("[Filter 0] Remove fights where fighter/opponent missing, or web scrape screwed up one of their names, Ex. Fighter=<U+0410>natoliy Krecu 
                The number of records is now: ",dim(temp0)[1],"\n",sep="")
cat("[Filter 1] Remove fights which don't appear exactly 2 times in the data, once as (Fighter=X, Opponent=Y) and once as (Fighter=Y, Opponent=X). 
                The number of records is now: ",dim(temp1)[1],"\n",sep="")
cat("[Filter 2] Only inlcude fights ending in wins/losses (this was already filtered out in an earlier program).
                The number of records is now: ",dim(temp2)[1],"\n",sep="")
cat("[Filter 3] Only inlcude Men's fights.
                The number of records is now: ",dim(temp3)[1],"\n",sep="")

# Save the resulting dataframe
setwd(dir=paste0(base.dir,"Processed data"))
All_UFC_Fights <- temp3
save(All_UFC_Fights,file="4d_All_UFC_Fights.RDATA")
write.csv(All_UFC_Fights,file="4d_All_UFC_Fights.csv",row.names=FALSE)

# Clear the workspace
rm(list = ls()[ !(ls() %in% params.and.funcs) ])











