###################################################
# Author:  Andrew Ghattas                         #
# Program: UFC_Step3.R                            #
# Purpose: Scrape the historical fighter rankings #
###################################################

#=========================================================#
# 3a - [Append Historical Rankings]                       #
#   Read in the historical rankings for each weightclass  #
#=========================================================#

# Number pages of rankings that exist
rankings_URL <- "https://www.fightmatrix.com/historical-mma-rankings/generated-historical-rankings/?Issue=1&Division=1"
do_it_again <- TRUE
while(do_it_again==TRUE){
  do_it_again <<- FALSE
  tryCatch( 
    { rankings <- read_html(rankings_URL) %>% html_nodes('table') %>% html_table(fill=TRUE) },
    error=function(x){ 
      do_it_again <<- TRUE
      cat("\n\n"); print("Error statment encountered so I'm sleeping for 10 seconds then trying again"); cat("\n\n")
      Sys.sleep(time=10) } ) }
Dates <- substr(as.character(rankings[[3]])[1],8,99999)
delim.Dates <- gsub(pattern="(.{10})",replace="\\1,",Dates)
split.Dates <- strsplit(x=delim.Dates,split=",")[[1]]
rev.split.Dates <- rev(split.Dates)
num.pages.rankings <- length(rev.split.Dates)

# Suffix of each rankings page (each suffix corresponds to a Men/Women's weightclass)
# Men
# 1=Heavyweight, 2=LightHeavyweight, 3=Middleweight, 4=Welterweight, 5=Lightweight, 6=Featherweight, 7=Bantamweight, 8=Flyweight, 9=Strawweight
# -1=Pound-for-pound, 11=Division dominance
# Women
# 16=Featherweight+, 15=Bantamweight, 14=Flyweight, 13=Strawweight, 12=Atomweight
# -2=Pound-for-pound, 17=Division dominance
Division_suffixes <- c(1:9,12:16)

# Store temporary fighter URLs here
dir(paste0(base.dir,"Processed data/temp"))
setwd(dir=paste0(base.dir,"Processed data/temp"))

# Progress bar
prog.bar <- txtProgressBar(min=1,max=length(Division_suffixes)*num.pages.rankings*10,style=3)

# Read in and save each rankings table | Division=5; Issue=94; page=1
counter <- 0
for(Division in Division_suffixes){   # Number of Male and Female divisions total
  for(Issue in 1:num.pages.rankings){ # Issue 1 is the rankings as of 1/1/1990, Issue 2 is as of 4/1/1990, ...etc.
    for(page in 1:10){                # Page 1 is ranks 1-25, page 2 is 26-50, ...etc. (it only goes up to rank 250 which corresponds to page 10)
      
      # Update counter
      counter <- counter + 1
      
      # Read in URL table
      rankings_URL <- paste0("https://www.fightmatrix.com/historical-mma-rankings/generated-historical-rankings/?Issue=",Issue,"&Division=",Division,"&Page=",page)
      do_it_again <- TRUE
      while(do_it_again==TRUE){
        do_it_again <<- FALSE
        tryCatch( 
          { rankings <- read_html(rankings_URL) %>% html_nodes('table') %>% html_table(fill=TRUE) },
          error=function(x){ 
            do_it_again <<- TRUE
            print("Error statment encountered so I'm sleeping for 10 seconds then trying again")
            Sys.sleep(time=10) } ) }
      
      # Convert HTML data to dataframe
      rankings <- data.frame(rankings[[4]])
      
      # Remove some columns
      rankings <- rankings[,-c(2,4,5)]
      
      # The first row is actually the header
      colnames(rankings) <- rankings[1,]
      rankings <- rankings[-1,]
      
      # Skip pages which don't actually have any rankings listed
      if(dim(rankings)[1]==0){ 
        setTxtProgressBar(pb=prog.bar,value=counter)
        next }
      
      # Add weightclass to dataframe
      WeightClasses <- c("Heavyweight (Men)","LightHeavyweight (Men)","Middleweight (Men)","Welterweight (Men)",
                         "Lightweight (Men)","Featherweight (Men)","Bantamweight (Men)","Flyweight (Men)","Strawweight (Men)",
                         "Atomweight (Women)","Strawweight (Women)","Flyweight (Women)","Bantamweight (Women)","Featherweight (Women)")
      rankings$WeightClass <- WeightClasses[which(Division_suffixes==Division)]
      
      # Add date to dataframe
      rankings$Date <- rev.split.Dates[Issue]
      
      # Append all fighter data to the master dataframe
      write.csv(x=rankings, file=paste0("rankings_",counter,".csv"), row.names=FALSE)
      
      # Output Progress Bar
      setTxtProgressBar(pb=prog.bar,value=counter)
    }
  }
}

# Create a master dataset appending all the .csv files
setwd(dir=paste0(base.dir,"Processed data/temp"))

all_files_in_dir <- list.files(path=paste0(base.dir,"Processed data/temp"))
prog.bar <- txtProgressBar(min=1,max=length(all_files_in_dir),style=3)

All_Rankings <- NULL
for(file in all_files_in_dir){
  All_Rankings[[which(all_files_in_dir==file)]] <- read.csv(file=file)
  setTxtProgressBar(pb=prog.bar,value=which(all_files_in_dir==file)) }
All_Rankings <- list.stack(All_Rankings)

# Sort the dataframe
All_Rankings$Date <- mdy(All_Rankings$Date)
All_Rankings <- All_Rankings[order(All_Rankings$WeightClass,All_Rankings$Date),]

# Capitalize fighter names for merging purposes later on
All_Rankings$Fighter <- toupper(All_Rankings$Fighter)

# Save the resulting dataframes
setwd(dir=paste0(base.dir,"Processed data"))
save(All_Rankings, file="3a_All_Rankings.RDATA")
write.csv(All_Rankings, file="3a_All_Rankings.csv",row.names=FALSE)

# Clear contents of the temporary directory
removeDirectory(path=paste0(base.dir,"Processed data/temp"), recursive=TRUE, mustExist=TRUE)

# Clear workspace
rm(list = ls()[ !(ls() %in% params.and.funcs) ])

#===================================#
# 3b - [Append Historical Rankings] #
#   Process dataframe               #
#===================================#

# Load data
setwd(dir=paste0(base.dir,"Processed data"))
load(file="3a_All_Rankings.RDATA")

# Create a Men/Women indicator (the initial model will just be built on Men)
All_Rankings$Gender <- NA
All_Rankings[grep(pattern="(Men)",x=All_Rankings$WeightClass),]$Gender <- "Male"
All_Rankings[grep(pattern="(Women)",x=All_Rankings$WeightClass),]$Gender <- "Women"
table(All_Rankings$Gender,useNA="always")

# Make the rank variable numeric
All_Rankings$Rank <- as.numeric(All_Rankings$Rank)

# Change some of the names so that they align with Sherdog naming when we merge on fighter name in the future
All_Rankings$Fighter <- as.vector(All_Rankings$Fighter)
All_Rankings[All_Rankings$Fighter=="BEN HENDERSON",]$Fighter <- "BENSON HENDERSON"
All_Rankings[All_Rankings$Fighter=="SEAN OCONNELL",]$Fighter <- "SEAN O'CONNELL"
All_Rankings[All_Rankings$Fighter=="SEAN OMALLEY",]$Fighter <- "SEAN O'MALLEY"

# Save the final dataframe
Final_Rankings <- All_Rankings
setwd(dir=paste0(base.dir,"Processed data"))
save(Final_Rankings, file="3b_Final_Rankings.RDATA")
write.csv(Final_Rankings, file="3b_Final_Rankings.csv", row.names=FALSE)

# Clear workspace
rm(list = ls()[ !(ls() %in% params.and.funcs) ])

#================================================#
# 3c - [Append Historical Rankings]              #
#   Merge historical rankings data to fight data #
#================================================#

# Load data
setwd(dir=paste0(base.dir,"Processed data"))
load(file="2d_All_UFC_Fights.RDATA")
load(file="3b_Final_Rankings.RDATA")

AUF <- All_UFC_Fights
rm(list="All_UFC_Fights")

# Based on the fight date, create a truncated version of date corresponding to months 1,4,7,9 which is the only dates in the rankings data because
# this is when their rankings get updated. We'll use this later to do a merge() to the rankings data on this truncated date
AUF$Trunc_date_for_Rankings_match <- ymd(NA)
rankings_dates <- sort(unique(Final_Rankings$Date))
for(i in 1:length(rankings_dates)){
  # Most recent rankings (if there are any fights in this time range)
  if(i==length(rankings_dates)){
    if(length(AUF[AUF$Date >= rankings_dates[i],]$Trunc_date_for_Rankings_match)>0){
      AUF[AUF$Date >= rankings_dates[i],]$Trunc_date_for_Rankings_match <- rankings_dates[i] } }
  # All other rankings (if there are any fights in this time range)
  if( is.na(rankings_dates[i+1])==FALSE){
    if(length(AUF[AUF$Date >= rankings_dates[i] & AUF$Date < rankings_dates[i+1],]$Trunc_date_for_Rankings_match)>0){
      AUF[AUF$Date >= rankings_dates[i] & AUF$Date < rankings_dates[i+1],]$Trunc_date_for_Rankings_match <- as.character(rankings_dates[i]) } } }

# Merge the rankings data to fight level data to pick up the Fighter's ranking. Note that there can be dupes in these matches if a given fighter or
# opponent is ranked in 2 divisions at the same time, we'll handle these instances later
Final_Rankings$Date <- ymd(Final_Rankings$Date)
AUF$Trunc_date_for_Rankings_match <- ymd(AUF$Trunc_date_for_Rankings_match)

AUF$AEG_key <- 1:dim(AUF)[1]

x1 <- merge(x=AUF, y=Final_Rankings, by.x=c("Fighter","Trunc_date_for_Rankings_match"), by.y=c("Fighter","Date"), all.x=TRUE)
colnames(x1)[colnames(x1) %in% c("Rank","WeightClass","Gender")] <- c("F_Rank","F_WC","F_Gender")

x2 <- merge(x=x1, y=Final_Rankings, by.x=c("Opponent","Trunc_date_for_Rankings_match"), by.y=c("Fighter","Date"), all.x=TRUE)
colnames(x2)[colnames(x2) %in% c("Rank","WeightClass","Gender")] <- c("O_Rank","O_WC","O_Gender")

# Note that there can be dupes in these matches if a given fighter or opponent is ranked in 2 divisions at the same time. When this happens, there
# will be 2 records for a given fight, one with (Fighter, Opponent with rank in WC1) and a second (Fighter, Opponent with rank in WC2). We'll 
# collapse these records by just keeping the record where Fighter rank WC = Opponent rank WC
dupe_AEG_keys <- as.numeric(names(table(x2$AEG_key)[table(x2$AEG_key)>1]))
x2$flag_for_del <- 0
for(dup in dupe_AEG_keys){
  #x2[x2$AEG_key==dup,]
  # If there is not enough fighter or opponent WC data then we can't be sure which record to keep, so just drop the 1st
  if(length(x2[x2$AEG_key==dup & x2$F_WC!=x2$O_WC,]$flag_for_del)>1){
    x2[x2$AEG_key==dup,][1,]$flag_for_del <- 1 
    next }
  # If there is enough fighter or opponent WC data then keep the record with matching fighter/opponent WC
  x2[x2$AEG_key==dup & x2$F_WC!=x2$O_WC,]$flag_for_del <- 1 }

# Remove the flagged records
x2 <- x2[x2$flag_for_del==0,]

# Remove interim variables
x2 <- x2[,!(colnames(x2) %in% c("Trunc_date_for_Rankings_match","AEG_key","flag_for_del"))]

# Save the results
All_UFC_Fights <- x2
setwd(dir=paste0(base.dir,"Processed data"))
save(All_UFC_Fights, file="3c_All_UFC_Fights.RDATA")
write.csv(x=All_UFC_Fights, file="3c_All_UFC_Fights.csv", row.names=FALSE)

# Clear the workspace
rm(list = ls()[ !(ls() %in% params.and.funcs) ])

#===================================#
# 3d - [Append Historical Rankings] #
#   Impute missing rankings         #
#===================================#

# Load data
setwd(dir=paste0(base.dir,"Processed data"))
load(file="3c_All_UFC_Fights.RDATA")
AUF <- All_UFC_Fights
rm(list=ls()[ls() %in% "All_UFC_Fights"])

# If rank is missing then generally its because they are not ranked. In those cases set Rank=251. However, some cases they switched around weightclasses and weren't listed in
# the rankings for 1 issue and it causes them to have no matching record when I matched to the rankings data. In this case, look at their previous fight and take that ranking
# as the current ranking (if they were in fact ranked in the previous fight). Why do we need to do this now instead of later in our program solely used for imputing? It is
# because our future feature engineering will use ranking and if rank is missing then it screws those variables up.
table(c(AUF$F_Rank,AUF$O_Rank),useNA="always")

AUF$Date <- ymd(AUF$Date)
AUF <- AUF[order(AUF$Fighter,AUF$Date),]

prog.bar <- txtProgressBar(min=1,max=length(unique(AUF$Fighter)),style=3)
for(fighter in unique(AUF$Fighter)){ # fighter="KHABIB NURMAGOMEDOV"; fighter="DANIEL CORMIER"
  # Iterate through each of the fighters fights
  N_Fgts <- dim(AUF[AUF$Fighter==fighter,])[1]
  for(i in 1:N_Fgts){
    # Subset to just the fighter specific data
    x <- AUF[AUF$Fighter==fighter,c("Fighter","Opponent","Date","F_Rank","O_Rank")]
    x <- x[order(x$Date),]
    # If they have no missing ranking data for this fight then move on to the next fight
    if(is.na(x[i,]$F_Rank)==FALSE){ next }
    # If they have missing ranking data for this fight, but its their 1st listed fight then assign them Rank=251
    f <- x[i,]$Fighter
    o <- x[i,]$Opponent
    if(is.na(x[i,]$F_Rank)==TRUE & i==1){
      AUF[AUF$Fighter==f & AUF$Opponent==o,]$F_Rank <- 250
      AUF[AUF$Fighter==o & AUF$Opponent==f,]$O_Rank <- 250 }
    # If they have missing ranking data for this fight, and its not their 1st listed fight use their ranking from their previous fight
    if(is.na(x[i,]$F_Rank)==TRUE & i>1){
      prev_rank <- x[i-1,]$F_Rank
      AUF[AUF$Fighter==f & AUF$Opponent==o,]$F_Rank <- prev_rank
      AUF[AUF$Fighter==o & AUF$Opponent==f,]$O_Rank <- prev_rank } }
  # Set progress bar
  setTxtProgressBar(pb=prog.bar,value=which(fighter==unique(AUF$Fighter)))
}

table(c(AUF$F_Rank,AUF$O_Rank),useNA="always")

# Save the results
All_UFC_Fights <- AUF
setwd(dir=paste0(base.dir,"Processed data"))
save(All_UFC_Fights, file="3d_All_UFC_Fights.RDATA")
write.csv(x=All_UFC_Fights, file="3d_All_UFC_Fights.csv", row.names=FALSE)

# Clear the workspace
rm(list = ls()[ !(ls() %in% params.and.funcs) ])


