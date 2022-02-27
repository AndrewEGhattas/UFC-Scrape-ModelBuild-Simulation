#####################################################
# Author:  Andrew Ghattas                           #
# Program: UFC_Step1.R                              #
# Purpose: Scrape the fight level data from UFC.com #
#####################################################

# Create working directory where everything will be saved
dir(path=paste0(base.dir,"Processed data"))

#=================================================#
# Scrape UFC.com to get the URLs of every fighter #
#=================================================#

# This is the list of all fighter. We'll scrape this page to get the links to a search corresponding to fighters whose names start with each letter of the
# alphabet. Ex. The page corresponding to all last names starting with "A", then all last names starting with "B",...etc. Why do we do this? Because I can
# only view all fighters at once (on the same page) when I filter to a specific letter
do_it_again <- TRUE
while(do_it_again==TRUE){
  do_it_again <<- FALSE
  tryCatch( 
    { MainPage_points_to <- read_html("http://ufcstats.com/statistics/fighters") %>% html_nodes('div') %>% html_nodes('a') %>% html_attr('href') },
    error=function(x){ 
      do_it_again <<- TRUE
      cat("\n\n"); print("Error statment encountered so I'm sleeping for 10 seconds then trying again"); cat("\n\n")
      Sys.sleep(time=10) } ) }

# Create the full URL list of pages corresponding to each letter
FighterLetter_pages <- paste0("http://ufcstats.com",MainPage_points_to[5:30],"&page=all")

# Store UFC fighter pages here
UFC_Fighter_pages <- c()

# Progress bar
prog.bar <- txtProgressBar(min=1,max=length(FighterLetter_pages),style=3)

# For each page (corresponding to a letter A-Z for last name) read all the fighters URLs
for(i in 1:length(FighterLetter_pages)){
  
  # Read in the HTML data
  do_it_again <- TRUE
  while(do_it_again==TRUE){
    do_it_again <<- FALSE
    tryCatch( 
      { URLs <- unique(read_html(FighterLetter_pages[i]) %>% html_nodes('tr') %>% html_nodes('td') %>% html_nodes('a') %>% html_attr('href')) },
      error=function(x){ 
        do_it_again <<- TRUE
        print("Error statment encountered so I'm sleeping for 10 seconds then trying again")
        Sys.sleep(time=10) } ) }
  
  # Append the letter specific fighter URLs to the master list
  UFC_Fighter_pages <- c(UFC_Fighter_pages, URLs)
  setTxtProgressBar(pb=prog.bar,value=i)
}

# Save the results
setwd(dir=paste0(base.dir,"Processed data"))
save(UFC_Fighter_pages,file="1a_UFC_Fighter_pages.RDATA")
write.csv(UFC_Fighter_pages,file="1a_UFC_Fighter_pages.csv",row.names=FALSE)

# Clear workspace
rm(list = ls()[ !(ls() %in% params.and.funcs) ])

#===================================================================================#
# (1) Scrape the fighter URLs for all available information                         #
# (2) Scrape each of the individual fighter URLs for URLs of their specific fights  #
# (3) Scrape all info. in each fight URL specific to that fighter                   #
#===================================================================================#

# Read in the data
setwd(dir=paste0(base.dir,"Processed data"))
load(file="1a_UFC_Fighter_pages.RDATA")

# Progress bar
prog.bar <- txtProgressBar(min=1,max=length(UFC_Fighter_pages),style=3)

# Store fighter level data here
dir(path=paste0(base.dir,"Processed data/temp"))

# Process each UFC fighter
for(UFC_fighter in UFC_Fighter_pages){ # UFC_fighter="http://ufcstats.com/fighter-details/d967f0128c323de6"; UFC_fighter="http://ufcstats.com/fighter-details/1338e2c7480bdf9e"
  
  # Read in search results HTML data
  #   Sometimes the search_URL is bad, so we'll rerun the webscape X times to confirm that its a real error not a connection error,
  #   if we still hit errors after X times then skip this fighter URL
  WebScrape_attempt_Num <- 0
  do_it_again <- TRUE
  while(do_it_again==TRUE & WebScrape_attempt_Num<=10){
    do_it_again <<- FALSE
    WebScrape_attempt_Num <- WebScrape_attempt_Num + 1
    tryCatch( 
      { UFC_html <- read_html(UFC_fighter) },
      error=function(x){ 
        do_it_again <<- TRUE
        print("Error statment encountered so I'm sleeping for 10 seconds then trying again")
        Sys.sleep(time=10) } ) }
  if(do_it_again==TRUE){ next } # If we attempted scraping this fighter URL X times and still got nothing then its probably a bad URL, so skip
  
  # Read in fighter name
  UFC_name <- UFC_html %>% html_nodes('.b-content__title-highlight') %>% html_text()
  UFC_name <- gsub("\n","",UFC_name)
  UFC_name <- trimws(UFC_name,which="both")
  
  # Read in fighter attributes 
  UFC_attrib <- UFC_html %>% html_nodes('.b-list__box-list-item_type_block') %>% html_text()
  UFC_attrib <- UFC_attrib[1:5]
  
  UFC_attrib[1] <- gsub("\n","",UFC_attrib[1])
  UFC_attrib[1] <- gsub('\"',"",UFC_attrib[1])
  UFC_attrib[1] <- gsub("Height:","",UFC_attrib[1])
  UFC_attrib[1] <- trimws(UFC_attrib[1],which="both")
  height <- UFC_attrib[1]
  
  UFC_attrib[2] <- gsub("\n","",UFC_attrib[2])
  UFC_attrib[2] <- gsub('\"',"",UFC_attrib[2])
  UFC_attrib[2] <- gsub("Weight:","",UFC_attrib[2])
  UFC_attrib[2] <- trimws(UFC_attrib[2],which="both")
  weight <- UFC_attrib[2]
  
  UFC_attrib[3] <- gsub("\n","",UFC_attrib[3])
  UFC_attrib[3] <- gsub('\"',"",UFC_attrib[3])
  UFC_attrib[3] <- gsub("Reach:","",UFC_attrib[3])
  UFC_attrib[3] <- trimws(UFC_attrib[3],which="both")
  reach <- UFC_attrib[3]
  
  UFC_attrib[4] <- gsub("\n","",UFC_attrib[4])
  UFC_attrib[4] <- gsub('\"',"",UFC_attrib[4])
  UFC_attrib[4] <- gsub("STANCE:","",UFC_attrib[4])
  UFC_attrib[4] <- trimws(UFC_attrib[4],which="both")
  stance <- UFC_attrib[4]
  
  UFC_attrib[5] <- gsub("\n","",UFC_attrib[5])
  UFC_attrib[5] <- gsub('\"',"",UFC_attrib[5])
  UFC_attrib[5] <- gsub("DOB:","",UFC_attrib[5])
  UFC_attrib[5] <- trimws(UFC_attrib[5],which="both")
  DOB <- UFC_attrib[5]
  
  # Read in data and extract fight table
  UFC_URL_tables <- UFC_html %>% html_nodes('table') %>% html_table(fill=TRUE)
  UFC_fights <- UFC_URL_tables[[1]]
  
  # If the fighter has 0 fights, the first row will have all missing values, just skip these fighters
  if(dim(UFC_fights)[1]==1){ next }
  
  # The first row is blank, remove it
  UFC_fights <- UFC_fights[-1,]
  
  # If the first record has W/L="next" this is an event in the future, so remove the record
  if(unlist(UFC_fights[,"W/L"])[1]=="next"){ UFC_fights <- UFC_fights[-1,] }
  
  # If removing the upcoming event results in no remaining fights for this fighter, skip this fighter
  if(dim(UFC_fights)[1]==0){ next }
  
  # If the following record has W/L="" this is an event in the future as well, so remove the record, and indicate via flag that this happened (to adjust a secondary issue later)
  flag.2nd.upcoming.fgt <- 0
  if(unlist(UFC_fights[,"W/L"])[1]==""){ 
    UFC_fights <- UFC_fights[-1,] 
    flag.2nd.upcoming.fgt <- 1 }
  
  # If the fighter has an upcoming UFC fight, but no other fights then skip them as well
  if(dim(UFC_fights)[1]==0){ next }
  
  # Rename the Win/loss variable
  colnames(UFC_fights)[colnames(UFC_fights)=="W/L"] <- "Outcome"
  
  # Extract the fighter and opponent names
  colnames(UFC_fights)[colnames(UFC_fights)=="Fighter"] <- "temp"
  UFC_fights$Fighter <- NA
  UFC_fights$Opponent <- NA
  for(i in 1:dim(UFC_fights)[1]){
    first.slashN <- gregexpr(pattern="\n",UFC_fights$temp[i])[[1]][1]
    last.slashN <- gregexpr(pattern="\n",UFC_fights$temp[i])[[1]][length(gregexpr(pattern="\n",UFC_fights$temp[i])[[1]])]
    UFC_fights$Fighter[i] <- trimws(substr(UFC_fights$temp[i],1,first.slashN-1))
    UFC_fights$Opponent[i] <- trimws(x=substr(UFC_fights$temp[i],last.slashN+1,999),which="left") }
  UFC_fights <- UFC_fights[,!(colnames(UFC_fights) %in% "temp")]
  
  # Extract the Date and Event name
  colnames(UFC_fights)[colnames(UFC_fights)=="Event"] <- "temp"
  UFC_fights$Event <- NA
  UFC_fights$Date <- NA
  for(i in 1:dim(UFC_fights)[1]){
    slashN <- gregexpr(pattern="\n",UFC_fights$temp[i])[[1]][1]
    UFC_fights$Event[i] <- trimws(x=substr(UFC_fights$temp[i],1,slashN-1),which="both")
    length.string <- nchar(UFC_fights$temp[i])
    UFC_fights$Date[i] <- substr(UFC_fights$temp[i],length.string-12,length.string) }
  UFC_fights <- UFC_fights[,!(colnames(UFC_fights) %in% "temp")]
  
  # Drop the following variables
  UFC_fights <- UFC_fights[,!(colnames(UFC_fights) %in% c("Kd","Str","Td","Sub","Pass","Method"))]
  
  # Scrape the links to each fight specific page
  fighters_fight_URLs <- UFC_html %>% html_nodes('tr') %>% html_attr('data-link')
  if(flag.2nd.upcoming.fgt==0){ UFC_fights$fight_page <- fighters_fight_URLs[is.na(fighters_fight_URLs)==FALSE]     }
  if(flag.2nd.upcoming.fgt==1){ UFC_fights$fight_page <- fighters_fight_URLs[is.na(fighters_fight_URLs)==FALSE][-1] }
  
  # Append the previously scraped fighter attributes to this dataframe
  UFC_fights$Height <- height
  UFC_fights$Weight <- weight
  UFC_fights$Reach <- reach
  UFC_fights$Stance <- stance
  UFC_fights$DOB <- DOB
  
  # Append the dataframe with the extracted fight details for each of the fighter's fights
  UFC_fights$method <- NA; UFC_fights$timeformat <- NA; UFC_fights$referee <- NA; UFC_fights$details <- NA
  
  UFC_fights$KD <- NA; UFC_fights$SS <- NA; UFC_fights$TD <- NA; UFC_fights$SubAtt <- NA; UFC_fights$Rev <- NA; UFC_fights$Ctrl <- NA
  UFC_fights$RD6_SS <- UFC_fights$RD5_SS <- UFC_fights$RD4_SS <- UFC_fights$RD3_SS <- UFC_fights$RD2_SS <- UFC_fights$RD1_SS <- NA
  UFC_fights$Ground <- UFC_fights$Clinch <- UFC_fights$Distance <- UFC_fights$Leg <- UFC_fights$Body <- UFC_fights$Head <- NA
  
  UFC_fights$Opp_KD <- NA; UFC_fights$Opp_SS <- NA; UFC_fights$Opp_TD <- NA; UFC_fights$Opp_SubAtt <- NA; UFC_fights$Opp_Rev <- NA; UFC_fights$Opp_Ctrl <- NA
  UFC_fights$Opp_RD6_SS <- UFC_fights$Opp_RD5_SS <- UFC_fights$Opp_RD4_SS <- UFC_fights$Opp_RD3_SS <- UFC_fights$Opp_RD2_SS <- UFC_fights$Opp_RD1_SS <- NA
  UFC_fights$Opp_Ground <- UFC_fights$Opp_Clinch <- UFC_fights$Opp_Distance <- UFC_fights$Opp_Leg <- UFC_fights$Opp_Body <- UFC_fights$Opp_Head <- NA
  
  UFC_fights$Fighter <- toupper(UFC_fights$Fighter)
  UFC_fights$Opponent <- toupper(UFC_fights$Opponent)
  
  for(j in 1:dim(UFC_fights)[1]){
    
    # Read in search results HTML data
    #   Sometimes the search_URL is bad, so we'll rerun the webscape X times to confirm that its a real error not a connection error,
    #   if we still hit errors after X times then skip this fighter URL
    WebScrape_attempt_Num <- 0
    do_it_again <- TRUE
    while(do_it_again==TRUE & WebScrape_attempt_Num<=10){
      do_it_again <<- FALSE
      WebScrape_attempt_Num <- WebScrape_attempt_Num + 1
      tryCatch( 
        { fight_HTML <- read_html(UFC_fights[j,]$fight_page) },
        error=function(x){ 
          do_it_again <<- TRUE
          print("Error statment encountered so I'm sleeping for 10 seconds then trying again")
          Sys.sleep(time=10) } ) }
    if(do_it_again==TRUE){ next } # If we attempted scraping this fighter URL X times and still got nothing then its probably a bad URL, so skip

    # Extract and process the header of the fight page
    header <- gsub("[\n]","",fight_HTML %>% html_nodes('.b-fight-details__text') %>% html_text())
    
    method.index.start <- gregexpr(pattern="Method:",header[[1]])[[1]][1]
    round.index.start <- gregexpr(pattern="Round:",header[[1]])[[1]][1]
    time.index.start <- gregexpr(pattern="Time:",header[[1]])[[1]][1]
    timeformat.index.start <- gregexpr(pattern="Time format:",header[[1]])[[1]][1]
    referee.index.start <- gregexpr(pattern="Referee:",header[[1]])[[1]][1]
    
    method <- substr(header[[1]],method.index.start,round.index.start-1)
    method <- trimws(gsub("Method:","",method),which="both")
    UFC_fights[j,]$method <- method
    
    timeformat <- substr(header[[1]],timeformat.index.start,referee.index.start-1)
    timeformat <- trimws(gsub("Time format:","",timeformat),which="both")
    UFC_fights[j,]$timeformat <- timeformat
    
    referee <- substr(header[[1]],referee.index.start,9999)
    referee <- trimws(gsub("Referee:","",referee),which="both")
    UFC_fights[j,]$referee <- referee
    
    details <- header[[2]]
    details <- trimws(gsub("Details:","",details),which="both")
    UFC_fights[j,]$details <- details
    
    # Read in all fight tables
    fight_tables <- fight_HTML %>% html_table()
    
    # If there is no detailed fight data then skip the next entire section
    if(length(fight_tables)>0){
      
      # Round totals
      x <- as.character(fight_tables[[1]])
      x <- data.frame(strsplit(x,split="\n    \n    \n"))
      names(x) <- c("Fighter","KD","SS","SSperc","Totstr","TD","TDperc","SubAtt","Rev","Ctrl")
      x$Fighter <- trimws(as.vector(x$Fighter),which="both")
      x$Fighter <- toupper(x$Fighter)
      x <- x[,names(x) %in% c("Fighter","KD","SS","TD","SubAtt","Rev","Ctrl")]
      
      # This is some logic to properly assign fighter/opponent when one of their names is inconsistent between UFC fighter page and the fight pages
      if( dim(x[x$Fighter==UFC_fights[j,]$Fighter,])[1]==1 ){
        fgt_rd_totals <- x[x$Fighter==UFC_fights[j,]$Fighter,]
        opp_rd_totals <- x[!(x$Fighter==UFC_fights[j,]$Fighter),] }
      if( dim(x[x$Fighter==UFC_fights[j,]$Opponent,])[1]==1 ){
        opp_rd_totals <- x[x$Fighter==UFC_fights[j,]$Opponent,] 
        fgt_rd_totals <- x[!(x$Fighter==UFC_fights[j,]$Opponent),] }
      if( dim(x[x$Fighter==UFC_fights[j,]$Opponent,])[1]==0 & dim(x[x$Fighter==UFC_fights[j,]$Fighter,])[1]==0 ){ next }
      
      UFC_fights[j,]$KD <-     as.vector(fgt_rd_totals$KD)
      UFC_fights[j,]$SS <-     as.vector(fgt_rd_totals$SS)
      UFC_fights[j,]$TD <-     as.vector(fgt_rd_totals$TD)
      UFC_fights[j,]$SubAtt <- as.vector(fgt_rd_totals$SubAtt)
      UFC_fights[j,]$Rev <-    as.vector(fgt_rd_totals$Rev)
      UFC_fights[j,]$Ctrl <-   as.vector(fgt_rd_totals$Ctrl)
      
      UFC_fights[j,]$Opp_KD <-     as.vector(opp_rd_totals$KD)
      UFC_fights[j,]$Opp_SS <-     as.vector(opp_rd_totals$SS)
      UFC_fights[j,]$Opp_TD <-     as.vector(opp_rd_totals$TD)
      UFC_fights[j,]$Opp_SubAtt <- as.vector(opp_rd_totals$SubAtt)
      UFC_fights[j,]$Opp_Rev <-    as.vector(opp_rd_totals$Rev)
      UFC_fights[j,]$Opp_Ctrl <-   as.vector(opp_rd_totals$Ctrl)
      
      # Significant strikes (by round)
      for(i in 1:dim(fight_tables[[2]])[1]){
        x <- as.character(fight_tables[[2]][i,])
        x <- data.frame(strsplit(x,split="\n    \n    \n"))
        names(x) <- c("Fighter","KD","SS","SSperc","Totstr","TD","TDperc","SubAtt","Rev","Ctrl")
        x$Fighter <- trimws(as.vector(x$Fighter),which="both")
        x$Fighter <- toupper(x$Fighter)
        
        # This is some logic to properly assign fighter/opponent when one of their names is inconsistent between UFC fighter page and the fight pages
        if( dim(x[x$Fighter==UFC_fights[j,]$Fighter,])[1]==1 ){
          fgt_rd_totals <- x[x$Fighter==UFC_fights[j,]$Fighter,]
          opp_rd_totals <- x[!(x$Fighter==UFC_fights[j,]$Fighter),] }
        if( dim(x[x$Fighter==UFC_fights[j,]$Opponent,])[1]==1 ){
          opp_rd_totals <- x[x$Fighter==UFC_fights[j,]$Opponent,] 
          fgt_rd_totals <- x[!(x$Fighter==UFC_fights[j,]$Opponent),] }
        if( dim(x[x$Fighter==UFC_fights[j,]$Opponent,])[1]==0 & dim(x[x$Fighter==UFC_fights[j,]$Fighter,])[1]==0 ){ next }
        
        UFC_fights[j,paste0("RD",i,"_SS")] <- as.vector(fgt_rd_totals$SS)
        UFC_fights[j,paste0("Opp_RD",i,"_SS")] <- as.vector(opp_rd_totals$SS) }
      
      # Region of significant strikes
      x <- as.character(fight_tables[[3]])
      x <- data.frame(strsplit(x,split="\n    \n    \n"))
      names(x) <- c("Fighter","SS","SSperc","Head","Body","Leg","Distance","Clinch","Ground")
      x$Fighter <- trimws(as.vector(x$Fighter),which="both")
      x$Fighter <- toupper(x$Fighter)
      
      # This is some logic to properly assign fighter/opponent when one of their names is inconsistent between UFC fighter page and the fight pages
      if( dim(x[x$Fighter==UFC_fights[j,]$Fighter,])[1]==1 ){
        fgt_rd_totals <- x[x$Fighter==UFC_fights[j,]$Fighter,]
        opp_rd_totals <- x[!(x$Fighter==UFC_fights[j,]$Fighter),] }
      if( dim(x[x$Fighter==UFC_fights[j,]$Opponent,])[1]==1 ){
        opp_rd_totals <- x[x$Fighter==UFC_fights[j,]$Opponent,] 
        fgt_rd_totals <- x[!(x$Fighter==UFC_fights[j,]$Opponent),] }
      if( dim(x[x$Fighter==UFC_fights[j,]$Opponent,])[1]==0 & dim(x[x$Fighter==UFC_fights[j,]$Fighter,])[1]==0 ){ next }
      
      UFC_fights[j,]$Head <-     as.vector(fgt_rd_totals$Head)
      UFC_fights[j,]$Body <-     as.vector(fgt_rd_totals$Body)
      UFC_fights[j,]$Leg <-     as.vector(fgt_rd_totals$Leg)
      UFC_fights[j,]$Distance <- as.vector(fgt_rd_totals$Distance)
      UFC_fights[j,]$Clinch <-   as.vector(fgt_rd_totals$Clinch)
      UFC_fights[j,]$Ground <-    as.vector(fgt_rd_totals$Ground)
      
      UFC_fights[j,]$Opp_Head <-     as.vector(opp_rd_totals$Head)
      UFC_fights[j,]$Opp_Body <-     as.vector(opp_rd_totals$Body)
      UFC_fights[j,]$Opp_Leg <-     as.vector(opp_rd_totals$Leg)
      UFC_fights[j,]$Opp_Distance <- as.vector(opp_rd_totals$Distance)
      UFC_fights[j,]$Opp_Clinch <-   as.vector(opp_rd_totals$Clinch)
      UFC_fights[j,]$Opp_Ground <-    as.vector(opp_rd_totals$Ground)
    }
  }
  
  # Output the dataframe to the temporary results directory
  setwd(dir=paste0(base.dir,"Processed data/temp"))
  fighter_name <- UFC_fights[1,]$Fighter
  write.csv(UFC_fights,file=paste0(fighter_name," (i=",which(UFC_Fighter_pages==UFC_fighter),").csv"),row.names=FALSE)
  
  # Update the progress bar
  setTxtProgressBar(pb=prog.bar,value=which(UFC_Fighter_pages==UFC_fighter))
}

# Create a master All_UFC_Fights dataset
setwd(dir=paste0(base.dir,"Processed data/temp"))
All_UFC_Fights <- list()
all_files_in_dir <- list.files(path=paste0(base.dir,"Processed data/temp/"))
prog.bar <- txtProgressBar(min=1,max=length(all_files_in_dir),style=3)

for(file in all_files_in_dir){
  All_UFC_Fights[[which(all_files_in_dir==file)]] <- read.csv(file=file)
  setTxtProgressBar(pb=prog.bar,value=which(all_files_in_dir==file)) }
All_UFC_Fights <- list.stack(All_UFC_Fights)

# Save the results
setwd(dir=paste0(base.dir,"Processed data/"))
save(All_UFC_Fights, file="1b_All_UFC_Fights.RDATA")
write.csv(x=All_UFC_Fights, file="1b_All_UFC_Fights.csv", row.names=FALSE)

# Clear contents of the temporary directory
removeDirectory(path=paste0(base.dir,"Processed data/temp"), recursive=TRUE, mustExist=TRUE)

# Clear workspace
rm(list = ls()[ !(ls() %in% params.and.funcs) ])


