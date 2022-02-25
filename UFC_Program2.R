###############################################
# Author:  Andrew Ghattas                     #
# Program: UFC_Step2.R                        #
# Purpose: Scrape the historical betting odds #
###############################################

#===================================================================================================#
# 2a - [Append Historical betting odds]                                                             #
#         Implement a search for each fighter (on bestfightodds) and collect associated odds pages  #
#===================================================================================================#

# Read in all the fighter names corresponding to any fighter found on Sherdog
setwd(dir=paste0(base.dir,"Processed data/"))
load(file="1b_All_UFC_Fights.RDATA")

# Store temporary fighter URLs here
dir(paste0(base.dir,"Processed data/temp"))
setwd(dir=paste0(base.dir,"Processed data/temp"))

# Progress bar
All_fighters <- as.vector(unique(All_UFC_Fights$Fighter))
prog.bar <- txtProgressBar(min=1,max=length(All_fighters),style=3)

# For each fighter name, conduct a search for their name and extract any websites corresponding betting odds for past events
for(FighterName in All_fighters){ # FighterName=All_fighters[159]; All_fighters[137540:length(All_fighters)]
  
  # Construct the URL name which corresponds to the search results when searching for the specific fighter
  search_URL <- paste0("https://www.bestfightodds.com/search?query=",gsub(" ","+",FighterName))
  
  # Sometimes the search_URL we created is bad, having weird characters in it because of weird fighter names, and things can fail, so we'll rerun the
  # webscape X times to confirm that its a real error not a connection error, if we still hit errors after X times then skip this fighter
  WebScrape_attempt_Num <- 0
  
  # Read in search results HTML data
  do_it_again <- TRUE
  while(do_it_again==TRUE & WebScrape_attempt_Num<=10){
    do_it_again <<- FALSE
    WebScrape_attempt_Num <- WebScrape_attempt_Num + 1
    tryCatch( 
      { fighter_search_HTML <- read_html(search_URL) },
      error=function(x){ 
        do_it_again <<- TRUE
        print("Error statment encountered so I'm sleeping for 10 seconds then trying again")
        Sys.sleep(time=10) } ) }
  
  # If we attempted scraping this fighter URL X times and still got nothing then its probably a bad URL, so skip scraping this URL
  if(do_it_again==TRUE){ next }
  
  # Sometimes when we search for a fighter name, it takes you directly to the fighter's odds page. We need to handle these differently or else the program
  # will break. In this case, we are already on the fighter odds page so we can grab all the links, append them to the master dataset and move on. We identify
  # these URLs because in the normal case there is no "#team-name" heading to pull when it doesn't take you right to the fighters odds page
  if(length(fighter_search_HTML %>% html_nodes('#team-name') %>% html_text())==1){
    # Extract the list of events which the specified fighter was a part of
    #   - if there aren't any odds/events listed then skip this fighter
    Event_odds_table_URL_suffixes <- fighter_search_HTML %>% html_nodes('td') %>% html_nodes('a') %>% html_attr('href')
    if(length(Event_odds_table_URL_suffixes)==0){ next }
    Event_odds_table_URL_suffixes <- Event_odds_table_URL_suffixes[!(Event_odds_table_URL_suffixes %in% "/events/future-events-197")]
    Event_odds_table_URLs <- paste0("https://www.bestfightodds.com",Event_odds_table_URL_suffixes)
    Event_odds_table_URLs <- Event_odds_table_URLs[seq(1,length(Event_odds_table_URLs),2)]
    
    # Extract the date of the events throught the following steps
    #   - Identify the nodes corresponding to events
    #   - Extract the corresponding event labels
    event_indicies <- which((fighter_search_HTML %>% html_nodes('tr') %>% html_attr('class'))=="event-header item-mobile-only-row")
    event_labels <- (fighter_search_HTML %>% html_nodes('tr') %>% html_text())[event_indicies]
    event_labels <- trimws(event_labels,which="both")
    event_labels <- event_labels[!(event_labels %in% "Future Events")]
    
    # Combine the event URLs with event date
    scraped_odds_event_URLs <- data.frame(Event=trimws(substr(event_labels,1,nchar(event_labels)-13),which="both"),
                                          Date=trimws(substr(event_labels,nchar(event_labels)-12,999),which="both"),
                                          Event_URL=Event_odds_table_URLs)
    
    # Write the results to temporary directory
    write.csv(x=scraped_odds_event_URLs, file=paste0("scraped_odds_event_URLs_",which(All_fighters==FighterName),".csv"), row.names=FALSE)
    
    # Output Progress Bar
    setTxtProgressBar(pb=prog.bar,value=which(All_fighters==FighterName))
    
    # Skip the rest of the code in this iteration, we've extracted everything we need to
    next }
  
  # Search result strings, which are either fighters or events
  search_results <- fighter_search_HTML %>% html_nodes('td') %>% html_nodes('a') %>% html_text()
  
  # Extract the index of the search result string which matches our fighter name
  #   - Index this as x[1] in case there are 2 fighters with the same common name, like Matt Brown, then it will only read one of them and the program
  #     won't crash...this happens so infrequently that its not worth providing a fix to capture both pages...furthermore, ultimately we'll be using
  #     the first fighter's URL just to ultimately get the BestFightOdds.com URLs for the cards he is on. So if I don't get a hit, this is fine, we 
  #     are just scraping to get as many event odds pages as we can. Its likely that if we miss someone, the events he is on will be captured by 
  #     scraping another fighter's page.
  if(length(which(search_results==FighterName))==0){ correct_search_result_index <- which(search_results==FighterName) }
  if(length(which(search_results==FighterName))>=1){ correct_search_result_index <- which(search_results==FighterName)[1] }
  
  # Extract the URL corresponding to the specific fighter we're searching for
  fighter_URL_suffix <- (fighter_search_HTML %>% html_nodes('td') %>% html_nodes('a') %>% html_attr('href'))[correct_search_result_index]
  fighter_full_URL <- paste0("https://www.bestfightodds.com",fighter_URL_suffix)
  
  # If we didn't get a hit on matching the fighter name to the search then skip them. Sometimes the names are off by a bit, like I'm searching for "Joe Duffy"
  # but its listed in the fight odds as "Joseph Duffy" so I don't get a hit. This is fine, we are just scraping to get as many event odds pages as we can. Its
  # likely that if we miss someone (like Joe Duffy), the events he is on will be captured by scraping another fighter's page.
  if(length(correct_search_result_index)==0){ next }
  
  # In case something goes wrong and this search has something wrong with it, then things can fail, so we'll rerun the webscape
  # X times to confirm that its a real error not a connection error, if we still hit errors after X times then skip this fighter
  WebScrape_attempt_Num <- 0
  
  # Read in fighter specific odds HTML data
  do_it_again <- TRUE
  while(do_it_again==TRUE & WebScrape_attempt_Num<=10){
    do_it_again <<- FALSE
    WebScrape_attempt_Num <- WebScrape_attempt_Num + 1
    tryCatch( 
      { fighter_odds_HTML <- read_html(fighter_full_URL) },
      error=function(x){ 
        do_it_again <<- TRUE
        print("Error statment encountered so I'm sleeping for 10 seconds then trying again")
        Sys.sleep(time=10) } ) }
  
  # If we attempted scraping this fighter URL X times and still got nothing then its probably a bad URL, so skip scraping this URL
  if(do_it_again==TRUE){ next }
  
  # Extract the list of events which the specified fighter was a part of
  #   - if there aren't any odds/events listed then skip this fighter
  Event_odds_table_URL_suffixes <- fighter_odds_HTML %>% html_nodes('td') %>% html_nodes('a') %>% html_attr('href')
  if(length(Event_odds_table_URL_suffixes)==0){ next }
  Event_odds_table_URL_suffixes <- Event_odds_table_URL_suffixes[!(Event_odds_table_URL_suffixes %in% "/events/future-events-197")]
  Event_odds_table_URLs <- paste0("https://www.bestfightodds.com",Event_odds_table_URL_suffixes)
  Event_odds_table_URLs <- Event_odds_table_URLs[seq(1,length(Event_odds_table_URLs),2)]
  
  # Extract the date of the events throught the following steps
  #   - Identify the nodes corresponding to events
  #   - Extract the corresponding event labels
  event_indicies <- which((fighter_odds_HTML %>% html_nodes('tr') %>% html_attr('class'))=="event-header item-mobile-only-row")
  event_labels <- (fighter_odds_HTML %>% html_nodes('tr') %>% html_text())[event_indicies]
  event_labels <- trimws(event_labels,which="both")
  event_labels <- event_labels[!(event_labels %in% "Future Events")]
  
  # Combine the event URLs with event date
  scraped_odds_event_URLs <- data.frame(Event=trimws(substr(event_labels,1,nchar(event_labels)-13),which="both"),
                                        Date=trimws(substr(event_labels,nchar(event_labels)-12,999),which="both"),
                                        Event_URL=Event_odds_table_URLs)
  
  # Write the results to temporary directory
  write.csv(x=scraped_odds_event_URLs, file=paste0("scraped_odds_event_URLs_",which(All_fighters==FighterName),".csv"), row.names=FALSE)
  
  # Output Progress Bar
  setTxtProgressBar(pb=prog.bar,value=which(All_fighters==FighterName)) }

# Create a master dataset appending all the .csv files
setwd(dir=paste0(base.dir,"Processed data/temp"))

all_files_in_dir <- list.files(path=paste0(base.dir,"Processed data/temp"))
prog.bar <- txtProgressBar(min=1,max=length(all_files_in_dir),style=3)

All_Odds_Events_URLs <- NULL
for(file in all_files_in_dir){
  All_Odds_Events_URLs[[which(all_files_in_dir==file)]] <- read.csv(file=file)
  setTxtProgressBar(pb=prog.bar,value=which(all_files_in_dir==file)) }
All_Odds_Events_URLs <- list.stack(All_Odds_Events_URLs)

# Dedupe the resulting dataframe
All_Odds_Events_URLs <- as.data.frame(group_by(All_Odds_Events_URLs,Event_URL) %>% slice(1))

# Save the resulting dataframes
setwd(dir=paste0(base.dir,"Processed data"))
save(All_Odds_Events_URLs, file="2a_All_Odds_Events_URLs.RDATA")
write.csv(All_Odds_Events_URLs, file="2a_All_Odds_Events_URLs.csv",row.names=FALSE)

# Clear contents of the temporary directory
removeDirectory(path=paste0(base.dir,"Processed data/temp"), recursive=TRUE, mustExist=TRUE)

# Clear workspace
rm(list = ls()[ !(ls() %in% params.and.funcs) ])

#===========================================#
# 2b -[Append Historical betting odds]      #
#       Scrape the odds URLs for each event #
#===========================================#

# Load data
setwd(dir=paste0(base.dir,"Processed data"))
load(file="2a_All_Odds_Events_URLs.RDATA")

# Store temporary fighter URLs here
dir(paste0(base.dir,"Processed data/temp"))
setwd(dir=paste0(base.dir,"Processed data/temp"))

# Progress bar
prog.bar <- txtProgressBar(min=1,max=dim(All_Odds_Events_URLs)[1],style=3)

# Scrape the odds data for each event URL
for(i in 1:dim(All_Odds_Events_URLs)[1]){ # i <- 1
  
  # In case a bad link has somehow made it up to this point, we'll rerun the webscape X times to confirm that its a real error not a 
  # connection error, if we still hit errors after X times then skip this URL
  WebScrape_attempt_Num <- 0
  
  # Read in Event odds HTML data
  do_it_again <- TRUE
  while(do_it_again==TRUE & WebScrape_attempt_Num<=10){
    do_it_again <<- FALSE
    WebScrape_attempt_Num <- WebScrape_attempt_Num + 1
    tryCatch( 
      { Event_odds_URL <- read_html(as.vector(All_Odds_Events_URLs[i,]$Event_URL)) },
      error=function(x){ 
        do_it_again <<- TRUE
        print("Error statment encountered so I'm sleeping for 10 seconds then trying again")
        Sys.sleep(time=10) } ) }
  
  # If we attempted scraping this fighter URL X times and still got nothing then its probably a bad URL, so skip scraping this URL
  if(do_it_again==TRUE){ next }
  
  # Extract the URL tables (skip to next link if there's nothing to extract)
  URL_tables <- Event_odds_URL %>% html_nodes('table') %>% html_table(fill=TRUE)
  if(length(URL_tables)==0){ next }
  
  # Extract the table of interest (if it has 1 row then there was no betting lines available for this event, skip it)
  URL_tables <- URL_tables[[2]]
  if(dim(URL_tables)[1]==1){ next }
  
  # Only keep rows which correspond to win/loss bets (which are the records beginning with a fighters name)
  fighters_on_card <- unique(Event_odds_URL %>% html_nodes('th') %>% html_nodes('a') %>% html_nodes('span') %>% html_text())
  names(URL_tables)[1] <- "Fighter"
  URL_tables <- URL_tables[URL_tables$Fighter %in% fighters_on_card,]
  
  # Use the Bovada and SportBet odds because they go back far in time
  URL_tables <- URL_tables[,!(names(URL_tables) %in% c("TheGreek","Props"))]
  
  # Clean up the odds by removing special characters
  for(j in 2:dim(URL_tables)[2]){ URL_tables[,j] <- as.numeric(substr(unlist(URL_tables[,j]),1,nchar(unlist(URL_tables[,j]))-1)) }
  
  # Make 1 row per fight
  Fighter1 <- URL_tables[seq(1,dim(URL_tables)[1],2),]
  names(Fighter1) <- paste0(names(Fighter1),"_1")
  Fighter1$key <- c(1:dim(Fighter1)[1])
  
  Fighter2 <- URL_tables[seq(2,dim(URL_tables)[1],2),]
  names(Fighter2) <- paste0(names(Fighter2),"_2")
  Fighter2$key <- c(1:dim(Fighter2)[1])
  
  odds_table2 <- merge(x=Fighter1,y=Fighter2,by="key")
  odds_table2 <- odds_table2[,!(names(odds_table2) %in% "key")]
  
  # Append date and event to the event specific odds table
  odds_table2$Date <- as.vector(All_Odds_Events_URLs[i,]$Date)
  odds_table2$Event <- as.vector(All_Odds_Events_URLs[i,]$Event)

  # Write the results to temporary directory
  write.csv(x=odds_table2, file=paste0("odds_table2_",i,".csv"), row.names=FALSE)
  
  # Output Progress Bar
  setTxtProgressBar(pb=prog.bar,value=i) }

# Create a master dataset appending all the .csv files
setwd(dir=paste0(base.dir,"Processed data/temp"))

all_files_in_dir <- list.files(path=paste0(base.dir,"Processed data/temp"))
prog.bar <- txtProgressBar(min=1,max=length(all_files_in_dir),style=3)

All_Odds <- NULL
for(file in all_files_in_dir){
  All_Odds[[which(all_files_in_dir==file)]] <- read.csv(file=file)
  setTxtProgressBar(pb=prog.bar,value=which(all_files_in_dir==file)) }
All_Odds <- list.stack(All_Odds, fill=TRUE)

# Save the results
setwd(dir=paste0(base.dir,"Processed data"))
save(All_Odds, file="2b_All_Odds.RDATA")
write.csv(x=All_Odds, file="2b_All_Odds.csv", row.names=FALSE)

# Clear contents of the temporary directory
removeDirectory(path=paste0(base.dir,"Processed data/temp"), recursive=TRUE, mustExist=TRUE)

# Clear workspace
rm(list = ls()[ !(ls() %in% params.and.funcs) ])

#===============================================#
# 2c - [Append Historical Betting Odds]         #
#   Process dataframe (keep just 1 odds value)  #
#===============================================#

# Read in the "All_Odds" data
load(file="2b_All_Odds.RDATA")

# Lubridate the dates
All_Odds$Date <- mdy(All_Odds$Date)

# Only keep 1 measure of odds, if its missing grab another one
All_Odds$Odds_1 <- All_Odds$X5D_1
All_Odds[is.na(All_Odds$Odds_1),]$Odds_1 <- All_Odds[is.na(All_Odds$Odds_1),]$Ref_1
All_Odds[is.na(All_Odds$Odds_1),]$Odds_1 <- All_Odds[is.na(All_Odds$Odds_1),]$Bet365_1

All_Odds$Odds_2 <- All_Odds$X5D_2
All_Odds[is.na(All_Odds$Odds_2),]$Odds_2 <- All_Odds[is.na(All_Odds$Odds_2),]$Ref_2
All_Odds[is.na(All_Odds$Odds_2),]$Odds_2 <- All_Odds[is.na(All_Odds$Odds_2),]$Bet365_2

# Drop a lot of the variables
All_Odds <- All_Odds[,names(All_Odds) %in% c("Date","Fighter_1","Fighter_2","Odds_1","Odds_2")]

# Save the results
Final_Odds <- All_Odds
setwd(dir=paste0(base.dir,"Processed data"))
save(Final_Odds, file="2c_Final_Odds.RDATA")
write.csv(x=Final_Odds, file="2c_Final_Odds.csv", row.names=FALSE)

# Clear the workspace
rm(list = ls()[ !(ls() %in% params.and.funcs) ])

#=====================================================#
# 2d - [Append Historical Betting Odds]               #
#   Merge historical betting odds data to fight data  #
#=====================================================#

# Read in the data
setwd(dir=paste0(base.dir,"Processed data"))
load(file="1b_All_UFC_Fights.RDATA")
load(file="2c_Final_Odds.RDATA")

# Progress bar
prog.bar <- txtProgressBar(min=1,max=dim(All_UFC_Fights)[1],style=3)

# Prepare the odds data to be merged to the fights data. Each fight appears twice in the fight dataset, once as (Fighter,Opponent) and another time
# as (Opponent,Fighter) so I need to create two odds records for each fight (simply reversed) so that we have a proper match
Final_Odds$Fighter_1 <- toupper(Final_Odds$Fighter_1)
Final_Odds$Fighter_2 <- toupper(Final_Odds$Fighter_2)

colnames(Final_Odds)[colnames(Final_Odds)=="Fighter_1"] <- "Fighter"
colnames(Final_Odds)[colnames(Final_Odds)=="Fighter_2"] <- "Opponent"
colnames(Final_Odds)[colnames(Final_Odds)=="Odds_1"] <- "F_odds"
colnames(Final_Odds)[colnames(Final_Odds)=="Odds_2"] <- "O_odds"

rev_Final_Odds <- Final_Odds
rev_Final_Odds <- rev_Final_Odds[,c("Opponent","Fighter","Date","O_odds","F_odds")]
colnames(rev_Final_Odds) <- c("Fighter","Opponent","Date","F_odds","O_odds")

new_Final_Odds <- rbind(Final_Odds,rev_Final_Odds)

# Merge the odds data with the fight level data
All_UFC_Fights$Fighter <- as.vector(All_UFC_Fights$Fighter)
All_UFC_Fights$Opponent <- as.vector(All_UFC_Fights$Opponent)
All_UFC_Fights$Date <- mdy(All_UFC_Fights$Date)
x <- merge(x=All_UFC_Fights, y=new_Final_Odds, by=c("Fighter","Opponent","Date"), all.x=TRUE)

# Save the results
All_UFC_Fights <- x
setwd(dir=paste0(base.dir,"Processed data"))
save(All_UFC_Fights, file="2d_All_UFC_Fights.RDATA")
write.csv(x=All_UFC_Fights, file="2d_All_UFC_Fights.csv", row.names=FALSE)

# Clear the workspace
rm(list = ls()[ !(ls() %in% params.and.funcs) ])

