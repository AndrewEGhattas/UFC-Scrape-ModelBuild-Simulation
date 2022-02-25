####################################
# UFC-Scrape-ModelBuild-Simulation #
####################################
A process to scrape UFC data, feature engineer, build models to predict fight outcomes, and simulate multiple betting strategies using historical betting odds.

####################
# High-level notes #
####################
1. Note that, if you are just interested in the resulting data, and don't want to run any programs. The final dataset "5a_AUF.csv" has been uploaded to GitHub. Keep in mind that this file is current up until 2/24/2022; fights which
   occur after this time will not be in "5a_AUF.csv", you'll need to run the programs to generate the most up to date dataset. Alternatively, if you do run the programs, then you can read in  5a_DataPlusHelperVars.R, and use the AUF
   dataframe.
2. For a lot of the results, this code writes out .R files as well as .csv. This is for bug-checking so that .csv can be convenienly opened/visually inspected when program changes need to be made.
3. The scraping process could probably be "optimized", but speed is not always a priority in scraping, so as to not overload a given site with requests.
4. I have code which implements the betting strategy, printing out radar plots, and expected gain graphs. However, I can't find a consistently profitable betting strategy, so this output is not currently included in the committed 
   set of programs
5. I'm new to web scraping, so these programs may be inefficient in their approaches, but they get the job done.
6. As the websites change, the web scraping code will also need to be updated. So far, the websites have stayed relatively fixed and minimal changes have been needed.
7. Note that when scoring a fight containing (fighter1,fighter2), we will first score the record where (fighter=fighter1,opponent=fighter2), then (fighter=fighter2,opponent=fighter1), then average the predictions

##################################
# Description of Program Outputs #
##################################
1. <ROOT>/H2O Models/
	a. This directory contains the H2O output models, that will be called upon to build the betting strategies within the program
2. <ROOT>/HypParam Plots/
	a. For each model type fit (GBM, GLM, NBC, NN, RF), there are multiple hyperparameters
	b. The plots in this directory summarize the distribution of validation set logloss, for each level of the tested hyperparameters
	c. The triangle points, and line in the figures display the mean validation logloss value
3. <ROOT>/Logs/
	a. The log files for each program called
4. <ROOT>/Model metadata/
	a. This file has an entry for each model, capturing its model type (GBM, GLM, NBC, NN, RF), the hyperparameters used, and the training/validation logloss
5. <ROOT>/Parameters and functions/
	a. Program parameters and utility functions created in the main control program, and called on for subsequent sub-program calls
6. <ROOT>/Processed data/
	a. This directory contains the interim output dataframes for each program
	b. The data is stored as .R and .csv. The .R files are actually used throughout the program calls, whereas the .csv files are useful for opening and visually inspecting in order to bug-check
	c. Note that, if you want to build your own models, using the final dataframe, but not the modeling framework proposed in these programs, then you can read in 5a_DataPlusHelperVars.R, and use the AUF dataframe, or equivalently,
	   read in 5a_AUF.csv
7. <ROOT>/Strategy graphics/
	a. These graphics are the results of back-testing different betting strategies
	b. First off, different models are used in each of these strategies,
		i. Top<X> denotes the top X models, where "top" is determined by the smallest validation logloss
		ii. The 2nd portion of the graphic titles correspond to which modeling framework is being considered: GBM, GLM, NBC, NN, RF, and "Any" denotes any of these model types
		iii. Ex. Top100_GBM represents the back-tested betting strategy results using an ensemble of the 100 best GBM models
		iv. Ex. Top25_Any represents the back-tested betting strategy results using an ensemble of the 25 best models across all model types (GBM, GLM, NBC, NN, RF)
	c. General description of the betting simulation
		i. Based on model/ensemble prediction, and actual betting odds, an expected return is calculated based on betting on either fighter
		ii. A bet can be accepted if the expected return is over some threshold "Y".
		iii. We will look at various choices for "Y"
	d. The 1st panel of the graphic,
		i. Shows the simulated gain over time, across all the fights that we have the opportunity to bet on
		ii. there isn't a point for each fight, because the betting strategy may not want to bet on the fight
		iii. There are multiple lines, to illustrate betting strategies having different values of "Y"
		iv. Only a subset of all values of "Y" are displayed, those having the highest final return
	e. The 2nd panel of the graphic,
		i. Shows the final simulated return as a function of decision threshold "Y"
		ii. The actual line is shown in red, and a few smoothing lines are overlayed on top of it

####################################################
# The programs in this process are described below #
####################################################
1. UFC_ControlProgram.R
	a. Program description: Main control program that creates program parameters and calls the other programs.
	b. Directories created
		i. Creates directory which will contain program parameters and utility functions
			<ROOT>/Parameters and functions/
		i. Creates directory which will capture the logs for each program
			<ROOT>/Logs/
	c. Outputs
		i Creates an R file containing all program parameters and utility functions: <ROOT>/Parameters and functions/ParamsAndFuncs.R
2. UFC_Program1.R
	a. Program description: Scrape the fight level data from UFC.com
	b. Directories created
		i Creates directory which will contain all output data: <ROOT>/Processed data/
	c. Outputs
		i. Log
			<ROOT>/Logs/UFC_Program1.txt
		ii. List of scraped fighter URLs
			<ROOT>/Processed data/1a_UFC_Fighter_pages.csv
			<ROOT>/Processed data/1a_UFC_Fighter_pages.R
		iii. List of scraped fight stats
			<ROOT>/Processed data/1b_All_UFC_Fights.csv
			<ROOT>/Processed data/1b_All_UFC_Fights.R
3. UFC_Program2.R
	a. Program description: Scrape the historical betting odds
	b. Directories created (temporary)
		i. Creates directory which will contain all temporary output, deleted at end of program
			<ROOT>/Processed data/temp/
	c. Outputs
		i. Log
			<ROOT>/Logs/UFC_Program2.txt
		ii. List of scraped bettings odds URLS
			<ROOT>/Processed data/2a_All_Odds_Events_URLs.csv
			<ROOT>/Processed data/2a_All_Odds_Events_URLs.R
		iii. List of fight level odds, scraped from each URL
			<ROOT>/Processed data/2b_All_Odds.csv
			<ROOT>/Processed data/2b_All_Odds.R
		iv. Processed fight level odds data
			<ROOT>/Processed data/2c_Final_Odds.csv
			<ROOT>/Processed data/2c_Final_Odds.R
		v. Merged odds data to fight level stats
			<ROOT>/Processed data/2d_All_UFC_Fights.csv
			<ROOT>/Processed data/2d_All_UFC_Fights.R
4. UFC_Program3.R
	a. Program description: Scrape the historical fighter rankings
	b. Directories created (temporary)
		i. Creates directory which will contain all temporary output, deleted at end of program
			<ROOT>/Processed data/temp/
	c. Outputs
		i. Log
			<ROOT>/Logs/UFC_Program3.txt
		ii. List of scraped historical rankings over time
			<ROOT>/Processed data/3a_All_Rankings.csv
			<ROOT>/Processed data/3a_All_Rankings.R
		iii. Processed historical rankings dataframe
			<ROOT>/Processed data/3b_Final_Rankings.csv
			<ROOT>/Processed data/3b_Final_Rankings.R
		iv. Merge historical rankings data to fight data
			<ROOT>/Processed data/3c_All_UFC_Fights.csv
			<ROOT>/Processed data/3c_All_UFC_Fights.R
		v. Missing values imputed (when possible)
			<ROOT>/Processed data/3d_All_UFC_Fights.csv
			<ROOT>/Processed data/3d_All_UFC_Fights.R
5. UFC_Program4.R
	a. Program description: Combine the fights, rankings, and odds data then feature engineer/process the dataframe
	b. Directories created (temporary)
		i. Creates directory which will contain all temporary output, deleted at end of program
			<ROOT>/Processed data/temp/
	c. Outputs
		i. Log
			<ROOT>/Logs/UFC_Program4.txt
		ii. Process raw web-scraped data and feature engineer
			<ROOT>/Processed data/4a_All_UFC_Fights.csv
			<ROOT>/Processed data/4a_All_UFC_Fights.R
		iii. Merge master dataset with itself
			<ROOT>/Processed data/4b_All_UFC_Fights.csv
			<ROOT>/Processed data/4b_All_UFC_Fights.R
		iv. Impute/combine/transform/drop some variables
			<ROOT>/Processed data/4c_All_UFC_Fights.csv
			<ROOT>/Processed data/4c_All_UFC_Fights.R
		v. Impose the final data filters prior to modeling
			<ROOT>/Processed data/4d_All_UFC_Fights.csv
			<ROOT>/Processed data/4d_All_UFC_Fights.R
6. UFC_Program5.R
	a. Program description: Fit and use the ML models for simulating the betting strategy. This program can be run, the hyperparameter tuning plots observed, hyperparameter values adjusted, then program rerun. This can be
			        continued until the user feels they've properly explored the hyperparameter space. If you do this, then the program will simply append the new models to existing directories, you won't lose the
			        models that were already fit/saved.
	b. Directories created (temporary)
		i. Creates directory which will contain all temporary output, deleted at end of program
			<ROOT>/Processed data/temp/
	c. Directories created
		i. Creates directory which will contain all H2O model objects: <ROOT>/H2O Models/
		ii. Creates directory which will contain all hyperparameter tuning plots: <ROOT>/HypParam Plots/
		iii. Creates directory which will contain aggregated model metadata and performance: <ROOT>/Model metadata/
		iv. Creates directory which will contain betting simulation results: <ROOT>/Strategy graphics/
	d. Outputs
		i. Log
			<ROOT>/Logs/UFC_Program5.txt
		ii. H2O Models
			<ROOT>/H2O Models/<MODELNAME>
		iii. Hyperparameter tuning plots
			<ROOT>/HypParam Plots/<PLOTNAME>
		iv. Model metadata tables
			<ROOT>/Model metadata/<TABLENAME>
		v. Final dataframe, vector of predictors, and object naming the target variable. Note that, if you want to build your own models, using the final dataframe, but not the modeling framework proposed in these
		   programs, then you can read in 5a_DataPlusHelperVars.R, and use the AUF dataframe, or equivalently, read in 5a_AUF.csv
			<ROOT>/Processed data/5a_DataPlusHelperVars.R
			<ROOT>/Processed data/5a_AUF.csv
		vi. Model metadata read stored as dataframes
			<ROOT>/Processed data/5c_ModelMetadata.R
		vii. Betting strategy graphics
			<ROOT>/Strategy graphics/<BETTINGSTRAT>

###########################################
# Data Dictionary for final dataframe AUF #
###########################################
Fighter: 	   Name of fighter
Date: 		   Date of fight
Result:		   Result of fight (win/lose)
R:		   Stoppage round (or number rounds fought if no stoppage)
Time:		   Time of stoppage in round (or total time fought if no stoppage)
I_DEC:		   Indicator if fight ended in a decision
I_KOTKO:	   Indicator if fight ended in a KO/TKO
I_SUB:		   Indicator if fight ended in a submission
I5:		   Indicator if fight was 5-rounds
F_DOB:		   Fighter date of birth
F_Age:		   Fighter age at time of fight (in years)
F_Hgt:		   Fighter height (in inches)
F_Rch:		   Fighter reach (in inches)
F_StOr:		   Indicator for fighter stance orthadox
F_StSw:		   Indicator for fighter stance switch
F_StSp:		   Indicator for fighter stance southpaw
F_Rank:		   Fighter rank at time of fight
F_odds:		   Fighter odds (if available) for this fight
F_HghRnkFgtLst3:   Highest ranked opponent that this fighter has fought in the last 3 fights
F_HghRnkWinLst3:   Highest ranked opponent beaten that this fighter has fought in the last 3 fights
F_F:		   Number of fighter's fights (in the UFC), at the time of current fight
F_W:		   Number of fighter's wins (in the UFC), at the time of current fight
F_L:		   Number of fighter's losses (in the UFC), at the time of current fight
F_F5:		   Number of fighter's fights (in the UFC) with opponents ranked 5th in the world or lower, in the previous 5 fights
F_W5:		   Number of fighter's wins (in the UFC) with opponents ranked 5th in the world or lower, in the previous 5 fights
F_L5:		   Number of fighter's loses (in the UFC) with opponents ranked 5th in the world or lower, in the previous 5 fights
F_F10:		   Number of fighter's fights (in the UFC) with opponents ranked 10th in the world or lower, in the previous 5 fights
F_W10:		   Number of fighter's wins (in the UFC) with opponents ranked 10th in the world or lower, in the previous 5 fights
F_L10:		   Number of fighter's losses (in the UFC) with opponents ranked 10th in the world or lower, in the previous 5 fights
F_F20:		   Number of fighter's fights (in the UFC) with opponents ranked 20th in the world or lower, in the previous 5 fights
F_W20: 		   Number of fighter's wins (in the UFC) with opponents ranked 20th in the world or lower, in the previous 5 fights
F_L20:		   Number of fighter's losses (in the UFC) with opponents ranked 20th in the world or lower, in the previous 5 fights
F_F50:		   Number of fighter's fights (in the UFC) with opponents ranked 50th in the world or lower, in the previous 5 fights
F_W50:		   Number of fighter's wins (in the UFC) with opponents ranked 50th in the world or lower, in the previous 5 fights
F_L50:		   Number of fighter's losses (in the UFC) with opponents ranked 50th in the world or lower, in the previous 5 fights
F_F100:		   Number of fighter's fights (in the UFC) with opponents ranked 100th in the world or lower, in the previous 5 fights
F_W100:		   Number of fighter's wins (in the UFC) with opponents ranked 100th in the world or lower, in the previous 5 fights
F_L100:		   Number of fighter's losses (in the UFC) with opponents ranked 100th in the world or lower, in the previous 5 fights
F_KOTKO_W:	   Number of fighter's wins (in the UFC) ending with KO/TKO, in the previous 10 fights
F_SUB_W: 	   Number of fighter's wins (in the UFC) ending with submission, in the previous 10 fights
F_DEC_W:	   Number of fighter's wins (in the UFC) ending with decision, in the previous 10 fights
F_KOTKO_L:	   Number of fighter's losses (in the UFC) ending with KO/TKO, in the previous 10 fights
F_SUB_L:	   Number of fighter's losses (in the UFC) ending with submission, in the previous 10 fights
F_DEC_L:	   Number of fighter's losses (in the UFC) ending with decision, in the previous 10 fights
F_KOTKO_W5:	   Number of fighter's wins (in the UFC) ending with KO/TKO with opponents ranked 5th in the world or lower, in the previous 10 fights
F_SUB_W5:	   Number of fighter's wins (in the UFC) ending with submission with opponents ranked 5th in the world or lower, in the previous 10 fights
F_DEC_W5:	   Number of fighter's wins (in the UFC) ending with decision with opponents ranked 5th in the world or lower, in the previous 10 fights
F_KOTKO_L5:	   Number of fighter's losses (in the UFC) ending with KO/TKO with opponents ranked 5th in the world or lower, in the previous 10 fights
F_SUB_L5:	   Number of fighter's losses (in the UFC) ending with submission with opponents ranked 5th in the world or lower, in the previous 10 fights
F_DEC_L5:	   Number of fighter's losses (in the UFC) ending with decision with opponents ranked 5th in the world or lower, in the previous 10 fights
F_KOTKO_W10:	   Number of fighter's wins (in the UFC) ending with KO/TKO with opponents ranked 10th in the world or lower, in the previous 10 fights
F_SUB_W10:	   Number of fighter's wins (in the UFC) ending with submission with opponents ranked 10th in the world or lower, in the previous 10 fights
F_DEC_W10:	   Number of fighter's wins (in the UFC) ending with decision with opponents ranked 10th in the world or lower, in the previous 10 fights
F_KOTKO_L10:	   Number of fighter's losses (in the UFC) ending with KO/TKO with opponents ranked 10th in the world or lower, in the previous 10 fights
F_SUB_L10:	   Number of fighter's losses (in the UFC) ending with submission with opponents ranked 10th in the world or lower, in the previous 10 fights
F_DEC_L10:	   Number of fighter's losses (in the UFC) ending with decision with opponents ranked 10th in the world or lower, in the previous 10 fights
F_KOTKO_W20:	   Number of fighter's wins (in the UFC) ending with KO/TKO with opponents ranked 20th in the world or lower, in the previous 10 fights
F_SUB_W20:	   Number of fighter's wins (in the UFC) ending with submission with opponents ranked 20th in the world or lower, in the previous 10 fights
F_DEC_W20:	   Number of fighter's wins (in the UFC) ending with decision with opponents ranked 20th in the world or lower, in the previous 10 fights
F_KOTKO_L20:	   Number of fighter's losses (in the UFC) ending with KO/TKO with opponents ranked 20th in the world or lower, in the previous 10 fights
F_SUB_L20:	   Number of fighter's losses (in the UFC) ending with submission with opponents ranked 20th in the world or lower, in the previous 10 fights
F_DEC_L20:	   Number of fighter's losses (in the UFC) ending with decision with opponents ranked 20th in the world or lower, in the previous 10 fights
F_KOTKO_W50:	   Number of fighter's wins (in the UFC) ending with KO/TKO with opponents ranked 50th in the world or lower, in the previous 10 fights
F_SUB_W50:	   Number of fighter's wins (in the UFC) ending with submission with opponents ranked 50th in the world or lower, in the previous 10 fights
F_DEC_W50:	   Number of fighter's wins (in the UFC) ending with decision with opponents ranked 50th in the world or lower, in the previous 10 fights
F_KOTKO_L50:	   Number of fighter's losses (in the UFC) ending with KO/TKO with opponents ranked 50th in the world or lower, in the previous 10 fights
F_SUB_L50:	   Number of fighter's losses (in the UFC) ending with submission with opponents ranked 50th in the world or lower, in the previous 10 fights
F_DEC_L50:	   Number of fighter's losses (in the UFC) ending with decision with opponents ranked 50th in the world or lower, in the previous 10 fights
F_KOTKO_W100:	   Number of fighter's wins (in the UFC) ending with KO/TKO with opponents ranked 100th in the world or lower, in the previous 10 fights
F_SUB_W100:	   Number of fighter's wins (in the UFC) ending with submission with opponents ranked 100th in the world or lower, in the previous 10 fights
F_DEC_W100:	   Number of fighter's wins (in the UFC) ending with decision with opponents ranked 100th in the world or lower, in the previous 10 fights
F_KOTKO_L100:	   Number of fighter's losses (in the UFC) ending with KO/TKO with opponents ranked 100th in the world or lower, in the previous 10 fights
F_SUB_L100:	   Number of fighter's losses (in the UFC) ending with submission with opponents ranked 100th in the world or lower, in the previous 10 fights
F_DEC_L100:	   Number of fighter's losses (in the UFC) ending with decision with opponents ranked 100th in the world or lower, in the previous 10 fights
F_TS_F:		   Time since last fight (in days) for the fighter (for UFC fights), initialized at 365 days if this is the fighter's 1st UFC fight
F_TS_KOTKOl:	   Time since last KO/TKO loss (in days) for the fighter (for UFC fights), initialized at 365 days if this is the fighter's 1st UFC fight
F_WinB3:	   Was a 3-fight (or more) win streak broke in the fighter's last fight (only pertains to UFC fights), excluding no contents/draws
F_WinsP3:	   Fighter number of wins in the last 3 UFC fights (excluding no contents/draws)
F_WinsP5:	   Fighter number of wins in the last 5 UFC fights (excluding no contents/draws)
F_WinsP7:	   Fighter number of wins in the last 7 UFC fights (excluding no contents/draws)
F_WinsP9:	   Fighter number of wins in the last 9 UFC fights (excluding no contents/draws)
F_WinsP11:	   Fighter number of wins in the last 11 UFC fights (excluding no contents/draws)
F_WinsP13:	   Fighter number of wins in the last 13 UFC fights (excluding no contents/draws)
F_WinsP15:	   Fighter number of wins in the last 15 UFC fights (excluding no contents/draws)
F_Fight_Time:	   Fighter cumulative total fight time (in minutes) in UFC fights
F_CtrlSum:	   Fighter total cumulative control time (in minutes) in UFC fights
F_CtrlPerc:	   Fighter cumulative percentage of time spent in control, in UFC fights
F_KD_p5m:	   Fighter cumulative knockdowns per 5 minutes, in previous 5 UFC fights
F_Rev_p5m:	   Fighter cumulative reversals per 5 minutes, in previous 5 UFC fights
F_SA_p5m:	   Fighter cumulative submission attempts per 5 minutes, in previous 5 UFC fights
F_SSL_p5m:	   Fighter cumulative significant strikes landed per 5 minutes, in previous 5 UFC fights
F_SST_p5m:	   Fighter cumulative significant strikes thrown per 5 minutes, in previous 5 UFC fights
F_SSAcc:	   Fighter cumulative significant strikes landed divided by significant strikes thrown, in previous 5 UFC fights
F_TDL_p5m:	   Fighter cumulative take-downs landed per 5 minutes, in previous 5 UFC fights
F_TDT_p5m:	   Fighter cumulative take-downs thrown per 5 minutes, in previous 5 UFC fights
F_TDAcc:	   Fighter cumulative take-downs landed divided by take-downs thrown, in previous 5 UFC fights
F_HeadL_p5m:	   Fighter cumulative head shots landed per 5 minutes, in previous 5 UFC fights
F_BodyL_p5m:	   Fighter cumulative body shots landed per 5 minutes, in previous 5 UFC fights
F_LegL_p5m:	   Fighter cumulative leg shots landed per 5 minutes, in previous 5 UFC fights
F_DistL_p5m:	   Fighter cumulative distance shots landed per 5 minutes, in previous 5 UFC fights
F_ClinchL_p5m:	   Fighter cumulative clinch shots landed per 5 minutes, in previous 5 UFC fights
F_GrdL_p5m:	   Fighter cumulative ground shots landed per 5 minutes, in previous 5 UFC fights
F_SSL_Beta:	   Using previous 10 fights, normalize this fighter's significant strikes landed (by time in case of stoppage) for each round, then estimate slope of line to assess how significant strikes landed changes over round, 
		   on average
F_Opp_KD_p5m:	   Fighter's previous 5 opponents (aggregated together) cumulative knockdowns per 5 minutes against the fighter
F_Opp_Rev_p5m:	   Fighter's previous 5 opponents (aggregated together) cumulative reversals per 5 minutes against the fighter
F_Opp_SA_p5m:      Fighter's previous 5 opponents (aggregated together) cumulative submission attempts per 5 minutes against the fighter
F_Opp_SSL_p5m:	   Fighter's previous 5 opponents (aggregated together) cumulative significant strikes landed per 5 minutes against the fighter
F_Opp_SST_p5m:	   Fighter's previous 5 opponents (aggregated together) cumulative significant strikes thrown per 5 minutes against the fighter
F_Opp_SSAcc:	   Fighter's previous 5 opponents (aggregated together) cumulative significant strike accuracy against the fighter
F_Opp_TDL_p5m:	   Fighter's previous 5 opponents (aggregated together) cumulative take-downs landed per 5 minutes against the fighter
F_Opp_TDT_p5m:	   Fighter's previous 5 opponents (aggregated together) cumulative take-downs thrown per 5 minutes against the fighter
F_Opp_TDAcc:	   Fighter's previous 5 opponents (aggregated together) cumulative take-down accuracy against the fighter
F_Opp_HeadL_p5m:   Fighter's previous 5 opponents (aggregated together) cumulative head shots landed per 5 minutes against the fighter
F_Opp_BodyL_p5m:   Fighter's previous 5 opponents (aggregated together) cumulative body shots landed per 5 minutes against the fighter
F_Opp_LegL_p5m:    Fighter's previous 5 opponents (aggregated together) cumulative leg shots landed per 5 minutes against the fighter
F_Opp_DistL_p5m:   Fighter's previous 5 opponents (aggregated together) cumulative distance shots landed per 5 minutes against the fighter
F_Opp_ClinchL_p5m: Fighter's previous 5 opponents (aggregated together) cumulative clinch shots landed per 5 minutes against the fighter
F_Opp_GrdL_p5m:	   Fighter's previous 5 opponents (aggregated together) cumulative ground shots landed per 5 minutes against the fighter
F_Opp_SSL_Beta:    Using the fighter's previous 10 opponents, normalize opponent significant strikes landed (by time in case of stoppage) for each round, then estimate slope of line to assess how significant strikes landed changes
		   over round, on average
Opponent:	   Name of opponent
O_DOB:		   Opponent date of birth
O_Age:		   Opponent age at time of fight (in years)
O_Hgt:		   Opponent height (in inches)
O_Rch:		   Opponent reach (in inches)
O_StOr:		   Indicator for opponent stance orthadox
O_StSw:		   Indicator for opponent stance switch
O_StSp:		   Indicator for opponent stance southpaw
O_Rank:		   Opponent rank at time of fight
O_odds:		   Opponent odds (if available) for this fight
O_HghRnkFgtLst3:   Highest ranked opponent that this opponent has fought in the last 3 fights
O_HghRnkWinLst3:   Highest ranked opponent beaten that this opponent has fought in the last 3 fights
O_F:		   Number of opponents's fights (in the UFC), at the time of current fight
O_W:  		   Number of opponents's wins (in the UFC), at the time of current fight
O_L:		   Number of opponents's losses (in the UFC), at the time of current fight
O_F5:		   Number of opponents's fights (in the UFC) with their opponents ranked 5th in the world or lower, in the previous 5 fights
O_W5:		   Number of opponents's wins (in the UFC) with their opponents ranked 5th in the world or lower, in the previous 5 fights
O_L5:		   Number of opponents's losses (in the UFC) with their opponents ranked 5th in the world or lower, in the previous 5 fights
O_F10:		   Number of opponents's fights (in the UFC) with their opponents ranked 10th in the world or lower, in the previous 5 fights
O_W10:		   Number of opponents's wins (in the UFC) with their opponents ranked 10th in the world or lower, in the previous 5 fights
O_L10:		   Number of opponents's losses (in the UFC) with their opponents ranked 10th in the world or lower, in the previous 5 fights
O_F20:		   Number of opponents's fights (in the UFC) with their opponents ranked 20th in the world or lower, in the previous 5 fights
O_W20:		   Number of opponents's wins (in the UFC) with their opponents ranked 20th in the world or lower, in the previous 5 fights
O_L20:		   Number of opponents's losses (in the UFC) with their opponents ranked 20th in the world or lower, in the previous 5 fights
O_F50:		   Number of opponents's fights (in the UFC) with their opponents ranked 50th in the world or lower, in the previous 5 fights
O_W50:		   Number of opponents's wins (in the UFC) with their opponents ranked 50th in the world or lower, in the previous 5 fights
O_L50:		   Number of opponents's losses (in the UFC) with their opponents ranked 50th in the world or lower, in the previous 5 fights
O_F100:		   Number of opponents's fights (in the UFC) with their opponents ranked 100th in the world or lower, in the previous 5 fights
O_W100:		   Number of opponents's wins (in the UFC) with their opponents ranked 100th in the world or lower, in the previous 5 fights
O_L100:		   Number of opponents's losses (in the UFC) with their opponents ranked 100th in the world or lower, in the previous 5 fights
O_KOTKO_W:	   Number of opponents's wins (in the UFC) ending with KO/TKO, in the previous 10 fights
O_SUB_W:	   Number of opponents's wins (in the UFC) ending with submission, in the previous 10 fights
O_DEC_W:	   Number of opponents's wins (in the UFC) ending with decision, in the previous 10 fights
O_KOTKO_L:	   Number of opponents's losses (in the UFC) ending with KO/TKO, in the previous 10 fights
O_SUB_L:	   Number of opponents's losses (in the UFC) ending with submission, in the previous 10 fights
O_DEC_L:           Number of opponents's losses (in the UFC) ending with decision, in the previous 10 fights
O_KOTKO_W5:	   Number of opponents's wins (in the UFC) ending with KO/TKO with their opponents ranked 5th in the world or lower, in the previous 10 fights
O_SUB_W5:	   Number of opponents's wins (in the UFC) ending with submission with their opponents ranked 5th in the world or lower, in the previous 10 fights
O_DEC_W5:	   Number of opponents's wins (in the UFC) ending with decision with their opponents ranked 5th in the world or lower, in the previous 10 fights
O_KOTKO_L5:	   Number of opponents's losses (in the UFC) ending with KO/TKO with their opponents ranked 5th in the world or lower, in the previous 10 fights
O_SUB_L5:	   Number of opponents's losses (in the UFC) ending with submission with their opponents ranked 5th in the world or lower, in the previous 10 fights
O_DEC_L5:	   Number of opponents's losses (in the UFC) ending with decision with their opponents ranked 5th in the world or lower, in the previous 10 fights
O_KOTKO_W10:	   Number of opponents's wins (in the UFC) ending with KO/TKO with their opponents ranked 10th in the world or lower, in the previous 10 fights
O_SUB_W10:	   Number of opponents's wins (in the UFC) ending with submission with their opponents ranked 10th in the world or lower, in the previous 10 fights
O_DEC_W10:	   Number of opponents's wins (in the UFC) ending with decision with their opponents ranked 10th in the world or lower, in the previous 10 fights
O_KOTKO_L10:	   Number of opponents's losses (in the UFC) ending with KO/TKO with their opponents ranked 10th in the world or lower, in the previous 10 fights
O_SUB_L10:	   Number of opponents's losses (in the UFC) ending with submission with their opponents ranked 10th in the world or lower, in the previous 10 fights
O_DEC_L10:	   Number of opponents's losses (in the UFC) ending with decision with their opponents ranked 10th in the world or lower, in the previous 10 fights
O_KOTKO_W20:	   Number of opponents's wins (in the UFC) ending with KO/TKO with their opponents ranked 20th in the world or lower, in the previous 10 fights
O_SUB_W20:	   Number of opponents's wins (in the UFC) ending with submission with their opponents ranked 20th in the world or lower, in the previous 10 fights
O_DEC_W20:	   Number of opponents's wins (in the UFC) ending with decision with their opponents ranked 20th in the world or lower, in the previous 10 fights
O_KOTKO_L20:	   Number of opponents's losses (in the UFC) ending with KO/TKO with their opponents ranked 20th in the world or lower, in the previous 10 fights
O_SUB_L20:	   Number of opponents's losses (in the UFC) ending with submission with their opponents ranked 20th in the world or lower, in the previous 10 fights
O_DEC_L20:	   Number of opponents's losses (in the UFC) ending with decision with their opponents ranked 20th in the world or lower, in the previous 10 fights
O_KOTKO_W50:	   Number of opponents's wins (in the UFC) ending with KO/TKO with their opponents ranked 50th in the world or lower, in the previous 10 fights
O_SUB_W50:	   Number of opponents's wins (in the UFC) ending with submission with their opponents ranked 50th in the world or lower, in the previous 10 fights
O_DEC_W50:	   Number of opponents's wins (in the UFC) ending with decision with their opponents ranked 50th in the world or lower, in the previous 10 fights
O_KOTKO_L50:	   Number of opponents's losses (in the UFC) ending with KO/TKO with their opponents ranked 50th in the world or lower, in the previous 10 fights
O_SUB_L50:	   Number of opponents's losses (in the UFC) ending with submission with their opponents ranked 50th in the world or lower, in the previous 10 fights
O_DEC_L50:	   Number of opponents's losses (in the UFC) ending with decision with their opponents ranked 50th in the world or lower, in the previous 10 fights
O_KOTKO_W100:	   Number of opponents's wins (in the UFC) ending with KO/TKO with their opponents ranked 100th in the world or lower, in the previous 10 fights
O_SUB_W100:	   Number of opponents's wins (in the UFC) ending with submission with their opponents ranked 100th in the world or lower, in the previous 10 fights
O_DEC_W100:	   Number of opponents's wins (in the UFC) ending with decision with their opponents ranked 100th in the world or lower, in the previous 10 fights
O_KOTKO_L100:	   Number of opponents's losses (in the UFC) ending with KO/TKO with their opponents ranked 100th in the world or lower, in the previous 10 fights
O_SUB_L100:	   Number of opponents's losses (in the UFC) ending with submission with their opponents ranked 100th in the world or lower, in the previous 10 fights
O_DEC_L100:	   Number of opponents's losses (in the UFC) ending with decision with their opponents ranked 100th in the world or lower, in the previous 10 fights
O_TS_F:		   Time since last fight (in days) for the opponent (for UFC fights), initialized at 365 days if this is the opponents's 1st UFC fight
O_TS_KOTKOl:	   Time since last KO/TKO loss (in days) for the opponent (for UFC fights), initialized at 365 days if this is the opponents's 1st UFC fight
O_WinB3:	   Was a 3-fight (or more) win streak broke in the opponent's last fight (only pertains to UFC fights), excluding no contents/draws
O_WinsP3:	   Opponent number of wins in the last 3 UFC fights (excluding no contents/draws)
O_WinsP5:	   Opponent number of wins in the last 5 UFC fights (excluding no contents/draws)
O_WinsP7:          Opponent number of wins in the last 7 UFC fights (excluding no contents/draws)
O_WinsP9:	   Opponent number of wins in the last 9 UFC fights (excluding no contents/draws)
O_WinsP11:	   Opponent number of wins in the last 11 UFC fights (excluding no contents/draws)
O_WinsP13:	   Opponent number of wins in the last 13 UFC fights (excluding no contents/draws)
O_WinsP15:	   Opponent number of wins in the last 15 UFC fights (excluding no contents/draws)
O_Fight_Time:	   Opponent cumulative total fight time (in minutes) in UFC fights
O_CtrlSum:	   Opponent total cumulative control time (in minutes) in UFC fights
O_CtrlPerc:	   Opponent cumulative percentage of time spent in control, in UFC fights
O_KD_p5m:	   Opponent cumulative knockdowns per 5 minutes, in previous 5 UFC fights
O_Rev_p5m:	   Opponent cumulative reversals per 5 minutes, in previous 5 UFC fights
O_SA_p5m:	   Opponent cumulative submission attempts per 5 minutes, in previous 5 UFC fights
O_SSL_p5m:	   Opponent cumulative significant strikes landed per 5 minutes, in previous 5 UFC fights
O_SST_p5m:	   Opponent cumulative significant strikes thrown per 5 minutes, in previous 5 UFC fights
O_SSAcc:	   Opponent cumulative significant strikes landed divided by significant strikes thrown, in previous 5 UFC fights
O_TDL_p5m:	   Opponent cumulative take-downs landed per 5 minutes, in previous 5 UFC fights
O_TDT_p5m:	   Opponent cumulative take-downs thrown per 5 minutes, in previous 5 UFC fights
O_TDAcc:	   Opponent cumulative take-downs landed divided by take-downs thrown, in previous 5 UFC fights
O_HeadL_p5m:	   Opponent cumulative head shots landed per 5 minutes, in previous 5 UFC fights
O_BodyL_p5m:	   Opponent cumulative body shots landed per 5 minutes, in previous 5 UFC fights
O_LegL_p5m:	   Opponent cumulative leg shots landed per 5 minutes, in previous 5 UFC fights
O_DistL_p5m:	   Opponent cumulative distance shots landed per 5 minutes, in previous 5 UFC fights
O_ClinchL_p5m:	   Opponent cumulative clinch shots landed per 5 minutes, in previous 5 UFC fights
O_GrdL_p5m:	   Opponent cumulative ground shots landed per 5 minutes, in previous 5 UFC fights
O_SSL_Beta:	   Using previous 10 fights, normalize this opponent's significant strikes landed (by time in case of stoppage) for each round, then estimate slope of line to assess how significant strikes landed changes over round, 
		   on average
O_Opp_KD_p5m:	   Opponent's previous 5 opponents (aggregated together) cumulative knockdowns per 5 minutes against the original opponent
O_Opp_Rev_p5m:	   Opponent's previous 5 opponents (aggregated together) cumulative reversals per 5 minutes against the original opponent
O_Opp_SA_p5m:	   Opponent's previous 5 opponents (aggregated together) cumulative submission attempts per 5 minutes against the original opponent
O_Opp_SSL_p5m:	   Opponent's previous 5 opponents (aggregated together) cumulative significant strikes landed per 5 minutes against the original opponent
O_Opp_SST_p5m:	   Opponent's previous 5 opponents (aggregated together) cumulative significant strikes thrown per 5 minutes against the original opponent
O_Opp_SSAcc:	   Opponent's previous 5 opponents (aggregated together) cumulative significant strike accuracy per 5 minutes against the original opponent
O_Opp_TDL_p5m:	   Opponent's previous 5 opponents (aggregated together) cumulative take-downs landed per 5 minutes against the original opponent
O_Opp_TDT_p5m:	   Opponent's previous 5 opponents (aggregated together) cumulative take-downs thrown per 5 minutes against the original opponent
O_Opp_TDAcc:	   Opponent's previous 5 opponents (aggregated together) cumulative take-down accuracy per 5 minutes against the original opponent
O_Opp_HeadL_p5m:   Opponent's previous 5 opponents (aggregated together) cumulative head shots landed per 5 minutes against the orignal opponent
O_Opp_BodyL_p5m:   Opponent's previous 5 opponents (aggregated together) cumulative body shots landed per 5 minutes against the orignal opponent
O_Opp_LegL_p5m:    Opponent's previous 5 opponents (aggregated together) cumulative leg shots landed per 5 minutes against the orignal opponent
O_Opp_DistL_p5m:   Opponent's previous 5 opponents (aggregated together) cumulative distance shots landed per 5 minutes against the orignal opponent
O_Opp_ClinchL_p5m: Opponent's previous 5 opponents (aggregated together) cumulative clinch shots landed per 5 minutes against the orignal opponent
O_Opp_GrdL_p5m:    Opponent's previous 5 opponents (aggregated together) cumulative ground shots landed per 5 minutes against the orignal opponent
O_Opp_SSL_Beta:	   Using the opponent's previous 10 opponents, normalize those opponents' significant strikes landed (by time in case of stoppage) for each round, then estimate slope of line to assess how significant strikes landed 
		   changes over round, on average
Gender:		   Gender of fighter/opponent (Men, Women)
WC:		   Weightclass fight is taking place at (Atomweight, Strawweight,...,Heavyweight)
WCNum:		   Weightclass fitht is taking place at (1         , 2          ,...,10         ) in numeric form
WC1:		   Indicator if this fight is taking place at Atomweight
WC2:		   Indicator if this fight is taking place at Strawweight
WC3:		   Indicator if this fight is taking place at Flyweight
WC4:		   Indicator if this fight is taking place at Bantamweight
WC5:	           Indicator if this fight is taking place at Featherweight
WC6:		   Indicator if this fight is taking place at Lightweight
WC7:		   Indicator if this fight is taking place at Welterweight
WC8:		   Indicator if this fight is taking place at Middleweight
WC9:		   Indicator if this fight is taking place at LightHeavyweight
WC10:		   Indicator if this fight is taking place at Heavyweight
fight_key:	   Unique fight key used to identify the record corresponding to (fighter=fighter1,opponent=fighter2) with (fighter=fighter2,opponent=fighter1)
Res:		   Indicator for if the fighter won this fight
min_rank:	   The lowest rank between fighter/opponent at the time of their fight
MinFgts:	   The minimum number of fights between fighter/opponent at the time of their fight
Test:		   Indicator determining which fights are to be completely held out from model fitting, used solely for betting strategy back-testing