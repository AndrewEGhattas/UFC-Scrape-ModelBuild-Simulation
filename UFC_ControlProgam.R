#############################################################################################
# Author: Andrew Ghattas                                                                    #
# Program: UFC_ControlProgam.R                                                              #
# Purpose: Build model to predict UFC fight winner, use this to simulate a betting strategy #
#                                                                                           #
#          This is the control program that calls all other programs. It declares program   #
#          parameters and functions which are used throughout all programs,                 # 
#          then sequentially call each program                                              #
#############################################################################################

# Clear workspace
rm(list=ls())

#============================#
# General program parameters #
#============================#

# Base directory which contains the /R Programs, and /Documentation directories
base.dir <- "G:/My Drive/My_written_programs/[R] UFC Fight Predictions/UFC-Scrape-ModelBuild-Simulation/"

# String containing the names of all program paramters and functions, this will be used to quickly clean up the workspace
params.and.funcs <- c("base.dir","RAM.to.use.H2O","Rand.iters","Test.size","dir","install.or.load.package", "params.and.funcs")

#==========================#
# Model fitting parameters #
#==========================#

# How much RAM to allocate to H2O
RAM.to.use.H2O <- "40g"

# The number of random hyper-parameter models to run for each model type
Rand.iters <- 2000

# The size of the test dataset. The train/validation sets are used for model fitting, whereas the test set is used strictly to evaluate the final
# model's performance. Keep in mind that there's 2 records for each fight (fighter1,fighter2) and (fighter2,fighter1) so Test.size=1000 fights 
# really means 500 fights. Why are there 2 records per fight? Because we'll score the model using each of the 2 combinations, then average
# the result for a final prediction.
Test.size <- 2000

#===================#
# Declare functions #
#===================#

# Directory creating function that doesn't throw a warning if directory already exists
dir <- function(path){ if(file.exists(file1=path)==FALSE){ dir.create(path=path) } }

# Function to ry to load a given package, if it hasn't already been installed, then install it
install.or.load.package <- function(X_package){
  # Try to load package, if the package was already installed this will load the package as expected, and "x" will contain
  # the location of the pacakge. If the package hasn't been installed then an error message will be thrown. The "silent" option
  # will supress an error being thrown, but the error message will be captured in "x" object
  x <- try(find.package(X_package), silent = TRUE)
  # If the package was found then load it
  if( grepl(x=x[1], pattern="Error in ") == FALSE ){ 
    library(X_package, character.only=TRUE)
    print( paste0("Package [", X_package, "] already installed") ) }
  # If the package had not previously been installed then install and load it
  if( grepl(x=x[1], pattern="Error in ") == TRUE ){ 
    install.packages(X_package)
    library(X_package, character.only=TRUE)
    print( paste0("Package [", X_package, "] was not previously installed") ) } }

#=========================================#
# Save the program parameters + functions #
#=========================================#

# Save the results
dir(path=paste0(base.dir,"/Parameters and functions"))
setwd(dir=paste0(base.dir,"/Parameters and functions"))
save(params.and.funcs, base.dir, RAM.to.use.H2O, Rand.iters, Test.size, dir, install.or.load.package, file="ParamsAndFuncs.R")

#=============#
# Set options #
#=============#

# Prevent scientific notation
options(scipen=999)

#==================#
# Install packages #
#==================#

# Used for: removeDirectory() function
install.or.load.package(X_package="R.utils")

# Used for: general tidyverse operations
install.or.load.package(X_package="tidyverse")

# Used for: general web scraping operations
install.or.load.package(X_package="rvest")

# Used for: general date manipulation
install.or.load.package(X_package="lubridate")

# Used for: list.stack() function
install.or.load.package(X_package="rlist")

# Used for: radarchart() function
install.or.load.package(X_package="fmsb")

# Used for: creating 2D-PDP
install.or.load.package(X_package="plot3Drgl")
install.or.load.package(X_package="rgl")

# Used for: general H2O model fitting
install.or.load.package(X_package="h2o")

# Used for: creating table output to .jpeg
install.or.load.package(X_package="gridExtra")

# Used for: detectCores() function
install.or.load.package(X_package="parallel")

# Used for: creating table figures
install.or.load.package(X_package="gtable")

# Used for: drawing line segments on table figures
install.or.load.package(X_package="grid")

#==============================================#
# Create directory where we'll store log files #
#==============================================#

# Create directory
dir(path=paste0(base.dir,"/Logs"))

#===================#
# Call each program #
#===================#

# Program 1: Scrape the fight level data from UFC.com
rm(list = ls()[ !(ls() %in% params.and.funcs) ])
sink(paste0(base.dir,"/Logs/UFC_Program1.txt"))
  setwd(dir=base.dir)
  source(file="UFC_Program1.R", echo=TRUE, max.deparse.length=1000000)
sink()

# Program 2: Scrape the historical betting odds
rm(list = ls()[ !(ls() %in% params.and.funcs) ])
sink(paste0(base.dir,"/Logs/UFC_Program2.txt"))
setwd(dir=base.dir)
source(file="UFC_Program2.R", echo=TRUE, max.deparse.length=1000000)
sink()

# Program 3: Scrape the historical fighter rankings
rm(list = ls()[ !(ls() %in% params.and.funcs) ])
sink(paste0(base.dir,"/Logs/UFC_Program3.txt"))
setwd(dir=base.dir)
source(file="UFC_Program3.R", echo=TRUE, max.deparse.length=1000000)
sink()
                                                                                                                                                                      
# Program 4: Combine the fights, rankings, and odds data then feature engineer/process the dataframe
rm(list = ls()[ !(ls() %in% params.and.funcs) ])
sink(paste0(base.dir,"/Logs/UFC_Program4.txt"))
setwd(dir=base.dir)
source(file="UFC_Program4.R", echo=TRUE, max.deparse.length=1000000)
sink()

# Program 5: Fit and use the ML models for simulating the betting strategy
rm(list = ls()[ !(ls() %in% params.and.funcs) ])
sink(paste0(base.dir,"/Logs/UFC_Program5.txt"))
setwd(dir=base.dir)
source(file="UFC_Program5.R", echo=TRUE, max.deparse.length=1000000, verbose=FALSE)
sink()











