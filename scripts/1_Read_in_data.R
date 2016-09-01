# ---
# Title: "1_Read_in_data.R"
# Description: "This code loads the required packages and reads in the csv file."
# Author: "Philipp Thomas"
# ---

# 1. Set working directory -----------------------------------------------------
# In order to run the code, the downloaded repository needs to be set as your
# working directory. You can reveal your current working directory with
getwd()

# To change your working directory, use setwd(/Your/Path/To/The/Downloaded/
# Repository). See http://rfunction.com/archives/1001
# setwd(Uncomment/This/Line/And/Adjust/Your/Path)

# Check your working directory again. On my PC the function 'getwd()' returns
# "/Users/philippthomas/Documents/UniBern/PDD/Dissertation/PhD_data-master".
# Your path should now end with "[Your/Local/Path]/PhD_data-master"
getwd()

# 2. Install packages ----------------------------------------------------------
# In order for all other packages to install, you need a package called
# 'pacman'. For the 'pacman' package to install and load, you need to load the
# function 'ipak_function' into your workspace. Run this line:
source("scripts/source_scripts/ipak_function.R")

# Then use the 'ipak' function to install and load 'pacman'. If the function
# returns TRUE, it has been successfully loaded
ipak("pacman")

# These additional packages were needed for the analyses
# coin      : needed for friedman post hoc tests
# dplyr     : provides a flexible grammar of data manipulation
# effsize   : for the computation of effect sizes
# ez        : for a ez computation of ANOVA
# ggplot2   : for visually pleasing plots
# lavaan    : package for structural equation modeling (SEM)
# lm.beta   : for standardised regression coefficients
# lmSupport : for F change statistics
# MASS      : needed for friedman post hoc tests
# Metrics   : for the computation of the root mean square error
# multcomp  : for friedman post hoc tests (loaded automatically)
# nlme      : used for Tukey post hoc tests
# nlstools  : a toolbox for nonlinear regression in R
# pbapply   : for progress bars in apply functions
# plotrix   : for special x anx y axes in plots
# ppcor     : for computation of partial correlations
# psych     : a package for psychometric research
# readxl    : read excel files into R
# reshape2  : for reshaping data frames
# rprime    : read eprime txt files into R
# R.matlab  : read MATLAB files into R
# semPlot   : functions to visualise SEM results

# Put needed package names into the vector 'mypackages'
mypackages <- c("reshape2", "ggplot2", "ppcor", "lm.beta", "lmSupport",
                "effsize", "ez", "nlme", "coin", "multcomp", "MASS",
                "lavaan", "semPlot", "psych", "dplyr", "pacman")

# Install and load 'mypackages'
p_load(mypackages, character.only = TRUE)

# 3. Read csv file -------------------------------------------------------------
dat <- read.csv("data/processed/dat.csv")  # read csv from downloaded folder
dat <- dat[,-1] %>% tbl_df()               # remove first col then make tbl_df()

# 4. Inspect data frame 'dat' --------------------------------------------------
# These three commands are different ways to inspect the data frame named 'dat'
dat                                        # inspect data frame table (tbl_df())
str(dat)                                   # or reveal structure of object
glimpse(dat)                               # or transposed version of 'print'

# 5. Check for data and packages -----------------------------------------------
# To check if data and packages were loaded correctly, you need to load the
# function 'check_function' into your workspace. Run this line:
source("scripts/source_scripts/check_function.R")

# Then check if data and packages were successfully loaded
check()
