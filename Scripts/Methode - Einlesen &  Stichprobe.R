################################################################################
#
# 12.05.16
# 
# Check https://osf.io/mf59e/ for more information
#
# 0. clear workspace -----------------------------------------------------------
rm(list = ls())

# 1. read csv file -------------------------------------------------------------
res <- read.csv("data/processed/res.csv")
res <- as.data.frame(res[,-1])
is.data.frame(res)

# 2. check number of subjects --------------------------------------------------
nrow(res)

# 3. plot sex distribution -----------------------------------------------------
library(ggplot2)
qplot(res$sex)
summary(res$sex)

# 4. plot age distribution -----------------------------------------------------
qplot(res$agey)
summary(res$agey)
sd(res$agey)

# 5. variation in education -----------------------------------------------
summary(res$edu)

# 6. get time difference between test sessions ---------------------------------
res$t1 <- as.Date(res$t1)
res$t2 <- as.Date(res$t2)
summary(as.numeric(res$t2 - res$t1))
hist(as.numeric(res$t2 - res$t1), breaks = 100)