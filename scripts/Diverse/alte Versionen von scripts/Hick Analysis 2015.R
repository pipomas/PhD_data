################################################################################
################################################################################
###                                                                             
###   R E A D   E P R I M E   T X T - F I L E S   &   C R E A T E   D A T A   F R A M E
###   Hick Task
###
###   - 0-bit
###   - 1-bit
###   - 2-bit
###   - 2.58-bit
###
###    Philipp Thomas, 18. Dezember 2015

library(rprime)          #<- needed to run analysis

# 0. Preparatory work ------------------------------------------------------------------------------------------
# 0.1 Set working directory (wd) to the folder containing all the .txt files -----------
# setwd("/Users/philippthomas/Documents/Uni Bern/PDD/Dissertation/Daten/Project/data/base/Hick")

# 0.2 Select folder of Hick txt files & create a list of file names ---------------------
allFiles <- paste("data/base/Hick", list.files(path = "data/base/Hick"), sep = "/")
numberOfFiles <- length(allFiles)

# 1. Read in Eprime .txt files, filter in relevant variables  and create data frames ----
list1 <- lapply(allFiles, read_eprime)
list2 <- lapply(list1, FrameList)
list3 <- lapply(list2, filter_in, key = "Procedure", values = c("blockprocH0","blockprocH1","blockprocH2","blockprocH258"))
list4 <- lapply(list3, to_data_frame)

# 1.1 Keep useful variables (i.e., accuracy and reaction time measures) -----------------
list5 <- lapply(list4, cbind.data.frame)
clean <- function(list){cbind.data.frame(list$plussignH0.ACC, list$plussignH0.RT,
                                         list$plussignH1.ACC, list$plussignH1.RT,
                                         list$plussignH2.ACC, list$plussignH2.RT,
                                         list$plussignH258.ACC, list$plussignH258.RT)}
list6 <- lapply(list5, clean)

# 1.2 Convert factors to numeric --------------------------------------------------------
list7 <-  lapply(list6, function(x) {
                        x$`list$plussignH0.RT` <- as.numeric(as.character(x$`list$plussignH0.RT`))
                        x$`list$plussignH1.RT` <- as.numeric(as.character(x$`list$plussignH1.RT`))
                        x$`list$plussignH2.RT` <- as.numeric(as.character(x$`list$plussignH2.RT`))
                        x$`list$plussignH258.RT` <- as.numeric(as.character(x$`list$plussignH258.RT`))
                        x}) 

# 1.3 Relocate relevant variables and drop others for better overview-------------------
list8 <- lapply(list7, function(x){
                       x[1:32,3:4] <- x[33:64,3:4]
                       x[1:32,5:6] <- x[65:96,5:6]
                       x[1:32,7:8] <- x[97:128,7:8]
                       x})

list9 <- lapply(list8, function(x){
                       x[33:128,] = x[-(33:128),]
                       })

# 2. Create new variables ---------------------------------------------------------------
# 2.1 Filter out reaction times with wrong response by creating new variables -----------
list10 <- lapply(list9, function(x){
                        x$`list$H0acc`   <- ifelse(x$`list$plussignH0.ACC`   == 1, 1, 0)
                        x$`list$H0rt`    <- ifelse(x$`list$plussignH0.ACC`   == 1, x$`list$plussignH0.RT`, NA)
                        x$`list$H1acc`   <- ifelse(x$`list$plussignH1.ACC`   == 1, 1, 0)
                        x$`list$H1rt`    <- ifelse(x$`list$plussignH1.ACC`   == 1, x$`list$plussignH1.RT`, NA)
                        x$`list$H2acc`   <- ifelse(x$`list$plussignH2.ACC`   == 1, 1, 0)
                        x$`list$H2rt`    <- ifelse(x$`list$plussignH2.ACC`   == 1, x$`list$plussignH2.RT`, NA)
                        x$`list$H258acc` <- ifelse(x$`list$plussignH258.ACC` == 1, 1, 0)
                        x$`list$H258rt`  <- ifelse(x$`list$plussignH258.ACC` == 1, x$`list$plussignH258.RT`, NA)
                        x})

# 2.2 Replace reaction times <100, >1500 (Luce, 1986) with NA ------------------
# list11 <- lapply(list10, function(x){
                          # x$`list$H0rtTrim`    <- ifelse(is.na(x$`list$H0rt`),   NA, ifelse(x$`list$H0rt`   < 100, NA, ifelse(x$`list$H0rt`   > 1500, NA, x$`list$H0rt`)))
                          # x$`list$H1rtTrim`    <- ifelse(is.na(x$`list$H1rt`),   NA, ifelse(x$`list$H1rt`   < 100, NA, ifelse(x$`list$H1rt`   > 1500, NA, x$`list$H1rt`)))
                          # x$`list$H2rtTrim`    <- ifelse(is.na(x$`list$H2rt`),   NA, ifelse(x$`list$H2rt`   < 100, NA, ifelse(x$`list$H2rt`   > 1500, NA, x$`list$H2rt`)))
                          # x$`list$H258rtTrim`  <- ifelse(is.na(x$`list$H258rt`), NA, ifelse(x$`list$H258rt` < 100, NA, ifelse(x$`list$H258rt` > 1500, NA, x$`list$H258rt`)))
                          # x})


# 2.2 Create variable errorR & errorRate ---------------------------------------
list11 <- lapply(list10, function(x){
                          x$`list$H0errorR` <- ifelse(x$`list$H0rt` > 100 & x$`list$H0rt` < 1500, sum(is.na(x$`list$H0rt`)), NA)
                          x$`list$H1errorR` <- ifelse(x$`list$H1rt` > 100 & x$`list$H1rt` < 1500, sum(is.na(x$`list$H1rt`)), NA)
                          x$`list$H2errorR` <- ifelse(x$`list$H2rt` > 100 & x$`list$H2rt` < 1500, sum(is.na(x$`list$H2rt`)), NA)
                          x$`list$H258errorR` <- ifelse(x$`list$H258rt` > 100 & x$`list$H258rt` < 1500, sum(is.na(x$`list$H258rt`)), NA)
                          x})

list12 <- lapply(list11, function(x){
                          x$`list$H0errorRate` <- mean(x$`list$H0errorR`, na.rm = TRUE)
                          x$`list$H1errorRate` <- mean(x$`list$H1errorR`, na.rm = TRUE)
                          x$`list$H2errorRate` <- mean(x$`list$H2errorR`, na.rm = TRUE)
                          x$`list$H258errorRate` <- mean(x$`list$H258errorR`, na.rm = TRUE)
                          x})

# 2.3 Compute mean reaction times ------------------------------------------------------
list13 <- lapply(list12, function(x){
  
                         x$`list$H0meanReac` <- ifelse(x$`list$H0rt` > 100 & x$`list$H0rt` < 1500, mean(x$`list$H0rt`, na.rm = TRUE), NA)
                         x$`list$H1meanReac` <- ifelse(x$`list$H1rt` > 100 & x$`list$H1rt` < 1500, mean(x$`list$H1rt`, na.rm = TRUE), NA)
                         x$`list$H2meanReac` <- ifelse(x$`list$H2rt` > 100 & x$`list$H2rt` < 1500, mean(x$`list$H2rt`, na.rm = TRUE), NA)
                         x$`list$H258meanReac` <- ifelse(x$`list$H258rt` > 100 & x$`list$H258rt` < 1500, mean(x$`list$H258rt`, na.rm = TRUE), NA)
                         
                         # x$`list$H0meanRT`   <- mean(x$`list$H0rtTrim`,   na.rm = T)
                         # x$`list$H1meanRT`   <- mean(x$`list$H1rtTrim`,   na.rm = T)
                         # x$`list$H2meanRT`   <- mean(x$`list$H2rtTrim`,   na.rm = T)
                         # x$`list$H258meanRT` <- mean(x$`list$H258rtTrim`, na.rm = T)
                         # x$`list$H0medianRT`   <- median(x$`list$H0rtTrim`,   na.rm = T)
                         # x$`list$H1medianRT`   <- median(x$`list$H1rtTrim`,   na.rm = T)
                         # x$`list$H2medianRT`   <- median(x$`list$H2rtTrim`,   na.rm = T)
                         # x$`list$H258medianRT` <- median(x$`list$H258rtTrim`, na.rm = T)
                         x})

list14 <- lapply(list13, function(x){
  
                         x$`list$H0meanRT` <- mean(x$`list$H0meanReac`, na.rm = TRUE)
                         x$`list$H1meanRT` <- mean(x$`list$H1meanReac`, na.rm = TRUE)
                         x$`list$H2meanRT` <- mean(x$`list$H2meanReac`, na.rm = TRUE)
                         x$`list$H258meanRT` <- mean(x$`list$H258meanReac`, na.rm = TRUE)
                         
                         # x$`list$H0meanRT`   <- mean(x$`list$H0rtTrim`,   na.rm = T)
                         # x$`list$H1meanRT`   <- mean(x$`list$H1rtTrim`,   na.rm = T)
                         # x$`list$H2meanRT`   <- mean(x$`list$H2rtTrim`,   na.rm = T)
                         # x$`list$H258meanRT` <- mean(x$`list$H258rtTrim`, na.rm = T)
                         # x$`list$H0medianRT`   <- median(x$`list$H0rtTrim`,   na.rm = T)
                         # x$`list$H1medianRT`   <- median(x$`list$H1rtTrim`,   na.rm = T)
                         # x$`list$H2medianRT`   <- median(x$`list$H2rtTrim`,   na.rm = T)
                         # x$`list$H258medianRT` <- median(x$`list$H258rtTrim`, na.rm = T)
                         x})

# 3. Create target data frame & variable 'subject' --------------------------------------
result                  <- data.frame(matrix(0,numberOfFiles,12))
# result$subject          <- as.numeric(substr(allFiles, 6,8))
result$subject          <- as.numeric(substr(allFiles, 21,23))

# 3.1 Paste mean reaction times into the target data frame -----------------------------
result[1:12] <- do.call(rbind,lapply(list13, function(x)
                                            x[1, c('list$H0meanRT','list$H1meanRT','list$H2meanRT','list$H258meanRT',
                                                   'list$H0medianRT','list$H1medianRT','list$H2medianRT','list$H258medianRT',
                                                   'list$H0errorRate','list$H1errorRate','list$H2errorRate','list$H258errorRate')]))


# 3.2 Reorder columns ------------------------------------------------------------------
colnames(result)        <- c("H0meanRT","H1meanRT","H2meanRT","H258meanRT","H0medianRT","H1medianRT","H2medianRT","H258medianRT","H0errorRate","H1errorRate","H2errorRate","H258errorRate","subject")
result                  <- result[c(13,1:12)]

# 4. Cleaning up  ---------------------------------------------------------
result[2:13] <- round(result[2:13], 0)
resultsHick <- result
rm(list=setdiff(ls(), "resultsHick"))
str(resultsHick)



# 5. Descriptives ---------------------------------------------------------
cat("\014")
describe(resultsHick)
cov(resultsHick[2:5])
corr.test(resultsHick[2:5], method = "spearman")
# corr.test(resultsHick[6:9], method = "spearman")


# 6. lol ------------------------------------------------------------------



resultsHick <- result
rm(result)
str(resultsHick)
print(resultsHick)
summary(resultsHick)
corr.test(resultsHick[2:5], method = "spearman")


rm(list=setdiff(ls(), "resultsHick"))


# 5. Plot reaction times for each subject individually ---------------------------------
xaxis <- c(0,1,2,3)
plot.new()
for (i in min(result$subject):max(result$subject))  
{ subj <- subset(result,
                 subject==i, 
                 select=c(H0meanRT, H1meanRT, H2meanRT, H258meanRT)) 
plot(xaxis,subj,
     ylim=c(100,500),
     xlim=c(0,3),
     xlab="bit",
     ylab="RT [ms]",
     main=i,
     bty           = "n",
     xaxt          = "n",
     yaxt          = "n",
     type          = "o")

#par(new=T)
axis(side = 1, at = c(0,1,2,3), labels = c("0","1","2","2.58"))
axis(side = 2, at = c(100,200,300,400,500), las=2)
}


# 6. Plot reaction times for all subjects simultaneously ------------------
library(plotrix)

xaxis <- c(0,1,2,3)
hickList <- list(resultsHick[,2], resultsHick[,3],
              resultsHick[,4], resultsHick[,5])

yaxis2 <- lapply(hickList, mean)

yaxis <- c(mean(resultsHick[,2]), mean(resultsHick[,3]),
           mean(resultsHick[,4]), mean(resultsHick[,5]))

plot.new()

plot(xaxis,yaxis2,
     xlim=c(0,3),
     ylim = c(100,500),
     xlab="bit",
     ylab="RT [ms]",
     main="mean RT all subjects",
     bty           = "n",
     xaxt          = "n",
     yaxt          = "n",
     type          = "o")

#par(new=T)
axis(side = 1, at = c(0,1,2,3), labels = c("0","1","2","2.58"))
axis(side = 2, at = c(100,200,300,400,500), las=2)

# dispersion function
# 
hickSD <- lapply(hickList, sd)
hickSD2 <- unlist(hickSD)
dispersion(xaxis, yaxis,
           ulim = hickSD2,
           llim = hickSD2,
           type = "a",
           fill = TRUE,
           intervals = TRUE,
           lty = 2,
           pch = 6
           )

# 7. Summary -------------------------------------------------------------------
resultsHick <- result
rm(result)
str(resultsHick)
print(resultsHick)
summary(resultsHick)
corr.test(resultsHick[2:5], method = "spearman")


# 8. Create new dataframe for intercept, slope & rsquare of each regression----
numberOfFiles <- length(resultsHick$subject)

intslop <- data.frame(matrix(0,numberOfFiles,4))
intslop <- rename(intslop,
                  subject   = X1,
                  intercept = X2,
                  slope     = X3,
                  Rsquare   = X4)

# 8.1 Regression loop ------------------------------------------------------
cond <- c(0:3)

allSubjects <- resultsHick$subject

for (i in allSubjects)
{intslop[i,1] <- i
  
yvalues <- t(subset(resultsHick,
                   subject == i, 
                   select = c(H0meanRT, H1meanRT, H2meanRT, H258meanRT)))

fit       <- lm(yvalues ~ cond)

intercept <- fit$coefficients[1]
slope     <- fit$coefficients[2]
rsquared  <- summary(fit)$r.squared

intslop[i,2] <- intercept
intslop[i,3] <- slope
intslop[i,4] <- rsquared
}

# 8.2 Merge 'intslop' dataframe with existing dataframe -------------------
resultsHick <- merge(resultsHick, intslop,
                     by = "subject")

##############################################
# 8.2 ######### T E S T #### Environment below ---------------------------------


# 8.2 lapply test ------------------------------------------

do.call(rbind, apply(resultsHick, 1,
                     FUN = function(i) {
                     print(paste("Processing index:", i)) # helpful to see how slow/fast
                     intslop2[i,1] <- i
                     yvalues <- t(subset(resultsHick,
                                         subject == i, 
                                         select = c(H0meanRT, H1meanRT, H2meanRT, H258meanRT)))
                     cond <- c(0:3)
                     fit       <- lm(yvalues ~ cond)
                     intercept <- fit$coefficients[1]
                     slope     <- fit$coefficients[2]
                     rsquared  <- summary(fit)$r.squared

                     intslop[i,2] <- intercept
                     intslop[i,3] <- slope
                     intslop[i,4] <- rsquared
                     
                     temp_df$intercept <- 1, etc.
                     return(temp_df) # key is to return a data.frame for each index.
                     }))



# 8.3 lapply teeeesting :) ------------------------------------------------

do.call(rbind, apply(resultsHick, 1,
                     FUN = function(i) {
                     # print(paste("Processing index:", i)) # helpful to see how slow/fast
                     i
                     }))










# 9. Create new dataframe for intercept, slope & rsquare of each regression----
numberOfFiles <- length(resultsHick$subject)

intslop2 <- data.frame(matrix(0,numberOfFiles,4))
intslop2 <- rename(intslop2,
                   subject   = X1,
                   intercept = X2,
                   slope     = X3,
                   Rsquare   = X4)

# 9.1 Regression loop ------------------------------------------------------
cond <- c(0:3)

allSubjects <- resultsHick$subject

expModel <- function(x,a,b) {a * exp(b * x)} 

for (i in allSubjects)
{intslop2[i,1] <- i
  
yvalues <- t(subset(resultsHick,
                   subject == i, 
                   select = c(H0meanRT, H1meanRT, H2meanRT, H258meanRT)))

fit       <- nls(yvalues ~ expModel(cond,a,b), start = list(a=1, b=1))}

intercept <- fit$coefficients[1]
slope     <- fit$coefficients[2]
rsquared  <- summary(fit)$r.squared

intslop[i,2] <- intercept
intslop[i,3] <- slope
intslop[i,4] <- rsquared
}


# 9.2 Simple exponential model --------------------------------------------
xvalues <- seq(1,4,1)
yvalues <- c(2.5, 7, 23, 59)
# yvalues <- c(133, 152, 231, 498)
nlsFit <- nls(yvalues ~ I(intercept*exp(slope*xvalues)), start = list(intercept = 30, slope = .10))

plot(xvalues,yvalues)
lines(xvalues, predict(nlsFit))
coef(nlsFit)

# -----
x <- seq(min(xvalues), max(xvalues), length=100)
y <- predict(nlsFit, list(xvalues=x))
points(x, y, type='l', col='blue')





expFunction <- function(x,a,b){a*exp(b*x)}




-------------------------------

## Defining grid of z values 
## (100 values ensures a smooth curve in your case) 
zValues <- seq(min(z), max(z), length.out = 100) 


## Adding predicted values corresponding to the grid values 
lines(zVal, predict(fit, data.frame(z = zValues))) 
  
  
  
n <- 100
x <- seq(n)
y <- rnorm(n, 50 + 30 * x^(-0.2), 1)
Data <- data.frame(x, y)



plot(y ~ x, Data)

# fit a loess line
loess_fit <- loess(y ~ x, Data)
lines(Data$x, predict(loess_fit), col = "blue")

# fit a non-linear regression
nls_fit <- nls(y ~ a + b * x^(-c), Data, start = list(a = 80, b = 20, 
    c = 0.2))
lines(Data$x, predict(nls_fit), col = "red")

--------------------------------------------













