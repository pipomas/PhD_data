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
###    Philipp Thomas, 08. Januar 2016

library(rprime)          #<- needed to run analysis

# 0. Preparatory work ----------------------------------------------------------
allFiles <- paste("data/base/Hick", list.files(path = "data/base/Hick"), sep = "/")
numberOfFiles <- length(allFiles)

# 1. Read in Eprime .txt files, filter in relevant variables  and create data frames ----
list1 <- lapply(allFiles, read_eprime)
list2 <- lapply(list1, FrameList)
list3 <- lapply(list2, filter_in, key = "Procedure", values = c("blockprocH0","blockprocH1","blockprocH2","blockprocH258"))
list4 <- lapply(list3, to_data_frame)

# 1.1 Keep useful variables (i.e., accuracy and reaction time measures) --------
list5 <- lapply(list4, cbind.data.frame)
clean <- function(list){cbind.data.frame(list$plussignH0.ACC, list$plussignH0.RT,
                                         list$plussignH1.ACC, list$plussignH1.RT,
                                         list$plussignH2.ACC, list$plussignH2.RT,
                                         list$plussignH258.ACC, list$plussignH258.RT)}
list6 <- lapply(list5, clean)

# 1.2 Convert factors to numeric -----------------------------------------------
list7 <-  lapply(list6, function(x) {
                        x$`list$plussignH0.RT` <- as.numeric(as.character(x$`list$plussignH0.RT`))
                        x$`list$plussignH1.RT` <- as.numeric(as.character(x$`list$plussignH1.RT`))
                        x$`list$plussignH2.RT` <- as.numeric(as.character(x$`list$plussignH2.RT`))
                        x$`list$plussignH258.RT` <- as.numeric(as.character(x$`list$plussignH258.RT`))
                        x}) 

# 1.3 Relocate relevant variables and drop others for better overview-----------
list8 <- lapply(list7, function(x){
                       x[1:32,3:4] <- x[33:64,3:4]
                       x[1:32,5:6] <- x[65:96,5:6]
                       x[1:32,7:8] <- x[97:128,7:8]
                       x})

list9 <- lapply(list8, function(x){
                       x[33:128,] <- x[-(33:128),]
                       })


list10 <- lapply(list9, function(x){
                        colnames(x) <- c("H0acc","H0rt","H1acc","H1rt","H2acc","H2rt","H258acc","H258rt")
                        x}) 

list11 <- lapply(list10, function(x){
                         x$H0validAnsw   <- ifelse(x$H0rt > 99   & x$H0rt < 1501,   x$H0rt,   NA)
                         x$H1validAnsw   <- ifelse(x$H1rt > 99   & x$H1rt < 1501,   x$H1rt,   NA)
                         x$H2validAnsw   <- ifelse(x$H2rt > 99   & x$H2rt < 1501,   x$H2rt,   NA)
                         x$H258validAnsw <- ifelse(x$H258rt > 99 & x$H258rt < 1501, x$H258rt, NA)
                         x})

list12 <- lapply(list11, function(x){
                         x$H0validCorr   <- ifelse(x$H0acc   == 1, x$H0validAnsw,   NA)
                         x$H1validCorr   <- ifelse(x$H1acc   == 1, x$H1validAnsw,   NA)
                         x$H2validCorr   <- ifelse(x$H2acc   == 1, x$H2validAnsw,   NA)
                         x$H258validCorr <- ifelse(x$H258acc == 1, x$H258validAnsw, NA)
                         x})  
                           
list13 <- lapply(list12, function(x){
                         x$H0errorRate <- 1 - sum(!is.na(x$H0validCorr)) / sum(!is.na(x$H0validAnsw))
                         x$H1errorRate <- 1 - sum(!is.na(x$H1validCorr)) / sum(!is.na(x$H1validAnsw))
                         x$H2errorRate <- 1 - sum(!is.na(x$H2validCorr)) / sum(!is.na(x$H2validAnsw))
                         x$H258errorRate <- 1 - sum(!is.na(x$H258validCorr)) / sum(!is.na(x$H258validAnsw))
                         x})

# 2.3 Compute mean reaction times ----------------------------------------------
list14 <- lapply(list13, function(x){
                         x$H0meanRT <- round(mean(x$H0validCorr, na.rm = TRUE), digits = 0)
                         x$H1meanRT <- round(mean(x$H1validCorr, na.rm = TRUE), digits = 0)
                         x$H2meanRT <- round(mean(x$H2validCorr, na.rm = TRUE), digits = 0)
                         x$H258meanRT <- round(mean(x$H258validCorr, na.rm = TRUE), digits = 0)
                         
                         x$H0sdRT <- round(sd(x$H0validCorr, na.rm = TRUE), digits = 0)
                         x$H1sdRT <- round(sd(x$H1validCorr, na.rm = TRUE), digits = 0)
                         x$H2sdRT <- round(sd(x$H2validCorr, na.rm = TRUE), digits = 0)
                         x$H258sdRT <- round(sd(x$H258validCorr, na.rm = TRUE), digits = 0)
                         
                         x$H0medianRT <- round(median(x$H0validCorr, na.rm = TRUE), digits = 0)
                         x$H1medianRT <- round(median(x$H1validCorr, na.rm = TRUE), digits = 0)
                         x$H2medianRT <- round(median(x$H2validCorr, na.rm = TRUE), digits = 0)
                         x$H258medianRT <- round(median(x$H258validCorr, na.rm = TRUE), digits = 0)
                         
                         x})


# 2.4 Create vectors of all RTs -------------------------------------------
list15 <- lapply(list14, function(x){
                         x$H0allCorr   <- ifelse(x$H0acc   == 1, x$H0rt,   NA)
                         x$H1allCorr   <- ifelse(x$H1acc   == 1, x$H1rt,   NA)
                         x$H2allCorr   <- ifelse(x$H2acc   == 1, x$H2rt,   NA)
                         x$H258allCorr <- ifelse(x$H258acc == 1, x$H258rt, NA)
                         x})  

list16 <- do.call(rbind, lapply(list15, function(x){
                                        x[,c("H0allCorr","H1allCorr","H2allCorr","H258allCorr")]
                                        }))

convertVector <- function(x){as.vector(t(x))}
list17 <- lapply(list16, convertVector)

set.seed(1)
H0rtAllCorr   <- sample(list17$H0allCorr, replace = FALSE)
H1rtAllCorr   <- sample(list17$H1allCorr, replace = FALSE)
H2rtAllCorr   <- sample(list17$H2allCorr, replace = FALSE)
H258rtAllCorr <- sample(list17$H258allCorr, replace = FALSE)

# 3. Create target data frame & variable 'subject' -----------------------------
result            <- data.frame(matrix(0,numberOfFiles,16))
# result$subject  <- as.numeric(substr(allFiles, 6,8))
result$subject    <- as.numeric(substr(allFiles, 21,23))

# 3.1 Paste mean reaction times into the target data frame ---------------------
result[1:16] <- do.call(rbind,lapply(list14, function(x)
                                            x[1, c("H0meanRT","H1meanRT","H2meanRT","H258meanRT",
                                                   "H0sdRT","H1sdRT","H2sdRT","H258sdRT",
                                                   "H0medianRT","H1medianRT","H2medianRT","H258medianRT",
                                                   "H0errorRate","H1errorRate","H2errorRate","H258errorRate")]))


# 3.2 Reorder columns ----------------------------------------------------------
colnames(result)        <- c("H0meanRT","H1meanRT","H2meanRT","H258meanRT",
                             "H0sdRT","H1sdRT","H2sdRT","H258sdRT",
                             "H0medianRT","H1medianRT","H2medianRT","H258medianRT",
                             "H0errorRate","H1errorRate","H2errorRate","H258errorRate","subject")
result <- result[c(17,1:16)]

# 4. Cleaning up  --------------------------------------------------------------
result[13:17] <- round(result[13:17], 2)
resultsHick <- result
rm(list=setdiff(ls(), c("resultsHick", "H0rtAllCorr", "H1rtAllCorr", "H2rtAllCorr", "H258rtAllCorr",
                        "resultsSupp2","resultsBIS","resultsFragebogen")))
cat("\014")
str(resultsHick)

################################################################################
################################################################################

# 5. Plot reaction times for each subject INDIVIDUALLY--------------------------
xaxis <- c(0,1,2,3)
plot.new()
for (i in min(resultsHick$subject):max(resultsHick$subject))  
{subj <- subset(resultsHick,
                 subject==i, 
                 select=c(H0meanRT, H1meanRT, H2meanRT, H258meanRT)) 
  plot(xaxis, subj, 
     ylim=c(100,600),
     xlim=c(0,3),
     xlab=expression(log[2]~~of~~the~~number~~of~~stimulus~~response~~alternatives),
     ylab="RT [ms]",
     main=paste("subject #", i, sep = ""),
     bty           = "n",
     xaxt          = "n",
     yaxt          = "n",
     type          = "o")
  
  par(mfrow=c(1,1))

#par(new=T)
axis(side = 1, at = c(0,1,2,3), labels = c("0","1","2","2.58"))
axis(side = 2, at = c(100,200,300,400,500), las=2)
}


# 6. Plot reaction times for ALL SUBJECTS --------------------------------------
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
     ylim = c(100,550),
     xlab=expression(log[2]~~of~~the~~number~~of~~stimulus~~response~~alternatives),
     ylab="RT [ms]",
     main=expression(paste(mean %+-% sd[mean], " of all subjects")),
     bty           = "n",
     xaxt          = "n",
     yaxt          = "n",
     type          = "o")

#par(new=T)
axis(side = 1, at = c(0,1,2,3), labels = c("0","1","2","2.58"))
axis(side = 2, at = c(100,200,300,400,500), las=2)

# dispersion function
hickSD <- lapply(hickList,sd)
hickSD2 <- unlist(hickSD)
dispersion(xaxis, yaxis,
           ulim = hickSD2,
           llim = hickSD2,
           type = "a",
           fill = TRUE,
           intervals = TRUE,
           lty = 2,
           pch = 6)

# 7. Summary & correlation of conditions ---------------------------------------
describe(resultsHick)
corr.test(resultsHick[2:5], method = "spearman")

# 8. Create new dataframe for intercept, slope & rsquare of each regression----
numberOfFiles <- length(resultsHick$subject)

intslop <- data.frame(matrix(0,numberOfFiles,4))

intslop <- rename(intslop,
                  subject    = X1,
                  Hinter     = X2,
                  Hslope     = X3,
                  HrSquare   = X4)

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

intslop[i,2] <- round(intercept, digits = 0)
intslop[i,3] <- round(slope, digits = 0)
intslop[i,4] <- round(rsquared, digits = 4)
}


# 8.2 Merge 'intslop' dataframe with existing dataframe -------------------
resultsHick <- merge(resultsHick, intslop,
                     by = "subject")


# 9. Cleaning up ----------------------------------------------------------
cat("\014")
rm(list = setdiff(ls(), c("resultsHick", "H0rtAllCorr", "H1rtAllCorr", "H2rtAllCorr", "H258rtAllCorr",
                        "resultsSupp2","resultsBIS")))


# 10. Scatterplots --------------------------------------------------------
par(mfrow=c(1,4))
plot(H0rtAllCorr,
     main = "0 bit",
     ylim = c(0,1600),
     yaxt = "n",
     ylab = "RT [ms]",
     xlab = "all trials (randomly ordered)",
     bty = "n",
     las = 1)

axis(side = 2, at = c(0,100,200,300,400,500,1000,1500,1600), las=2)
abline(h = 100, col = "red", lwd = 2 )
abline(h = 1500, col = "red", lwd = 2 )

plot(H1rtAllCorr,
     main = "1 bit",
     ylim = c(0,1600),
     yaxt = "n",
     ylab = "RT [ms]",
     xlab = "all trials (randomly ordered)",
     bty = "n",
     las = 1)

axis(side = 2, at = c(0,100,200,300,400,500,1000,1500,1600), las=2)
abline(h = 100, col = "red", lwd = 2 )
abline(h = 1500, col = "red", lwd = 2 )

plot(H2rtAllCorr,
     main = "2 bit",
     ylim = c(0,1600),
     yaxt = "n",
     ylab = "RT [ms]",
     xlab = "all trials (randomly ordered)",
     bty = "n",
     las = 1)

axis(side = 2, at = c(0,100,200,300,400,500,1000,1500,1600), las=2)
abline(h = 100, col = "red", lwd = 2 )
abline(h = 1500, col = "red", lwd = 2 )

plot(H258rtAllCorr,
     main = "2.58 bit",
     ylim = c(0,1600),
     yaxt = "n",
     ylab = "RT [ms]",
     xlab = "all trials (randomly ordered)",
     bty = "n",
     las = 1)

axis(side = 2, at = c(0,100,200,300,400,500,1000,1500,1600), las=2)
abline(h = 100, col = "red", lwd = 2 )
abline(h = 1500, col = "red", lwd = 2 )
par(mfrow=c(1,1))


# 11. density plots -------------------------------------------------------
par(mfrow=c(4,1))
plot(density(resultsHick$H0meanRT,
            bw = 5),
             main = "0 bit",
             col = 1,
             xlim = c(100,600),
             # ylim = c(0,.3),
             # xaxt = "n",
             yaxt = "n",
             xlab = "mean RT",
             ylab = "",
             bty = "n",
             lty = 0)
polygon(density(resultsHick$H0meanRT, bw = 6),
        col = "8",
        density = 80)

plot(density(resultsHick$H1meanRT,
            bw = 5),
             main = "1 bit",
             col = 1,
             xlim = c(100,600),
             # ylim = c(0,.3),
             # xaxt = "n",
             yaxt = "n",
             xlab = "mean RT",
             ylab = "",
             bty = "n",
             lty = 0)
polygon(density(resultsHick$H1meanRT, bw = 6),
        col = "8",
        density = 80)

plot(density(resultsHick$H2meanRT,
            bw = 5),
             main = "2 bit",
             col = 1,
             xlim = c(100,600),
             # ylim = c(0,.3),
             # xaxt = "n",
             yaxt = "n",
             xlab = "mean RT",
             ylab = "",
             bty = "n",
             lty = 0)
polygon(density(resultsHick$H2meanRT, bw = 6),
        col = "8",
        density = 80)

plot(density(resultsHick$H258meanRT,
            bw = 5),
             main = "2.58 bit",
             col = 1,
             xlim = c(100,600),
             # ylim = c(0,.3),
             # xaxt = "n",
             yaxt = "n",
             xlab = "mean RT",
             ylab = "",
             bty = "n",
             lty = 0)
polygon(density(resultsHick$H258meanRT, bw = 6),
        col = "8",
        density = 80)

par(mfrow=c(1,1))


##############################################
# 8.2 ######### T E S T #### Environment below ---------------------------------
# 8.2 ######### T E S T #### Environment below ---------------------------------
# 8.2 ######### T E S T #### Environment below ---------------------------------
# 8.2 ######### T E S T #### Environment below ---------------------------------
# 8.2 ######### T E S T #### Environment below ---------------------------------
# 8.2 ######### T E S T #### Environment below ---------------------------------
# 8.2 ######### T E S T #### Environment below ---------------------------------
# 
# 
# 
# # 8.2 lapply test ------------------------------------------
# 
# do.call(rbind, apply(resultsHick, 1,
#                      FUN = function(i) {
#                      print(paste("Processing index:", i)) # helpful to see how slow/fast
#                      intslop2[i,1] <- i
#                      yvalues <- t(subset(resultsHick,
#                                          subject == i, 
#                                          select = c(H0meanRT, H1meanRT, H2meanRT, H258meanRT)))
#                      cond <- c(0:3)
#                      fit       <- lm(yvalues ~ cond)
#                      intercept <- fit$coefficients[1]
#                      slope     <- fit$coefficients[2]
#                      rsquared  <- summary(fit)$r.squared
# 
#                      intslop[i,2] <- intercept
#                      intslop[i,3] <- slope
#                      intslop[i,4] <- rsquared
#                      
#                      temp_df$intercept <- 1, etc.
#                      return(temp_df) # key is to return a data.frame for each index.
#                      }))
# 
# 
# 
# # 8.3 lapply teeeesting :) ------------------------------------------------
# 
# do.call(rbind, apply(resultsHick, 1,
#                      FUN = function(i) {
#                      # print(paste("Processing index:", i)) # helpful to see how slow/fast
#                      i
#                      }))
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# # 9. Create new dataframe for intercept, slope & rsquare of each regression----
# numberOfFiles <- length(resultsHick$subject)
# 
# intslop2 <- data.frame(matrix(0,numberOfFiles,4))
# intslop2 <- rename(intslop2,
#                    subject   = X1,
#                    intercept = X2,
#                    slope     = X3,
#                    Rsquare   = X4)
# 
# # 9.1 Regression loop ------------------------------------------------------
# cond <- c(0:3)
# 
# allSubjects <- resultsHick$subject
# 
# expModel <- function(x,a,b) {a * exp(b * x)} 
# 
# for (i in allSubjects)
# {intslop2[i,1] <- i
#   
# yvalues <- t(subset(resultsHick,
#                    subject == i, 
#                    select = c(H0meanRT, H1meanRT, H2meanRT, H258meanRT)))
# 
# fit       <- nls(yvalues ~ expModel(cond,a,b), start = list(a=1, b=1))}
# 
# intercept <- fit$coefficients[1]
# slope     <- fit$coefficients[2]
# rsquared  <- summary(fit)$r.squared
# 
# intslop[i,2] <- intercept
# intslop[i,3] <- slope
# intslop[i,4] <- rsquared
# }
# 
# 
# # 9.2 Simple exponential model --------------------------------------------
# xvalues <- seq(1,4,1)
# yvalues <- c(2.5, 7, 23, 59)
# # yvalues <- c(133, 152, 231, 498)
# nlsFit <- nls(yvalues ~ I(intercept*exp(slope*xvalues)), start = list(intercept = 30, slope = .10))
# 
# plot(xvalues,yvalues)
# lines(xvalues, predict(nlsFit))
# coef(nlsFit)
# 
# # -----
# x <- seq(min(xvalues), max(xvalues), length=100)
# y <- predict(nlsFit, list(xvalues=x))
# points(x, y, type='l', col='blue')
# 
# 
# 
# 
# 
# expFunction <- function(x,a,b){a*exp(b*x)}
# 
# 
# 
# 
# -------------------------------
# 
# ## Defining grid of z values 
# ## (100 values ensures a smooth curve in your case) 
# zValues <- seq(min(z), max(z), length.out = 100) 
# 
# 
# ## Adding predicted values corresponding to the grid values 
# lines(zVal, predict(fit, data.frame(z = zValues))) 
#   
#   
#   
# n <- 100
# x <- seq(n)
# y <- rnorm(n, 50 + 30 * x^(-0.2), 1)
# Data <- data.frame(x, y)
# 
# 
# 
# plot(y ~ x, Data)
# 
# # fit a loess line
# loess_fit <- loess(y ~ x, Data)
# lines(Data$x, predict(loess_fit), col = "blue")
# 
# # fit a non-linear regression
# nls_fit <- nls(y ~ a + b * x^(-c), Data, start = list(a = 80, b = 20, 
#     c = 0.2))
# lines(Data$x, predict(nls_fit), col = "red")
# 
# --------------------------------------------
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
cat("\014")
describe(resultsHick)
corr.test(resultsHick[2:5], method = "spearman")

