################################################################################
################################################################################
###
###
###   R E A D   M A T L A B   F I L E S   &   C R E A T E   D A T A   F R A M E
###   Spatial Suppression - Melnick et al. (2013)
###
###   Modified setup:
###   - 4 stimulus sizes: 1.8°, 3.6°, 5.4°, 7.2°
###   - 3 runs
###   - 2 repeats (number of staircases per condition)
###   - 22 trials per staircase
###   - 176 trials per run (2 * 22 * 4 )
###   - 528 trials in total
###   - Asus VG248QE (144 Hz)
###   - 1080 x 1920 p
###   - Screen luminance ~ 190(?) cd/m^2
###   - Room luminance (white paper next to the monitor) ~ 9 cd/m^2
###     [left = 10.2, bottom = 8.2, right = 7.7, top = 8.7 --> ~9]
###   - Matlab contrast was set to 100 %
###
###    Philipp Thomas, 08. Januar 2016

# library(R.matlab)      <- needed to run analysis
# library(dplyr)         <- needed to run analysis
# library(xlsx)          <- needed to print the xlsx file
library(ggplot2)
par(mfrow=c(1,1))

# 1. Select folder of .mat files & create a list of file names -----------------
allFiles <- paste("data/base/Supp2", list.files(path = "data/base/Supp2"), sep = "/")
numberOfFiles <- length(allFiles)                  #count files

# 2. Create target dataframe ---------------------------------------------------
result <- data.frame(matrix(0,numberOfFiles,24))   #create an empty dataframe(0,numberOfFiles,24 variables)

# 3. Read files and create a data frame ----------------------------------------
list1    <- lapply(allFiles, readMat)              #list apply function readMat on allFiles
clean   <- function(list){(list$result)}           #read result section of list
list2   <- lapply(list1, clean)                    #list apply function 'clean'
trans   <- function(matrix){as.vector(t(matrix))}  #transform matrix to vector
list3   <- lapply(list2, trans)                    #list apply function 'trans'
result  <- do.call(rbind, list3)                   #row bind list3
results <- as.data.frame(cbind(allFiles,result))   #attach file names & create data frame

# 4. Rename variables of data frame 'results' ----------------------------------
results  <- rename(results,
                   subject=allFiles,
                   s1r1p1=V2,   s2r1p1=V8,    s3r1p1=V14,     s4r1p1=V20,
                   s1r1p2=V3,   s2r1p2=V9,    s3r1p2=V15,     s4r1p2=V21,
                   s1r2p1=V4,   s2r2p1=V10,   s3r2p1=V16,     s4r2p1=V22,
                   s1r2p2=V5,   s2r2p2=V11,   s3r2p2=V17,     s4r2p2=V23,
                   s1r3p1=V6,   s2r3p1=V12,   s3r3p1=V18,     s4r3p1=V24,
                   s1r3p2=V7,   s2r3p2=V13,   s3r3p2=V19,     s4r3p2=V25)

# 5. Arrange data frame by subject ---------------------------------------------
results  <- arrange(results, subject)                     #doesn't work
sapply(results, class)                                    #check class of variables:
                                                          # -> all variables are factors
factorconvert <- function(f){as.numeric(levels(f))[f]}    #convert factors -> numeric
results[2:25]  <- lapply(results[2:25], factorconvert)    #apply function
sapply(results, class)                                    #check conversion: -> it worked

# 6. Retain only subject number -----------------------------------------------
results$subject <- as.numeric(substr(allFiles, 17,19))

# 6.1 Drop subjects because of wrong contrast ----------------------------------
results <- subset(results, !subject %in% c(1:7))

# 7. Retain 't2' --------------------------------------------------------------
results$t2 <- as.Date(substr(allFiles,31,40))

# 8. Retain 'exp2' and convert to factor --------------------------------------
results$exp2 <- substr(allFiles,21,27)
results[,27] <- factor(results[,27])
nrow(results)
names(results)

# 9. Correct for the hard coded stimulus duration limit of 1000 ms -------------
names(results[2:25])
results[2:25][results[2:25]>400]  <- 400              #replace all thresholds > 400 with 400

# 10. Drop subjects who hit the built in stimulus duration limit more than once ----
# Code not very elegant
# names(results[,2:7])
# names(results[,8:13])
# names(results[,14:19])
# names(results[,20:25])
dropSubjects <- function(x, n, cols) x[rowSums(x[cols] == n) < 2, ]
results <- dropSubjects(results, 400, c(-1,-8:-25))
results <- dropSubjects(results, 400, c(-1:-7,-14:-25))
results <- dropSubjects(results, 400, c(-1:-13,-20:-25))
results <- dropSubjects(results, 400, c(-1:-19))

# 11. Create actual stimulus duration -------------------------------------------
# Compute approximation of actual stimulus duration (full width at half height
# of the temporal envelope) and create new variables w/ prefix x25 for '*2.5'.
# raw data gets multiplied by 2.5 (therefore the prefix x25)
results$x25s1r1p1  <- results$s1r1p1 * 2.5
results$x25s1r1p2  <- results$s1r1p2 * 2.5
results$x25s1r2p1  <- results$s1r2p1 * 2.5
results$x25s1r2p2  <- results$s1r2p2 * 2.5
results$x25s1r3p1  <- results$s1r3p1 * 2.5
results$x25s1r3p2  <- results$s1r3p2 * 2.5

results$x25s2r1p1  <- results$s2r1p1 * 2.5
results$x25s2r1p2  <- results$s2r1p2 * 2.5
results$x25s2r2p1  <- results$s2r2p1 * 2.5
results$x25s2r2p2  <- results$s2r2p2 * 2.5
results$x25s2r3p1  <- results$s2r3p1 * 2.5
results$x25s2r3p2  <- results$s2r3p2 * 2.5

results$x25s3r1p1  <- results$s3r1p1 * 2.5
results$x25s3r1p2  <- results$s3r1p2 * 2.5
results$x25s3r2p1  <- results$s3r2p1 * 2.5
results$x25s3r2p2  <- results$s3r2p2 * 2.5
results$x25s3r3p1  <- results$s3r3p1 * 2.5
results$x25s3r3p2  <- results$s3r3p2 * 2.5

results$x25s4r1p1  <- results$s4r1p1 * 2.5
results$x25s4r1p2  <- results$s4r1p2 * 2.5
results$x25s4r2p1  <- results$s4r2p1 * 2.5
results$x25s4r2p2  <- results$s4r2p2 * 2.5
results$x25s4r3p1  <- results$s4r3p1 * 2.5
results$x25s4r3p2  <- results$s4r3p2 * 2.5

# 12. Compute raw thresholds ----------------------------------------------------
# Compute raw thresholds (since THESE values are computed by MATLAB in the
# first place -> log space!) and create new variables w/ prefix log10.
results$log10s1r1p1  <- log10(results$x25s1r1p1)
results$log10s1r1p2  <- log10(results$x25s1r1p2)
results$log10s1r2p1  <- log10(results$x25s1r2p1)
results$log10s1r2p2  <- log10(results$x25s1r2p2)
results$log10s1r3p1  <- log10(results$x25s1r3p1)
results$log10s1r3p2  <- log10(results$x25s1r3p2)

results$log10s2r1p1  <- log10(results$x25s2r1p1)
results$log10s2r1p2  <- log10(results$x25s2r1p2)
results$log10s2r2p1  <- log10(results$x25s2r2p1)
results$log10s2r2p2  <- log10(results$x25s2r2p2)
results$log10s2r3p1  <- log10(results$x25s2r3p1)
results$log10s2r3p2  <- log10(results$x25s2r3p2)

results$log10s3r1p1  <- log10(results$x25s3r1p1)
results$log10s3r1p2  <- log10(results$x25s3r1p2)
results$log10s3r2p1  <- log10(results$x25s3r2p1)
results$log10s3r2p2  <- log10(results$x25s3r2p2)
results$log10s3r3p1  <- log10(results$x25s3r3p1)
results$log10s3r3p2  <- log10(results$x25s3r3p2)

results$log10s4r1p1  <- log10(results$x25s4r1p1)
results$log10s4r1p2  <- log10(results$x25s4r1p2)
results$log10s4r2p1  <- log10(results$x25s4r2p1)
results$log10s4r2p2  <- log10(results$x25s4r2p2)
results$log10s4r3p1  <- log10(results$x25s4r3p1)
results$log10s4r3p2  <- log10(results$x25s4r3p2)

names(results)


# 13. Compute log10 mean threshold (excluding min & max threshold) -------------
meanCustom <- function(x){
              x <- x[x != min(x) & x != max(x)]
              return(mean(x))
              }

results$S1log10mean <- round(apply(results[,52:57], 1, meanCustom), digits = 4)
results$S2log10mean <- round(apply(results[,58:63], 1, meanCustom), digits = 4)
results$S3log10mean <- round(apply(results[,64:69], 1, meanCustom), digits = 4)
results$S4log10mean <- round(apply(results[,70:75], 1, meanCustom), digits = 4)

names(results[52:57])
names(results[58:63])
names(results[64:69])
names(results[70:75])


# 14. Compute log10 sd thresholds (excluding min & max threshold) --------------
sdCustom <- function(x){
            x <- x[x != min(x) & x != max(x)]
            return(sd(x))
            }

results$S1log10sd <- round(apply(results[,52:57], 1, sdCustom), digits = 4)
results$S2log10sd <- round(apply(results[,58:63], 1, sdCustom), digits = 4)
results$S3log10sd <- round(apply(results[,64:69], 1, sdCustom), digits = 4)
results$S4log10sd <- round(apply(results[,70:75], 1, sdCustom), digits = 4)

# 15. Compute mean threshold and sd (used mainly for plotting) ------------------
results$S1mean <- round(10 ^ results$S1log10mean, digits = 0)
results$S2mean <- round(10 ^ results$S2log10mean, digits = 0)
results$S3mean <- round(10 ^ results$S3log10mean, digits = 0)
results$S4mean <- round(10 ^ results$S4log10mean, digits = 0)

results$S1sd <- round(10 ^ results$S1log10sd, digits = 3)
results$S2sd <- round(10 ^ results$S2log10sd, digits = 3)
results$S3sd <- round(10 ^ results$S3log10sd, digits = 3)
results$S4sd <- round(10 ^ results$S4log10sd, digits = 3)



# 16. Compute 'SUPPRESSION INDEX 2' ----------------------------------------------
results$SI2  <- round(results$S4log10mean - results$S1log10mean, digits = 3)
names(results[,c(1,27,26,76:92)])

# 17. Drop some variables and create new dataframe -----------------------------
resultsSupp2 <- results[,c(1,27,26,76:92)]

# 18. Drop subjects ... --------------------------------------------------------
# .... 18.1 ... with wrong contrast -------------------------------------------------
# resultsSupp2 <- subset(resultsSupp2, !subject %in% c(1:7))

# .... 18.2 ... with negative suppression -------------------------------------------
# resultsSupp2 <- subset(resultsSupp2, SI2 < 0)

# 19. Cleaning up --------------------------------------------------------------
# rm(list=setdiff(ls(), c("resultsSupp2", "resultsHick")))
# cat("\014")
corr.test(resultsSupp2[4:7],method = "spearman")
corr.test(resultsSupp2[12:15],method = "spearman")
nrow(resultsSupp2)

################################################################################
# 20. Plot thresholds for each subject INDIVIDUALLY ----------------------------
xSup              <- c(1.8,3.6,5.4,7.2)
plot.new()
# for (i in min(resultsSupp2$subject):max(resultsSupp2$subject))
for (i in resultsSupp2$subject)
{ subj <- subset(resultsSupp2,
                 subject == i,
                 select = c(S1mean, S2mean, S3mean, S4mean))
plot(xSup,subj,
     log = "xy",
     ylim=c(20,400),
     xlim=c(1.8,7.2),
     xlab="visual angle [°]",
     ylab="duration threshold [ms]",
     main=paste("subject #", i, sep = ""),
     bty           = "n",
     xaxt          = "n",
     yaxt          = "n",
     type          = "o")

#par(new=T)
axis(side = 1, at = c(1.8,3.6,5.4,7.2), labels=T)
axis(side = 2, at = c(25,50,100,200,400), las=1)
}

# 21. Plot thresholds for ALL SUBJECTS -----------------------------------------
library(plotrix)
xSup     <- c(1.8,3.6,5.4,7.2)
Supp2List <- list(resultsSupp2[,12], resultsSupp2[,13], resultsSupp2[,14], resultsSupp2[,15])
yaxis2 <- lapply(Supp2List, mean)
yaxis <- c(mean(resultsSupp2[,12]), mean(resultsSupp2[,13]),
           mean(resultsSupp2[,14]), mean(resultsSupp2[,15]))

plot.new()
plot(xSup,yaxis2,
     log = "x",
     xlim=c(1.8,7.2),
     ylim = c(0,300),
     xlab="visual angle [°]",
     ylab="duration threshold [ms]",
     main=expression(paste(mean %+-% sd[mean], " of all subjects")),
     bty           = "n",
     xaxt          = "n",
     yaxt          = "n",
     type          = "o")

#par(new=T)
axis(side = 1, at = c(1.8,3.6,5.4,7.2), labels=T)
axis(side = 2, at = c(0,25,50,100,200,300), las=1)

# dispersion function
SuppSD <- lapply(Supp2List,sd)
SuppSD2 <- unlist(SuppSD)
dispersion(xSup, yaxis,
           ulim = SuppSD2,
           # llim = SuppSD2,
           # type = "a",
           fill = TRUE,
           # intervals = TRUE,
           # lty = 2
           )

# 22. Plot thresholds for each size -------------------------------------------
par(mfrow=c(1,4))
plot(resultsSupp2$S1mean,
     main = paste("S1, N = ", nrow(resultsSupp2), sep = ""),
     ylim = c(0,600),
     xlim = c(0,200),
     yaxt = "n",
     ylab = "Threshold 82 % correct [ms]",
     xlab = "Subject",
     col = ifelse(resultsSupp2$subject %in% c(10,35,173), "red", "black"),
     pch = ifelse(resultsSupp2$subject %in% c(10,35,173), 20, 20),
     cex = ifelse(resultsSupp2$subject %in% c(10,35,173), 2, 2),
     bty = "n",
     las = 1)

axis(side = 2, at = seq(0,600,100), las=2)
abline(h = mean(resultsSupp2$S1mean), col = "black", lwd = 1)

abline(h = mean(resultsSupp2$S1mean) + 3*sd(resultsSupp2$S1mean), col = "black", lwd = 2, lty = 2)
abline(h = colMeans(subset(resultsSupp2, !subject %in% c(10,35,173), select = S1mean)), col = "red", lwd = 2)
text(100, 600, labels = "-4 ms", cex = 7, col = "red")



plot(resultsSupp2$S2mean,
     main = paste("S2, N = ", nrow(resultsSupp2), sep = ""),
     ylim = c(0,600),
     xlim = c(0,200),
     yaxt = "n",
     ylab = "",
     xlab = "Subject",
     col = ifelse(resultsSupp2$subject %in% c(10,35,173), "red", "black"),
     pch = ifelse(resultsSupp2$subject %in% c(10,35,173), 20, 20),
     cex = ifelse(resultsSupp2$subject %in% c(10,35,173), 2, 2),
     bty = "n",
     las = 1)

axis(side = 2, at = seq(0,600,100), las=2)
abline(h = mean(resultsSupp2$S2mean), col = "black", lwd = 1)

# abline(h = mean(resultsSupp2$S1mean) + 3*sd(resultsSupp2$S1mean), col = "black", lwd = 2, lty = 2)
abline(h = colMeans(subset(resultsSupp2, !subject %in% c(10,35,173), select = S2mean)), col = "red", lwd = 2)
text(100, 600, labels = "-3 ms", cex = 7, col = "red")

plot(resultsSupp2$S3mean,
     main = paste("S3, N = ", nrow(resultsSupp2), sep = ""),
     ylim = c(0,600),
     xlim = c(0,200),
     yaxt = "n",
     ylab = "",
     xlab = "Subject",
     col = ifelse(resultsSupp2$subject %in% c(10,35,173), "red", "black"),
     pch = ifelse(resultsSupp2$subject %in% c(10,35,173), 20, 20),
     cex = ifelse(resultsSupp2$subject %in% c(10,35,173), 2, 2),
     bty = "n",
     las = 1)

axis(side = 2, at = seq(0,600,100), las=2)
abline(h = mean(resultsSupp2$S3mean), col = "black", lwd = 1)

abline(h = colMeans(subset(resultsSupp2, !subject %in% c(10,35,173), select = S3mean)), col = "red", lwd = 2)
text(100, 600, labels = "-4 ms", cex = 7, col = "red")

plot(resultsSupp2$S4mean,
     main = paste("S4, N = ", nrow(resultsSupp2), sep = ""),
     ylim = c(0,600),
     xlim = c(0,200),
     yaxt = "n",
     ylab = "",
     xlab = "Subject",
     col = ifelse(resultsSupp2$subject %in% c(10,35,173), "red", "black"),
     pch = ifelse(resultsSupp2$subject %in% c(10,35,173), 20, 20),
     cex = ifelse(resultsSupp2$subject %in% c(10,35,173), 2, 2),
     bty = "n",
     las = 1)

axis(side = 2, at = seq(0,600,100), las=2)
abline(h = mean(resultsSupp2$S4mean), col = "black", lwd = 1)

abline(h = colMeans(subset(resultsSupp2, !subject %in% c(10,35,173), select = S4mean)), col = "red", lwd = 2)
text(100, 600, labels = "-5 ms", cex = 7, col = "red")

par(mfrow=c(1,1))

# XX. Plot Suppression Index -------------------------------
# dotchart(resultsSupp2$SI2,
#          labels = row.names(resultsSupp2),
#          cex = .7,
#          ylab = "subject number",
#          xlab = "Suppression Index [log10]",
#          main = "Streuung der Suppression Indizes",
#          bty = "n")

# 23. Check for outliers ------------------------------------------------------
plot(resultsSupp2$S1mean, resultsSupp2$S4mean,
     log = "xy",
     ylim=c(10,1000),
     xlim=c(10,1000),
     xlab="s1 threshold 80% correct [ms]",
     ylab="s4 threshold 80% correct [ms]",
     main="Spatial Suppression\nOutliers (-> 11)",
     bty   = "n",
     xaxt  = "n",
     yaxt  = "n",
     type  = "p",
     cex   = .8,
     col = ifelse(resultsSupp2$S1mean>200, "red", "black"),
     pch = 19,
     # col=ifelse(resultsSupp2$subject %in% c(10,34,35,44,49,53,27,58,79,102,105,117,121,131), "red", "black"),
     # pch=ifelse(resultsSupp2$subject %in% c(10,34,35,44,49,53,27,58,79,102,105,117,121,131), 19, 20)
     )

axis(side = 1, at = c(10,100,1000))
axis(side = 2, at = c(10,100,1000), las=1)

# lines(0,1)
abline(a = 0,
       b = 1,
       lwd = .4,
       lty = 2)

# install.packages("calibrate")
# library(calibrate)
# textxy(resultsSupp2$S1mean, resultsSupp2$S4mean,
#        labs = resultsSupp2$subject,
#        cex = .6,
#        offset = 1)

# 24. Melnick et al. (2013) ------------------------------------
# 24.1 raw SIs of Melnick et al.'s study 2
dujeSI <- c(0.362176436,0.702660283,0.344620084,0.290644562,0.149251118,
            0.484738763,0.349264063,0.115494802,0.173536825,0.479335855,
            0.386266659,0.145451501,0.294042028,0.413284945,0.23903658,
            0.153098429,0.44774094,0.028228388,0.448190377,0.190441108,
            0.204717546,0.374748843,0.260663706,0.543388369,0.396705579,
            0.166746954,0.128258397,0.183556697,0.216251613,0.668146298,
            0.232097077,0.500830966,0.419319821,0.209205028,0.201201615,
            0.357834169,0.617245338,0.288727244,0.226376473,0.175228586,
            0.270472329,0.29887317,0.474161315,0.280192418,0.53771961,
            0.216466428,0.320808272,0.294487441,0.263583725,0.016305304,
            0.470331552,0.537914732,0.409208243)

# XX Dotchart of Melnick et al.'s values
# dotchart(dujeSI,
#          cex = .7,
#          xlab = "Suppression Index [log10]",
#          main = "Streuung der Suppression Indizes Melnick study 2",
#          bty = "n")

# XX. Compare Melnick et al.'s & our densities --------------------------------
# par(mfrow=c(1,2))
# plot(density(dujeSI, bw = .06315),
#      main = "Melnick et al. study 2 density",
#      col = 2,
#      xlim = c(-.2,1),
#      ylim = c(0,5),
#      xaxt = "n",
#      yaxt = "n",
#      bty= "n")
# axis(side = 1, at = seq(-.2,1,.2))
# axis(side = 2, at = seq(0,5,1),las=2)
#
# plot(density(resultsSupp2$SI2, bw = .06315),
#      main = "our density",
#      col = 4,
#      xlim = c(-.2,1),
#      ylim = c(0,5),
#      xaxt = "n",
#      yaxt = "n",
#      bty = "n")
# axis(side = 1, at = seq(-.2,1,.2))
# axis(side = 2, at = seq(0,5,1),las=2)
# par(mfrow=c(1,1))

# .... 24.2 Histogram plot of Melnick et al. (2013) data ----------------------------
par(mfrow=c(2,1))
hist(dujeSI,
     breaks = 80000,
     main = "Histogram of Melnick et al. (2013), study 2, N = 53",
     ylim = c(0,4),
     xlim = c(-.2,1),
     xaxt = "n",
     yaxt = "n",
     xlab = expression(paste("Suppression indices [", log[10](large~~stimulus~~threshold) - log[10](small~~stimulus~~threshold), "]")),
     freq = TRUE,
     col = 8,
     border = NULL)

axis(side = 1, at = seq(-.1,1,.1))
axis(side = 2, at = seq(0,4,1), las = 2)

lines(density(dujeSI, bw=.02), col=8, lwd = 1.5)


# .... 24.3 Histogram plot of bern data ----------------------------
hist(resultsSupp2$SI2,
     breaks = 80000,
     main = paste("Histogram of Bern data, N = ", nrow(resultsSupp2), sep = ""),
     ylim = c(0,4),
     xlim = c(-.2,1),
     xaxt = "n",
     yaxt = "n",
     xlab = expression(paste("Suppression indices [", log[10](large~~stimulus~~threshold) - log[10](small~~stimulus~~threshold), "]")),
     freq = TRUE,
     col = 4,
     border = NULL)

axis(side = 1, at = seq(-.1,1,.1))
axis(side = 2, at = seq(0,4,1), las = 2)

lines(density(resultsSupp2$SI2, bw=.02), col=4, lwd = 1)
par(mfrow=c(1,1))

# .... 24.4 Density plots -----------------------------------------------------------
plot(density(dujeSI, bw = .02),#.06315),
     main = "Melnick et al. (2013) vs Bern data",
     col = 1,
     lty = 0,
     xlim = c(-.2,1),
     ylim = c(0,5),
     xaxt = "n",
     yaxt = "n",
     xlab = expression(paste("Suppression indices [", log[10](large~~stimulus~~threshold) - log[10](small~~stimulus~~threshold), "]")),
     ylab = "",
     bty = "n")
axis(side = 1, at = seq(-.2,1,.2))
# axis(side = 2, at = seq(0,5,1),las=2)

polygon(density(dujeSI, bw = .02),
        col = "8",
        density = 80)

polygon(density(resultsSupp2$SI2, bw = .02),
        col = "4",
        density = 30)

text(.65,2,"Melnick et al. (2013), study 2, N = 53", col=8, cex=1)
text(.2,3.75,labels= paste("Bern data, N = ", nrow(resultsSupp2), sep = ""), col=4, cex=1)

# 25. Create new dataframe for intercept, slope & rsquare of each regression----
numberOfFiles <- length(resultsSupp2$subject)

intslop <- data.frame(matrix(0,numberOfFiles,4))

intslop <- rename(intslop,
                  subject   = X1,
                  Sinter    = X2,
                  Sslope    = X3,
                  Srsquared = X4)

# .... 25.1 Regression loop ------------------------------------------------------
cond <- c(1.8, 3.6, 5.4, 7.2)
allSubjects <- resultsSupp2$subject

for (i in allSubjects)
{intslop[i,1] <- i

yvalues <- t(subset(resultsSupp2,
                   subject == i,
                   select = c(S1mean, S2mean, S3mean, S4mean)))

expFunction <- function(x,intercept,slope){I(intercept*exp(slope*x))}

nlsFit <- nls(yvalues ~ expFunction(cond, intercept, slope), start = list(intercept = 20, slope = .01))

intercept <- coef(nlsFit)[1]
slope     <- coef(nlsFit)[2]

SSres     <- sum(residuals(nlsFit)^2)            # Residual sum of squares
SStot     <- sum((yvalues - mean(yvalues))^2)    # Total sum of squares
SSmod     <- (SStot-SSres)                       # Model sum of squares
rsquared  <- SSmod/SStot                         # R-squared measure
  
intslop[i,2] <- round(intercept, digits = 0)
intslop[i,3] <- round(slope, digits = 4)
intslop[i,4] <- round(rsquared, digits = 3)
}

# .... 25.2 Merge 'intslop' dataframe with existing dataframe -------------------
resultsSupp2 <- merge(resultsSupp2, intslop,
                     by = "subject")

# 26. Suppression Index summary ---------------------------------------------
sapply(subset(resultsSupp2, select = c(S1mean,S2mean,S3mean,S4mean)), mean)
summary(resultsSupp2$SI2)
mean(dujeSI)
mean(resultsSupp2$SI2)
sd(dujeSI)
sd(resultsSupp2$SI2)
corr.test(resultsSupp2[4:7],method = "spearman")
summary(resultsSupp2$exp2)
nrow(resultsSupp2)

qplot(resultsSupp2$exp2)

# XXXXXXXX. Exclude subjects --------------------------------------------------------

# subset(resultsSupp2, subject %in% c(20,21,22,27,30))



# Write excel file -----------------
# write.xlsx(results, "SuppressionTask.xlsx")
# # write.xlsx(SuppDuje, "SuppressionTask.xlsx")
# rm(list = setdiff(ls(), c("resultsHick", "H0rtAllCorr", "H1rtAllCorr", "H2rtAllCorr", "H258rtAllCorr",
#                         "resultsSupp2","resultsBIS")))
# cat("\014")
