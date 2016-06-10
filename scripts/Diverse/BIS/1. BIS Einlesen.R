################################################################################
################################################################################
###                                                                             
###   R E A D   BIS Daten einlesen 
###
###   Philipp Thomas, 18. Januar 2016



#library(xlsx)          #<- needed to run analysis
#library(dyplr)         #<- needed to run analysis

# 0.  Preparatory work ---------------------------------------------------------
# 0.1 Set working directory (wd) to your folder if needed                                                   
# setwd("/Users/philippthomas/Documents/Uni Bern/PDD/Dissertation/Daten/Project/data/base/BIS")

# 1. read in the xlsx file -----------------------------------------------------
#dat <- read.xlsx("Fragebogen.xlsx",1) # or line below
#dat1 <- read.xlsx(file = "data/base/BIS/BIS.xlsx", 1) # or faster read_excel function
library(readxl)
dat2 <- read_excel(path = "data/base/BIS/BIS.xlsx", 1)

# all.equal(dat1, dat2)
# identical(dat1, dat2)

# 2. Retain only complete columns (no NAs) ---------------------------------------
dat <- dat2[complete.cases(dat2),]

# 3. Remove all whitespaces -----------------------------------------------
dat <- as.data.frame(apply(dat,2,function(x)gsub("\\s+", "",x)))

# 4. Convert factors to numeric ------------------------------------------------
# .... 4.1 Convert factors to numeric ------------------------------------------
as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}
dat[c(1,9:54)] <- lapply(dat[c(1,9:54)], as.numeric.factor)
dat <- droplevels(dat)
str(dat)

# .... 4.2 Rename factor labels ------------------------------------------------
table(dat$vision)
levels(dat$vision) <- c("good", "medium", "poor")
table(dat$vision)

table(dat$hearing)
levels(dat$hearing) <- c("good", "medium", "poor")
table(dat$hearing)

table(dat$disease)
levels(dat$disease) <- c("yes", "no")
table(dat$disease)

table(dat$drug)
levels(dat$drug) <- c("yes","no")
table(dat$drug)

table(dat$smoking)
levels(dat$smoking) <- c("yes","no")
table(dat$smoking)

table(dat$exp1)
levels(dat$exp1) <- c("Daniele","Dominic","JasminS","Philipp")
table(dat$exp1)

# 5. Create dataframe & clean up -----------------------------------------------
resultsBIS <- dat
rm(dat)
rm(as.numeric.factor)
resultsBIS <- arrange(resultsBIS, subject)
resultsBIS <- subset(resultsBIS, select = -weg)

# 5.1 Drop subjects because of invalid (i.e. 0) XG raw scores -----------------------------
resultsBIS <- subset(resultsBIS, !subject %in% c(54,72,98,160))

# 6. Create z-standardized values ----------------------------------------------
# 6.1 Scale raw scores into z-scores 
resultsBIS$zAN <- scale(resultsBIS$ANraw)
resultsBIS$zBD <- scale(resultsBIS$BDraw)
resultsBIS$zCH <- scale(resultsBIS$CHraw)
resultsBIS$zKW <- scale(resultsBIS$KWraw)
resultsBIS$zOE <- scale(resultsBIS$OEraw)
resultsBIS$zOG <- scale(resultsBIS$OGraw)
resultsBIS$zRZ <- scale(resultsBIS$RZraw)
resultsBIS$zSC <- scale(resultsBIS$SCraw)
resultsBIS$zST <- scale(resultsBIS$STraw)
resultsBIS$zTG <- scale(resultsBIS$TGraw)
resultsBIS$zTM <- scale(resultsBIS$TMraw)
resultsBIS$zUW <- scale(resultsBIS$UWraw)
resultsBIS$zWA <- scale(resultsBIS$WAraw)
resultsBIS$zWE <- scale(resultsBIS$WEraw)
resultsBIS$zWM <- scale(resultsBIS$WMraw)
resultsBIS$zXG <- scale(resultsBIS$XGraw)
resultsBIS$zZN <- scale(resultsBIS$ZNraw)
resultsBIS$zZP <- scale(resultsBIS$ZPraw)
resultsBIS$zZZ <- scale(resultsBIS$ZZraw)

# 7. Create mean z-scores  -----------------------------------------------------
resultsBIS$zNumeric  <- rowMeans(subset(resultsBIS, select = c(zRZ, zSC, zXG, zZN, zZP, zZZ)))
resultsBIS$zVerbal   <- rowMeans(subset(resultsBIS, select = c(zST, zWA, zTG, zTM, zWM, zKW)))
resultsBIS$zFigural  <- rowMeans(subset(resultsBIS, select = c(zAN, zBD, zOE, zOG, zWE, zCH)))
resultsBIS$zSpeed    <- rowMeans(subset(resultsBIS, select = c(zOE, zBD, zTG, zRZ, zKW, zXG)))
resultsBIS$zCapacity <- rowMeans(subset(resultsBIS, select = c(zAN, zCH, zWA, zTM, zZN, zSC)))
resultsBIS$zMemory   <- rowMeans(subset(resultsBIS, select = c(zOG, zWE, zST, zWM, zZP, zZZ)))

resultsBIS$zSpeedNumeric    <- rowMeans(subset(resultsBIS, select = c(zXG, zRZ)))
resultsBIS$zSpeedVerbal     <- rowMeans(subset(resultsBIS, select = c(zTG, zKW)))
resultsBIS$zSpeedFigural    <- rowMeans(subset(resultsBIS, select = c(zBD, zOE)))

resultsBIS$zCapacityNumeric <- rowMeans(subset(resultsBIS, select = c(zZN, zSC)))
resultsBIS$zCapacityVerbal  <- rowMeans(subset(resultsBIS, select = c(zWA, zTM)))
resultsBIS$zCapacityFigural <- rowMeans(subset(resultsBIS, select = c(zAN, zCH)))

resultsBIS$zMemoryNumeric   <- rowMeans(subset(resultsBIS, select = c(zZP, zZZ)))
resultsBIS$zMemoryVerbal    <- rowMeans(subset(resultsBIS, select = c(zST, zWM)))
resultsBIS$zMemoryFigural   <- rowMeans(subset(resultsBIS, select = c(zOG, zWE)))

resultsBIS$zTotal <- rowMeans(subset(resultsBIS, select = c(zRZ, zSC, zXG, zZN, zZP, zZZ,
                                                            zST, zWA, zTG, zTM, zWM, zKW,
                                                            zAN, zBD, zOE, zOG, zWE, zCH,
                                                            zOE, zBD, zTG, zRZ, zKW, zXG,
                                                            zAN, zCH, zWA, zTM, zZN, zSC,
                                                            zOG, zWE, zST, zWM, zZP, zZZ)))

# 8. Rearrange columns and remove variables ------------------------------------
names(resultsBIS)
names(resultsBIS[,c(1:8,
                    9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39,41,43,45,
                    10,12,14,16,18,20,22,24,26,28,30,32,34,36,38,40,42,44,46,
                    54:72,
                    79:87,
                    73:78,
                    88,
                    47:52,
                    53
                    )])

resultsBIS <- resultsBIS[,c(1:8,
                    9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39,41,43,45,
                    10,12,14,16,18,20,22,24,26,28,30,32,34,36,38,40,42,44,46,
                    54:72,
                    79:87,
                    73:78,
                    88,
                    47:52,
                    53
                    )]

resultsBIS <- subset(resultsBIS, select = -c(ANraw,BDraw,CHraw,KWraw,OEraw,OGraw,
                                             RZraw,SCraw,STraw,TGraw,TMraw,UWraw,
                                             WAraw,WEraw,WMraw,XGraw,ZNraw,ZPraw,
                                             ZZraw,
                                             ANcf,BDsf,CHcf,KWsv,OEsf,OGmf,
                                             RZsn,SCcn,STmv,TGsv,TMcv,UWsv,
                                             WAcv,WEmf,WMmv,XGsn,ZNcn,ZPmn,
                                             ZZmn,C,S,M,V,N,F,sumVNF))


# # 7. BIS data Oli ----------------------------------------------------------
# olizBIS <- c(-0.025870687,  0.975029078  ,0.430234696, -0.531584574,-0.111529375, -0.721289579,
# 0.227895758 , 0.358834001  ,0.099534600 ,-0.330806311 , 0.355621656  ,0.393366559,
# .147546207 ,-0.649636558 ,-0.423120318  ,0.491346455 ,-0.381076240 ,-0.527171681,
# .636758765  ,0.586623934 ,-0.124115887  ,0.333690763,  0.144245139 ,-1.154826207,
# .573904199  ,0.112816789  ,0.619189742  ,0.556190830,  0.217429042 ,-0.388037353,
# -0.884098602,  0.275914842 ,-0.634887530,  0.622320893,  0.959231698, -0.336015182,
# 1.034513647 , 1.101839419  ,0.003933568 ,-0.127067688 , 0.194625011 , 0.615420725,
# 0.008092141 , 0.631654908  ,0.541450687 ,-0.678500874 , 0.031344018 , 0.302432778,
# 0.031866609 , 0.845918672  ,0.646375983 , 0.313581690 ,-0.199072499 ,-0.334494336,
# -0.408025974,  0.418298842,  0.971173379, -0.301318297, -0.403738107,  0.009952210,
# 0.195534581 ,-0.206145204 ,-0.120329691 , 0.472471229 ,-0.999734233 , 0.659579568,
# -0.377854087,  0.573324668, -0.120695201,  0.162233338,  0.529905301,  0.470708876,
# 0.200747357 , 0.216044259 , 0.100922189 , 0.632765251 , 0.261540387 ,-0.232127141,
# -0.194741595,  0.418281254, -0.729781370, -0.194750928,  0.457608801,  0.114241876,
# -0.485485115, -0.132078594, -0.318299056, -0.788598255,  0.013570292, -0.586539010,
# 0.125819104 ,-0.426580765 , 1.007791469 ,-0.490950057 ,-0.295499213 ,-0.097564302,
# -0.463842261, -0.529995631, -0.979030747, -0.560062685,  0.472449939, -0.295779440,
# -0.709897994,  0.026461424, -0.175892477, -0.339103062, -0.475608268, -0.314746912,
# -0.530543283, -0.337824834,  0.220933610,  0.937995166, -0.111731637, -0.923352675,
# 0.046740689 ,-0.144389315 ,-0.716643998 ,-0.134934794 ,-0.393420975 , 0.073386156,
# -0.164174528, -0.351700049, -0.279635539, -0.192526125, -0.279136198,  0.500903002,
# 0.342469707 , 0.305751548 ,-0.601632418 , 0.252544865 , 0.327101623 ,-0.870553346,
# -0.240744772, -0.134178756,  0.345026665, -0.620323730, -0.185206074, -0.156293956,
# -0.811193029, -0.509329768, -0.322123331,  0.060587619, -0.463788490, -0.872752739,
# 0.262460208 , 0.535863608 , 0.913885840 ,-0.719811317 ,-1.002631394 ,-0.051542801,
# -0.049331706, -0.461451658, -0.123545876, -0.846142492,  0.131964445, -0.038131581,
# -0.106219132,  0.162432027, -0.173589153,  0.213225923, -0.946015064, -0.929531936,
# 0.085009748 ,-0.255330628 ,-0.440682808 ,-0.458568137 , 1.052159794 ,-0.116064568,
# 0.863793416 ,-0.643593478 ,-0.240571446 , 0.112357403 ,-0.107641342 ,-0.976959210,
# -1.118288099, -0.152264445, -0.343592820,  1.142000431, -0.277599197,  1.488204063,
# 0.557814920 , 0.890303595 , 0.509340012 ,-0.161505955 , 0.662618843 , 1.518552363,
# 0.102367742 ,-0.274434625 , 0.395381278 ,-0.156992931 ,-0.105595842 ,-0.208856963,
# 0.859866172 , 0.484318276 , 0.473997808 , 0.560052459 , 0.873969517 ,-0.288636505,
# 0.030004553 , 0.491692056 , 0.076267356 ,-0.479437784 ,-0.809545014 ,-0.176426389,
# -0.943300946,  0.245836642, -0.129335612,  0.102602514,  0.112936780, -0.784988768,
# -0.239049296,  0.931118254,  0.100884057, -0.580515197,  1.045712855,  0.752234170,
# 0.852419020 , 0.828015948 , 0.452640283 , 1.582213139 , 0.638773659 , 1.408118322,
# -0.226607923,  0.193069188, -0.512694078, -0.797566378,  0.007118835, -0.641116666,
# 0.267651802 , 1.067125483 , 0.422057950 , 0.531585675 , 0.635083356 , 0.776888608,
# -0.975582261, -1.540547419, -0.447991636, -0.427533657, -1.213460751, -0.534113621,
# -0.848564127)
# 
# # 10. Summary -------------------------------------------------------------
# 

# 10. Plot operations and contents side by side --------------------------------
# .... 10.1 Speed --------------------------------------------------------------
par(mfrow=c(1,3))
plot(resultsBIS$zSpeedNumeric,
     main = paste("speed numeric (standardized), N = ", nrow(resultsBIS), sep = ""),
     ylim = c(-3,3),
     xlim = c(0,200),
     # yaxt = "",
     ylab = "z-value",
     xlab = "Subject",
     col = ifelse(resultsBIS$subject == 138, "red", "black"),
     pch = ifelse(resultsBIS$subject == 138, 19, 1),
     cex = ifelse(resultsBIS$subject == 138, 2,1),
     bty = "n",
     las = 1)

plot(resultsBIS$zSpeedFigural,
     main = paste("speed figural (standardized), N = ", nrow(resultsBIS), sep = ""),
     ylim = c(-3,3),
     xlim = c(0,200),
     # yaxt = "",
     ylab = "",
     xlab = "Subject",
     col = ifelse(resultsBIS$subject == 138, "red", "black"),
     pch = ifelse(resultsBIS$subject == 138, 19, 1),
     cex = ifelse(resultsBIS$subject == 138, 2,1),
     bty = "n",
     las = 1)

plot(resultsBIS$zSpeedVerbal,
     main = paste("speed verbal (standardized), N = ", nrow(resultsBIS), sep = ""),
     ylim = c(-3,3),
     xlim = c(0,200),
     # yaxt = "",
     ylab = "",
     xlab = "Subject",
     col = ifelse(resultsBIS$subject == 138, "red", "black"),
     pch = ifelse(resultsBIS$subject == 138, 19, 1),
     cex = ifelse(resultsBIS$subject == 138, 2,1),
     bty = "n",
     las = 1)
par(mfrow=c(1,1))

# .... 10.2 Capacity------------------------------------------------------------
par(mfrow=c(1,3))
plot(resultsBIS$zCapacityNumeric,
     main = paste("capacity numeric (standardized), N = ", nrow(resultsBIS), sep = ""),
     ylim = c(-3,3),
     xlim = c(0,200),
     # yaxt = "",
     ylab = "z-value",
     xlab = "Subject",
          col = ifelse(resultsBIS$subject == 138, "red", "black"),
     pch = ifelse(resultsBIS$subject == 138, 19, 1),
     cex = ifelse(resultsBIS$subject == 138, 2,1),
     bty = "n",
     las = 1)

plot(resultsBIS$zCapacityFigural,
     main = paste("capacity figural (standardized), N = ", nrow(resultsBIS), sep = ""),
     ylim = c(-3,3),
     xlim = c(0,200),
     # yaxt = "",
     ylab = "",
     xlab = "Subject",
          col = ifelse(resultsBIS$subject == 138, "red", "black"),
     pch = ifelse(resultsBIS$subject == 138, 19, 1),
     cex = ifelse(resultsBIS$subject == 138, 2,1),
     bty = "n",
     las = 1)

plot(resultsBIS$zCapacityVerbal,
     main = paste("capacity verbal (standardized), N = ", nrow(resultsBIS), sep = ""),
     ylim = c(-3,3),
     xlim = c(0,200),
     # yaxt = "",
     ylab = "",
     xlab = "Subject",
          col = ifelse(resultsBIS$subject == 138, "red", "black"),
     pch = ifelse(resultsBIS$subject == 138, 19, 1),
     cex = ifelse(resultsBIS$subject == 138, 2,1),
     bty = "n",
     las = 1)
par(mfrow=c(1,1))

# .... 10.3 Memory ------------------------------------------------------------
par(mfrow=c(1,3))
plot(resultsBIS$zMemoryNumeric,
     main = paste("memory numeric (standardized), N = ", nrow(resultsBIS), sep = ""),
     ylim = c(-3,3),
     xlim = c(0,200),
     # yaxt = "",
     ylab = "z-value",
     xlab = "Subject",
          col = ifelse(resultsBIS$subject == 138, "red", "black"),
     pch = ifelse(resultsBIS$subject == 138, 19, 1),
     cex = ifelse(resultsBIS$subject == 138, 2,1),
     bty = "n",
     las = 1)

plot(resultsBIS$zMemoryFigural,
     main = paste("memory figural (standardized), N = ", nrow(resultsBIS), sep = ""),
     ylim = c(-3,3),
     xlim = c(0,200),
     # yaxt = "",
     ylab = "",
     xlab = "Subject",
          col = ifelse(resultsBIS$subject == 138, "red", "black"),
     pch = ifelse(resultsBIS$subject == 138, 19, 1),
     cex = ifelse(resultsBIS$subject == 138, 2,1),
     bty = "n",
     las = 1)

plot(resultsBIS$zMemoryVerbal,
     main = paste("memory verbal (standardized), N = ", nrow(resultsBIS), sep = ""),
     ylim = c(-3,3),
     xlim = c(0,200),
     # yaxt = "",
     ylab = "",
     xlab = "Subject",
          col = ifelse(resultsBIS$subject == 138, "red", "black"),
     pch = ifelse(resultsBIS$subject == 138, 19, 1),
     cex = ifelse(resultsBIS$subject == 138, 2,1),
     bty = "n",
     las = 1)
par(mfrow=c(1,1))

# 11. Plot operations and contents side by side --------------------------------
# .... 11.1 Speed Capacity Memory --------------------------------------------
par(mfrow=c(1,3))
plot(resultsBIS$zSpeed,
     main = paste("speed (standardized), N = ", nrow(resultsBIS), sep = ""),
     ylim = c(-3,3),
     xlim = c(0,200),
     # yaxt = "",
     ylab = "z-value",
     xlab = "Subject",
          col = ifelse(resultsBIS$subject == 138, "red", "black"),
     pch = ifelse(resultsBIS$subject == 138, 19, 1),
     cex = ifelse(resultsBIS$subject == 138, 2,1),
     bty = "n",
     las = 1)

plot(resultsBIS$zCapacity,
     main = paste("capacity (standardized), N = ", nrow(resultsBIS), sep = ""),
     ylim = c(-3,3),
     xlim = c(0,200),
     # yaxt = "",
     ylab = "",
     xlab = "Subject",
          col = ifelse(resultsBIS$subject == 138, "red", "black"),
     pch = ifelse(resultsBIS$subject == 138, 19, 1),
     cex = ifelse(resultsBIS$subject == 138, 2,1),
     bty = "n",
     las = 1)

plot(resultsBIS$zMemory,
     main = paste("Memory (standardized), N = ", nrow(resultsBIS), sep = ""),
     ylim = c(-3,3),
     xlim = c(0,200),
     # yaxt = "",
     ylab = "",
     xlab = "Subject",
          col = ifelse(resultsBIS$subject == 138, "red", "black"),
     pch = ifelse(resultsBIS$subject == 138, 19, 1),
     cex = ifelse(resultsBIS$subject == 138, 2,1),
     bty = "n",
     las = 1)
par(mfrow=c(1,1))

# .... 11.1 Numeric, Figural, Verbal --------------------------------------------
par(mfrow=c(1,3))
plot(resultsBIS$zNumeric,
     main = paste("numeric (standardized), N = ", nrow(resultsBIS), sep = ""),
     ylim = c(-3,3),
     xlim = c(0,200),
     # yaxt = "",
     ylab = "z-value",
     xlab = "Subject",
          col = ifelse(resultsBIS$subject == 138, "red", "black"),
     pch = ifelse(resultsBIS$subject == 138, 19, 1),
     cex = ifelse(resultsBIS$subject == 138, 2,1),
     bty = "n",
     las = 1)

plot(resultsBIS$zFigural,
     main = paste("figural (standardized), N = ", nrow(resultsBIS), sep = ""),
     ylim = c(-3,3),
     xlim = c(0,200),
     # yaxt = "",
     ylab = "",
     xlab = "Subject",
          col = ifelse(resultsBIS$subject == 138, "red", "black"),
     pch = ifelse(resultsBIS$subject == 138, 19, 1),
     cex = ifelse(resultsBIS$subject == 138, 2,1),
     bty = "n",
     las = 1)

plot(resultsBIS$zVerbal,
     main = paste("verbal (standardized), N = ", nrow(resultsBIS), sep = ""),
     ylim = c(-3,3),
     xlim = c(0,200),
     # yaxt = "",
     ylab = "",
     xlab = "Subject",
          col = ifelse(resultsBIS$subject == 138, "red", "black"),
     pch = ifelse(resultsBIS$subject == 138, 19, 1),
     cex = ifelse(resultsBIS$subject == 138, 2,1),
     bty = "n",
     las = 1)
par(mfrow=c(1,1))
# 
# # 12. Densities ---------------------- 
# # .... 12.1 Speed ---------------
# 
# 
# par(mfrow=c(3, 2))
# plot(density(resultsBIS$zSpeed),
#      main = "Dichte der z-standardisierten Speed Werte",
#      col = 1,
#      xlim = c(-3,3),
#      ylim = c(0,1),
#      xaxt = "n",
#      yaxt = "n",
#      xlab = "z-Wert",
#      ylab = "",
#      bty = "n",
#      lwd = 0)
# polygon(density(resultsBIS$zSpeed, bw = .1),
#         col = "blue",
#         density = 30)
# axis(side = 1, at = seq(-3,3,.5))
# abline(v=0, lty = 3)
# 
# # .... 12.2 Capacity -----------------------------------------------------------
# plot(density(resultsBIS$zCapacity),
#      main = "Dichte der z-standardisierten Capacity Werte",
#      col = 1,
#      xlim = c(-3,3),
#      ylim = c(0,1),
#      xaxt = "n",
#      yaxt = "n",
#      xlab = "z-Wert",
#      ylab = "",
#      bty = "n",
#      lwd = 0)
# polygon(density(resultsBIS$zCapacity, bw = .1),
#         col = "blue",
#         density = 30)
# axis(side = 1, at = seq(-3,3,.5))
# abline(v=0, lty = 3)
# 
# # .... 12.3 Memory -------------------------------------------------------------
# plot(density(resultsBIS$zMemory),
#      main = "Dichte der z-standardisierten Memory Werte",
#      col = 1,
#      xlim = c(-3,3),
#      ylim = c(0,1),
#      xaxt = "n",
#      yaxt = "n",
#      xlab = "z-Wert",
#      ylab = "",
#      bty = "n",
#      lwd = 0)
# polygon(density(resultsBIS$zMemory, bw = .1),
#         col = "blue",
#         density = 30)
# axis(side = 1, at = seq(-3,3,.5))
# abline(v=0, lty = 3)
# 
# 
# 
# 
# # .... 12.3 Numeric ------------------------------------------------------------
# plot(density(resultsBIS$zNumeric),
#      main = "Dichte der z-standardisierten Numeric Werte",
#      col = 1,
#      xlim = c(-3,3),
#      ylim = c(0,1),
#      xaxt = "n",
#      yaxt = "n",
#      xlab = "z-Wert",
#      ylab = "",
#      bty = "n",
#      lwd = 0)
# polygon(density(resultsBIS$zNumeric, bw = .1),
#         col = "blue",
#         density = 30)
# axis(side = 1, at = seq(-3,3,.5))
# abline(v=0, lty = 3)
# 
# 
# # .... 12.3 Figural ------------------------------------------------------------
# plot(density(resultsBIS$zFigural),
#      main = "Dichte der z-standardisierten Figural Werte",
#      col = 1,
#      xlim = c(-3,3),
#      ylim = c(0,1),
#      xaxt = "n",
#      yaxt = "n",
#      xlab = "z-Wert",
#      ylab = "",
#      bty = "n",
#      lwd = 0)
# polygon(density(resultsBIS$zFigural, bw = .1),
#         col = "blue",
#         density = 30)
# axis(side = 1, at = seq(-3,3,.5))
# abline(v=0, lty = 3)
# 
# # .... 12.3 Verbal -------------------------------------------------------------
# plot(density(resultsBIS$zVerbal),
#      main = "Dichte der z-standardisierten Verbal Werte",
#      col = 1,
#      xlim = c(-3,3),
#      ylim = c(0,1),
#      xaxt = "n",
#      yaxt = "n",
#      xlab = "z-Wert",
#      ylab = "",
#      bty = "n",
#      lwd = 0)
# polygon(density(resultsBIS$zVerbal, bw = .1),
#         col = "blue",
#         density = 30)
# axis(side = 1, at = seq(-3,3,.5))
# abline(v=0, lty = 3)
# 
# par(mfrow=c(1, 1))
# # 7. BIS data Oli ----------------------------------------------------------
# # oliBIS <- c(1428,1483,1500,1506,1520,1523,1525,1527,1534,1537,1541,1543,1549,
# #             1549,1550,1550,1551,1557,1557,1562,1568,1569,1570,1574,1575,1576,
# #             1576,1583,1586,1590,1592,1596,1597,1599,1600,1601,1602,1610,1610,
# #             1610,1610,1611,1617,1617,1618,1618,1619,1621,1622,1624,1624,1628,
# #             1630,1630,1631,1632,1633,1633,1634,1634,1635,1638,1638,1640,1640,
# #             1641,1644,1647,1648,1650,1652,1653,1653,1655,1657,1657,1657,1660,
# #             1662,1663,1664,1664,1664,1667,1668,1670,1672,1674,1674,1675,1675,
# #             1675,1677,1678,1679,1679,1680,1681,1682,1682,1683,1683,1683,1687,
# #             1688,1689,1690,1691,1691,1692,1692,1693,1693,1694,1694,1694,1698,
# #             1698,1699,1701,1702,1703,1706,1708,1708,1714,1716,1717,1720,1720,
# #             1724,1725,1726,1726,1728,1729,1731,1731,1732,1732,1734,1736,1737,
# #             1737,1716,1741,1741,1741,1743,1739,1746,1746,1748,1748,1751,1753,
# #             1759,1760,1761,1761,1762,1763,1765,1767,1769,1769,1770,1772,1772,
# #             1775,1780,1780,1781,1782,1782,1782,1792,1792,1795,1797,1797,1797,
# #             1800,1803,1806,1806,1807,1808,1809,1811,1812,1812,1812,1814,1815,
# #             1816,1817,1819,1820,1820,1827,1828,1828,1829,1831,1831,1831,1837,
# #             1838,1838,1840,1842,1843,1846,1849,1852,1854,1866,1868,1870,1875,
# #             1877,1892,1892,1893,1894,1899,1903,1904,1905,1909,1912,1916,1923,
# #             1923,1925,1927,1931,1942,1993,2021,2026,2033)
# 
# 
# 
# plot(density(olizBIS, bw = .1),
#      main = "Dichte der BIS Werte",
#      col = 1,
#      xlim = c(-2,2),
#      ylim = c(0,1),
#      xaxt = "n",
#      yaxt = "n",
#      xlab = "BIS z-Werte über alle 18 Subtests gemittelt",
#      ylab = "",
#      bty = "n",
#      lwd = 0)
# 
# axis(side = 1, at = seq(-2,2,.5))
# polygon(density(olizBIS, bw = .1),
#         col = "8",
#         density = 60)
# 
# lines(density(resultsBIS$zTotal, bw = .1))
# polygon(density(resultsBIS$zTotal,
#                 bw = .1),
#         col = "blue",
#         density = 20)
# 
# text(1,.4,labels= "Oli's Daten, N = 241", col=8, cex=1)
# text(.4,.85,labels= paste("Unsere Daten, N = ", nrow(resultsBIS), sep = ""), col=4, cex=1)
# 
# 
# # Split by sex ------------------------------------------------------------
# # a <- merge(resultsBIS, resultsFragebogen, by = "subject")
# # male <- subset(a, sex == "male")
# # female <- subset(a, sex == "female")
# # 
# # plot(density(male$zTotal, bw = .1),
# #      main = "zTotal values",
# #      col = 1,
# #      xlim = c(-2,2),
# #      ylim = c(0,1),
# #      xaxt = "n",
# #      yaxt = "n",
# #      xlab = "BIS z-Werte über alle 18 Subtests gemittelt",
# #      ylab = "",
# #      bty = "n",
# #      lwd = 0)
# # 
# # axis(side = 1, at = seq(-2,2,.5))
# # polygon(density(male$zTotal, bw = .1),
# #         col = "blue",
# #         density = 30)
# # 
# # 
# # polygon(density(female$zTotal,
# #                 bw = .1),
# #         col = "red",
# #         density = 30)
# # 
# # text(1,.4,labels= paste("female, N = ", nrow(female), sep = ""), col="red", cex=1)
# # text(.4,.95,labels= paste("male, N = ", nrow(male), sep = ""), col="blue", cex=1)
# 
# 
# 
# 
# 
# 
# 
# 
# # 10. histograaaaam --------------------------------------------------------
# par(mfrow=c(2, 1))
# hist(olizBIS,
#      breaks = 100000,
#      freq = TRUE,
#      col = "grey",
#      main = "Oli's BIS Daten, N = 241",
#      xlab = "BIS z-Werte über alle 18 Subtests gemittelt",
#      ylab = "Frequency",
#      xlim = c(-2,2),
#      ylim = c(0,3),
#      axes = FALSE
#      )
# 
# axis(side = 2, at = seq(0,3,1), las =2)
# axis(side = 1, at = seq(-2,2,1))
# 
# # polygon(density(olizBIS, bw = .1),
# #         col = "8",
# #         density = 60)
# 
# 
# hist(resultsBIS$zTotal,
#      breaks = 100000,
#      freq = TRUE,
#      col = "grey",
#      main = paste("Unsere BIS Daten, N = ", nrow(resultsBIS), sep = ""),
#      xlab = "BIS z-Werte über alle 18 Subtests gemittelt",
#      ylab = "Frequency",
#      xlim = c(-2,2),
#      ylim = c(0,3),
#      axes = FALSE
#      )
# 
# axis(side = 2, at = seq(0,3,1), las =2)
# axis(side = 1, at = seq(-2,2,1))
# 
# # polygon(density(resultsBIS$zTotal,
# #                 bw = .1),
# #         col = "blue",
# #         density = 20)
# 
# 
# par(mfrow=c(1, 1))

# Cleaning up -----------------------------------------------------------------------
# rm(list = setdiff(ls(), c("resultsHick", "H0rtAllCorr", "H1rtAllCorr", "H2rtAllCorr", "H258rtAllCorr",
#                         "resultsSupp2","resultsBIS")))
# cat("\014")

# resultsBIS <- subset(resultsBIS, !subject == 138)


# save(resultsBIS, file="data/processed/resultsBIS.rda")
