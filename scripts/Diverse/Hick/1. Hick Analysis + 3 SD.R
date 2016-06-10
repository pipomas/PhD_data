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
###    Philipp Thomas, 4. Februar 2016
###    
###    Version:
###    1. use only correct trials
###    2. exclude RTs < 100 & > 2500 ms
###    3. exclude RTs based on intraindividual variability,
###       i.e., all RTs > + 3 SDs
###
###
###

library(rprime)          #<- needed to run analysis

# 0. Preparatory work ----------------------------------------------------------
allFiles <- as.list(paste("data/base/Hick",
                          list.files(path = "data/base/Hick"),
                          sep = "/"))
numberOfFiles <- length(allFiles)

# 1. Import txt files and create data frames -----------------------------------
# .... 1.1 Import and filter in variables --------------------------------------
list1 <- lapply(allFiles, read_eprime)
list2 <- lapply(list1, FrameList)
list3 <- lapply(list2, filter_in,
                       key = "Procedure",
                       values = c("blockprocH0","blockprocH1","blockprocH2","blockprocH258"))
list4 <- lapply(list3, to_data_frame)

# .... 1.2 Drop unused variables (i.e., keep RT and accuracy) ------------------
list5 <- lapply(list4, cbind.data.frame)
clean <- function(list){cbind.data.frame(list$plussignH0.ACC, list$plussignH0.RT,
                                         list$plussignH1.ACC, list$plussignH1.RT,
                                         list$plussignH2.ACC, list$plussignH2.RT,
                                         list$plussignH258.ACC, list$plussignH258.RT)}
list6 <- lapply(list5, clean)

# .... 1.3 Convert factors to numeric ------------------------------------------
list7 <-  lapply(list6, function(x) {
                        x$`list$plussignH0.RT` <- as.numeric(as.character(x$`list$plussignH0.RT`))
                        x$`list$plussignH1.RT` <- as.numeric(as.character(x$`list$plussignH1.RT`))
                        x$`list$plussignH2.RT` <- as.numeric(as.character(x$`list$plussignH2.RT`))
                        x$`list$plussignH258.RT` <- as.numeric(as.character(x$`list$plussignH258.RT`))
                        x}) 

# .... 1.4 Reorder, drop, and rename variables for better overview -------------
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

# 2. Start analysis ------------------------------------------------------------
# .... 2.1 Drop all RTs < 100 and > 2500 (this is totally subjective) ----------
list11 <- lapply(list10, function(x){
                         x$H0validAnsw   <- ifelse(x$H0rt > 99   & x$H0rt < 2501,   x$H0rt,   NA)
                         x$H1validAnsw   <- ifelse(x$H1rt > 99   & x$H1rt < 2501,   x$H1rt,   NA)
                         x$H2validAnsw   <- ifelse(x$H2rt > 99   & x$H2rt < 2501,   x$H2rt,   NA)
                         x$H258validAnsw <- ifelse(x$H258rt > 99 & x$H258rt < 2501, x$H258rt, NA)
                         x})

# .... 2.2 Compute variable representing only valid and correct answers --------
list12 <- lapply(list11, function(x){
                         x$H0validCorr   <- ifelse(x$H0acc   == 1, x$H0validAnsw,   NA)
                         x$H1validCorr   <- ifelse(x$H1acc   == 1, x$H1validAnsw,   NA)
                         x$H2validCorr   <- ifelse(x$H2acc   == 1, x$H2validAnsw,   NA)
                         x$H258validCorr <- ifelse(x$H258acc == 1, x$H258validAnsw, NA)
                         x})  

# .... 2.3 Compute error rate --------------------------------------------------
list13 <- lapply(list12, function(x){
                         x$H0errorRate   <- round(1 - sum(!is.na(x$H0validCorr))   / sum(!is.na(x$H0validAnsw)), digits = 2)
                         x$H1errorRate   <- round(1 - sum(!is.na(x$H1validCorr))   / sum(!is.na(x$H1validAnsw)), digits = 2)
                         x$H2errorRate   <- round(1 - sum(!is.na(x$H2validCorr))   / sum(!is.na(x$H2validAnsw)), digits = 2)
                         x$H258errorRate <- round(1 - sum(!is.na(x$H258validCorr)) / sum(!is.na(x$H258validAnsw)), digits = 2)
                         x})

# .... 2.4 Compute a bunch of variables  ---------------------------------------
#          - meanRT0  = mean   of all valid correct trials > 99 & < 2500 ms
#          - sdRT     = sd     of all valid correct trials > 99 & < 2500 ms
#          - medianRT = median of all valid correct trials > 99 & < 2500 ms
#          - upperCut = meanRT0 + 3 * sdRT
list14 <- lapply(list13, function(x){
                         x$H0meanRT0   <- round(mean(x$H0validCorr, na.rm = TRUE), digits = 0)
                         x$H1meanRT0   <- round(mean(x$H1validCorr, na.rm = TRUE), digits = 0)
                         x$H2meanRT0   <- round(mean(x$H2validCorr, na.rm = TRUE), digits = 0)
                         x$H258meanRT0 <- round(mean(x$H258validCorr, na.rm = TRUE), digits = 0)
                         
                         x$H0sdRT   <- round(sd(x$H0validCorr, na.rm = TRUE), digits = 0)
                         x$H1sdRT   <- round(sd(x$H1validCorr, na.rm = TRUE), digits = 0)
                         x$H2sdRT   <- round(sd(x$H2validCorr, na.rm = TRUE), digits = 0)
                         x$H258sdRT <- round(sd(x$H258validCorr, na.rm = TRUE), digits = 0)
                         
                         x$H0medianRT   <- round(median(x$H0validCorr, na.rm = TRUE), digits = 0)
                         x$H1medianRT   <- round(median(x$H1validCorr, na.rm = TRUE), digits = 0)
                         x$H2medianRT   <- round(median(x$H2validCorr, na.rm = TRUE), digits = 0)
                         x$H258medianRT <- round(median(x$H258validCorr, na.rm = TRUE), digits = 0)
                         
                         x$H0upperCut   <- x$H0meanRT0   + (3 * x$H0sdRT)
                         x$H1upperCut   <- x$H1meanRT0   + (3 * x$H1sdRT)
                         x$H2upperCut   <- x$H2meanRT0   + (3 * x$H2sdRT)
                         x$H258upperCut <- x$H258meanRT0 + (3 * x$H258sdRT)
                         
                         x})

# .... 2.5 Compute DV meanRT: use all valid correct trials < upperCut ----------
list15 <- lapply(list14, function(x){
                         x$H0meanRT   <- round(mean(ifelse(x$H0validCorr    < x$H0upperCut,   x$H0validCorr, NA),   na.rm = TRUE), digits = 0)
                         x$H1meanRT   <- round(mean(ifelse(x$H1validCorr    < x$H1upperCut,   x$H1validCorr, NA),   na.rm = TRUE), digits = 0)
                         x$H2meanRT   <- round(mean(ifelse(x$H2validCorr    < x$H2upperCut,   x$H2validCorr, NA),   na.rm = TRUE), digits = 0)
                         x$H258meanRT <- round(mean(ifelse(x$H258validCorr  < x$H258upperCut, x$H258validCorr, NA), na.rm = TRUE), digits = 0)
                         x})

# .... 2.6 Compute difference between meanRT0 and meanRT -----------------------
#          (negative = decrease in mean RT)
list16 <- lapply(list15, function(x){
                         x$H0meanDiff   <- x$H0meanRT   - x$H0meanRT0
                         x$H1meanDiff   <- x$H1meanRT   - x$H1meanRT0
                         x$H2meanDiff   <- x$H2meanRT   - x$H2meanRT0
                         x$H258meanDiff <- x$H258meanRT - x$H258meanRT0
                         x})

# 3. Plots ---------------------------------------------------------------------
# .... 3.1 Preparatory work for this section -----------------------------------
#          - Create vectors of all RTs 
list20 <- lapply(list12, function(x){
                         x$H0allCorr   <- ifelse(x$H0acc   == 1, x$H0rt,   NA)
                         x$H1allCorr   <- ifelse(x$H1acc   == 1, x$H1rt,   NA)
                         x$H2allCorr   <- ifelse(x$H2acc   == 1, x$H2rt,   NA)
                         x$H258allCorr <- ifelse(x$H258acc == 1, x$H258rt, NA)
                         x})  

list21 <- do.call(rbind, lapply(list20, function(x){
                                        x[,c("H0allCorr","H1allCorr","H2allCorr","H258allCorr")]
                                        }))

convertVector <- function(x){as.vector(t(x))}
list22 <- lapply(list21, convertVector)

set.seed(1)
H0rtAllCorr   <- sample(list22$H0allCorr, replace = FALSE)
H1rtAllCorr   <- sample(list22$H1allCorr, replace = FALSE)
H2rtAllCorr   <- sample(list22$H2allCorr, replace = FALSE)
H258rtAllCorr <- sample(list22$H258allCorr, replace = FALSE)
rm(list20)
rm(list21)
rm(list22)

# .... 3.2 Plot trials for each subject individually ---------------------------
# par(mfrow=c(1,4))
# 
# for (i in 1:length(allFiles)) {
#    
#     plot(c(1:nrow(list16[[i]]["H0validCorr"])),as.vector(unlist(list16[[i]]["H0validCorr"])),
#                     ylim = c(100,2500),
#                     xlim = c(1,32),
#                     xlab = "Trial number",
#                     ylab = "RT [ms]",
#                     main = paste("trials of subject #", substr(allFiles[[i]], 21,23), " in H 0", sep = ""),
#                     bty  = "n",
#                     yaxt = "n",
#                     xaxt = "n",
#                     pch = 19)
#                axis(side = 1, at = c(0,10,20,30))
#                axis(side = 2, at = c(100,200,300,400,500,1000,1500,2000,2500), las=2)
#     rect(par("usr")[1],1500,par("usr")[2],par("usr")[4],col = "gray", density = 100)
#     rect(par("usr")[1],100,par("usr")[2],0,col = "gray", density = 100)
#     
#     text(16, 2400, labels = paste("valid trials (100 - 2500), mean = ",list16[[i]][1,"H0meanRT0"][1], sep = ""), cex = 2, col = "black")
#     text(16, 2200, labels = paste("3 * SD (", list16[[i]][1,"H0sdRT"], ") correction, mean = ", list16[[i]][1,"H0meanRT"][1], sep = ""), cex = 2, col = "red")   
#     text(16, 1800, labels = list16[[i]][1,"H0meanDiff"][1], cex = 7, col = "red")
#     abline(a = list16[[i]][1,"H0meanRT"], b = 0, col = "red", lwd = 2)
#     abline(a = list16[[i]][1,"H0meanRT0"], b = 0, col = "black", lwd = 1)
#     abline(a = list16[[i]][1,"H0meanRT"] + 3*list16[[i]][1,"H0sdRT"], b = 0, col = "black", lty = 2)
#     
#     
#     plot(c(1:nrow(list16[[i]]["H1validCorr"])),as.vector(unlist(list16[[i]]["H1validCorr"])),
#                     ylim = c(100,2500),
#                     xlim = c(1,32),
#                     xlab = "Trial number",
#                     ylab = "RT [ms]",
#                     main = paste("trials of subject #", substr(allFiles[[i]], 21,23), " in H 1", sep = ""),
#                     bty  = "n",
#                     yaxt = "n",
#                     xaxt = "n",
#                     pch = 19)
#                axis(side = 1, at = c(0,10,20,30))
#                axis(side = 2, at = c(100,200,300,400,500,1000,1500,2000,2500), las=2)
#     rect(par("usr")[1],1500,par("usr")[2],par("usr")[4],col = "gray", density = 100)
#     rect(par("usr")[1],100,par("usr")[2],0,col = "gray", density = 100)
#     
#     text(16, 2400, labels = paste("valid trials (100 - 2500), mean = ",list16[[i]][1,"H1meanRT0"][1], sep = ""), cex = 2, col = "black")
#     text(16, 2200, labels = paste("3 * SD (", list16[[i]][1,"H1sdRT"], ") correction, mean = ", list16[[i]][1,"H1meanRT"][1], sep = ""), cex = 2, col = "red")   
#     text(16, 1800, labels = list16[[i]][1,"H1meanDiff"][1], cex = 7, col = "red")
#     abline(a = list16[[i]][1,"H1meanRT"], b = 0, col = "red", lwd = 2)
#     abline(a = list16[[i]][1,"H1meanRT0"], b = 0, col = "black", lwd = 1)
#     abline(a = list16[[i]][1,"H1meanRT"] + 3*list16[[i]][1,"H1sdRT"], b = 0, col = "black", lty = 2)
#     
#     
#     plot(c(1:nrow(list16[[i]]["H2validCorr"])),as.vector(unlist(list16[[i]]["H2validCorr"])),
#                     ylim = c(100,2500),
#                     xlim = c(1,32),
#                     xlab = "Trial number",
#                     ylab = "RT [ms]",
#                     main = paste("trials of subject #", substr(allFiles[[i]], 21,23), " in H 2", sep = ""),
#                     bty  = "n",
#                     yaxt = "n",
#                     xaxt = "n",
#                     pch = 19)
#                axis(side = 1, at = c(0,10,20,30))
#                axis(side = 2, at = c(100,200,300,400,500,1000,1500,2000,2500), las=2)
#     rect(par("usr")[1],1500,par("usr")[2],par("usr")[4],col = "gray", density = 100)
#     rect(par("usr")[1],100,par("usr")[2],0,col = "gray", density = 100)
#     
#     text(16, 2400, labels = paste("valid trials (100 - 2500), mean = ",list16[[i]][1,"H2meanRT0"][1], sep = ""), cex = 2, col = "black")
#     text(16, 2200, labels = paste("3 * SD (", list16[[i]][1,"H2sdRT"], ") correction, mean = ", list16[[i]][1,"H2meanRT"][1], sep = ""), cex = 2, col = "red")   
#     text(16, 1800, labels = list16[[i]][1,"H2meanDiff"][1], cex = 7, col = "red")
#     abline(a = list16[[i]][1,"H2meanRT"], b = 0, col = "red", lwd = 2)
#     abline(a = list16[[i]][1,"H2meanRT0"], b = 0, col = "black", lwd = 1)
#     abline(a = list16[[i]][1,"H2meanRT"] + 3*list16[[i]][1,"H2sdRT"], b = 0, col = "black", lty = 2)
#     
#     
#     plot(c(1:nrow(list16[[i]]["H258validCorr"])),as.vector(unlist(list16[[i]]["H258validCorr"])),
#                     ylim = c(100,2500),
#                     xlim = c(1,32),
#                     xlab = "Trial number",
#                     ylab = "RT [ms]",
#                     main = paste("trials of subject #", substr(allFiles[[i]], 21,23), " in H 2.58", sep = ""),
#                     bty  = "n",
#                     yaxt = "n",
#                     xaxt = "n",
#                     pch = 19)
#                axis(side = 1, at = c(0,10,20,30))
#                axis(side = 2, at = c(100,200,300,400,500,1000,1500,2000,2500), las=2)
#     rect(par("usr")[1],1500,par("usr")[2],par("usr")[4],col = "gray", density = 100)
#     rect(par("usr")[1],100,par("usr")[2],0,col = "gray", density = 100)
#     
#     text(16, 2400, labels = paste("valid trials (100 - 2500), mean = ",list16[[i]][1,"H258meanRT0"][1], sep = ""), cex = 2, col = "black")
#     text(16, 2200, labels = paste("3 * SD (", list16[[i]][1,"H258sdRT"], ") correction, mean = ", list16[[i]][1,"H258meanRT"][1], sep = ""), cex = 2, col = "red")   
#     text(16, 1800, labels = list16[[i]][1,"H258meanDiff"][1], cex = 7, col = "red")
#     abline(a = list16[[i]][1,"H258meanRT"], b = 0, col = "red", lwd = 2)
#     abline(a = list16[[i]][1,"H258meanRT0"], b = 0, col = "black", lwd = 1)
#     abline(a = list16[[i]][1,"H258meanRT"] + 3*list16[[i]][1,"H258sdRT"], b = 0, col = "black", lty = 2)
#     
#     }
# 
# par(mfrow=c(1,1))


# .... 3.3 Plot distance weighted mean (dwe) for subject 189 -------------------
# par(mfrow=c(1,4))
# 
# plot(c(1:nrow(list16[[184]]["H0validCorr"])),as.vector(unlist(list16[[184]]["H0validCorr"])),
#                     ylim = c(100,2500),
#                     xlim = c(1,32),
#                     xlab = "Trial number",
#                     ylab = "RT [ms]",
#                     main = "trials of subject #189 in H 0",
#                     bty  = "n",
#                     yaxt = "n",
#                     xaxt = "n",
#                     pch = 19)
#                axis(side = 1, at = c(0,10,20,30))
#                axis(side = 2, at = c(100,200,300,400,500,1000,1500,2000,2500), las=2)
#     rect(par("usr")[1],1500,par("usr")[2],par("usr")[4],col = "gray", density = 100)
#     rect(par("usr")[1],100,par("usr")[2],0,col = "gray", density = 100)
#     
#     text(16, 2400, labels = paste("valid trials (100 - 2500), mean = ",list16[[184]][1,"H0meanRT0"][1], sep = ""), cex = 2, col = "black")
#     text(16, 2200, labels = paste("3 * SD (", list16[[184]][1,"H0sdRT"], ") correction, mean = ", list16[[184]][1,"H0meanRT"][1], sep = ""), cex = 2, col = "red")   
#     text(16, 2050, labels = "         distance-weighted mean = 240", cex = 2, col = "blue")
#     
#     text(16, 1850, labels = list16[[184]][1,"H0meanDiff"][1], cex = 3, col = "red")
#     
#     text(16, 1700, labels = -32, cex = 3, col = "blue")
#     
#     abline(a = list16[[184]][1,"H0meanRT"], b = 0, col = "red", lwd = 2)
#     abline(a = 240, b = 0, col = "blue", lwd = 1)
#     abline(a = list16[[184]][1,"H0meanRT0"], b = 0, col = "black", lwd = 1)
#     abline(a = list16[[184]][1,"H0meanRT"] + 3*list16[[184]][1,"H0sdRT"], b = 0, col = "black", lty = 2)
# 
# plot(c(1:nrow(list16[[184]]["H1validCorr"])),as.vector(unlist(list16[[184]]["H1validCorr"])),
#                     ylim = c(100,2500),
#                     xlim = c(1,32),
#                     xlab = "Trial number",
#                     ylab = "RT [ms]",
#                     main = "trials of subject #189 in H 1",
#                     bty  = "n",
#                     yaxt = "n",
#                     xaxt = "n",
#                     pch = 19)
#                axis(side = 1, at = c(0,10,20,30))
#                axis(side = 2, at = c(100,200,300,400,500,1000,1500,2000,2500), las=2)
#     rect(par("usr")[1],1500,par("usr")[2],par("usr")[4],col = "gray", density = 100)
#     rect(par("usr")[1],100,par("usr")[2],0,col = "gray", density = 100)
#     
#     text(16, 2400, labels = paste("valid trials (100 - 2500), mean = ",list16[[184]][1,"H1meanRT0"][1], sep = ""), cex = 2, col = "black")
#     text(16, 2200, labels = paste("3 * SD (", list16[[184]][1,"H1sdRT"], ") correction, mean = ", list16[[184]][1,"H1meanRT"][1], sep = ""), cex = 2, col = "red")   
#     text(16, 2050, labels = "         distance-weighted mean = 311", cex = 2, col = "blue")
#     
#     text(16, 1850, labels = list16[[184]][1,"H1meanDiff"][1], cex = 3, col = "red")
#     
#     text(16, 1700, labels = -35, cex = 3, col = "blue")
#     
#     abline(a = list16[[184]][1,"H1meanRT"], b = 0, col = "red", lwd = 2)
#     abline(a = 311, b = 0, col = "blue", lwd = 1)
#     abline(a = list16[[184]][1,"H1meanRT0"], b = 0, col = "black", lwd = 1)
#     abline(a = list16[[184]][1,"H1meanRT"] + 3*list16[[184]][1,"H1sdRT"], b = 0, col = "black", lty = 2)
# 
# 
# plot(c(1:nrow(list16[[184]]["H2validCorr"])),as.vector(unlist(list16[[184]]["H2validCorr"])),
#                     ylim = c(100,2500),
#                     xlim = c(1,32),
#                     xlab = "Trial number",
#                     ylab = "RT [ms]",
#                     main = "trials of subject #189 in H 2",
#                     bty  = "n",
#                     yaxt = "n",
#                     xaxt = "n",
#                     pch = 19)
#                axis(side = 1, at = c(0,10,20,30))
#                axis(side = 2, at = c(100,200,300,400,500,1000,1500,2000,2500), las=2)
#     rect(par("usr")[1],1500,par("usr")[2],par("usr")[4],col = "gray", density = 100)
#     rect(par("usr")[1],100,par("usr")[2],0,col = "gray", density = 100)
#     
#     text(16, 2400, labels = paste("valid trials (100 - 2500), mean = ",list16[[184]][1,"H2meanRT0"][1], sep = ""), cex = 2, col = "black")
#     text(16, 2200, labels = paste("3 * SD (", list16[[184]][1,"H2sdRT"], ") correction, mean = ", list16[[184]][1,"H2meanRT"][1], sep = ""), cex = 2, col = "red")   
#     text(16, 2050, labels = "         distance-weighted mean = 495", cex = 2, col = "blue")
#     
#     text(16, 1850, labels = list16[[184]][1,"H2meanDiff"][1], cex = 3, col = "red")
#     
#     text(16, 1700, labels = -27, cex = 3, col = "blue")
#     
#     abline(a = list16[[184]][1,"H2meanRT"], b = 0, col = "red", lwd = 2)
#     abline(a = 495, b = 0, col = "blue", lwd = 1)
#     abline(a = list16[[184]][1,"H2meanRT0"], b = 0, col = "black", lwd = 1)
#     abline(a = list16[[184]][1,"H2meanRT"] + 3*list16[[184]][1,"H2sdRT"], b = 0, col = "black", lty = 2)
# 
#     
# plot(c(1:nrow(list16[[184]]["H258validCorr"])),as.vector(unlist(list16[[184]]["H258validCorr"])),
#                     ylim = c(100,2500),
#                     xlim = c(1,32),
#                     xlab = "Trial number",
#                     ylab = "RT [ms]",
#                     main = "trials of subject #189 in H 2.58",
#                     bty  = "n",
#                     yaxt = "n",
#                     xaxt = "n",
#                     pch = 19)
#                axis(side = 1, at = c(0,10,20,30))
#                axis(side = 2, at = c(100,200,300,400,500,1000,1500,2000,2500), las=2)
#     rect(par("usr")[1],1500,par("usr")[2],par("usr")[4],col = "gray", density = 100)
#     rect(par("usr")[1],100,par("usr")[2],0,col = "gray", density = 100)
#     
#     text(16, 2400, labels = paste("valid trials (100 - 2500), mean = ",list16[[184]][1,"H258meanRT0"][1], sep = ""), cex = 2, col = "black")
#     text(16, 2200, labels = paste("3 * SD (", list16[[184]][1,"H258sdRT"], ") correction, mean = ", list16[[184]][1,"H258meanRT"][1], sep = ""), cex = 2, col = "red")   
#     text(16, 2050, labels = "         distance-weighted mean = 519", cex = 2, col = "blue")
#     
#     text(16, 1850, labels = list16[[184]][1,"H258meanDiff"][1], cex = 3, col = "red")
#     
#     text(16, 1700, labels = -34, cex = 3, col = "blue")
#     
#     abline(a = list16[[184]][1,"H258meanRT"], b = 0, col = "red", lwd = 2)
#     abline(a = 519, b = 0, col = "blue", lwd = 1)
#     abline(a = list16[[184]][1,"H258meanRT0"], b = 0, col = "black", lwd = 1)
#     abline(a = list16[[184]][1,"H258meanRT"] + 3*list16[[184]][1,"H258sdRT"], b = 0, col = "black", lty = 2)
# par(mfrow=c(1,1))

# 3. Create target data frame & variable 'subject' -----------------------------
result            <- data.frame(matrix(0,numberOfFiles,20))
result$subject    <- as.numeric(substr(allFiles, 21,23))

# .... 3.1 Paste key variables into the target data frame ----------------------
result[1:20] <- do.call(rbind,lapply(list16, function(x)
                                            x[1, c("H0meanRT","H1meanRT","H2meanRT","H258meanRT",
                                                   "H0sdRT","H1sdRT","H2sdRT","H258sdRT",
                                                   "H0medianRT","H1medianRT","H2medianRT","H258medianRT",
                                                   "H0errorRate","H1errorRate","H2errorRate","H258errorRate",
                                                   "H0meanDiff", "H1meanDiff", "H2meanDiff", "H258meanDiff")]))


# .... 3.2 Rename columns in object result -------------------------------------
colnames(result)        <- c("H0meanRT","H1meanRT","H2meanRT","H258meanRT",
                             "H0sdRT","H1sdRT","H2sdRT","H258sdRT",
                             "H0medianRT","H1medianRT","H2medianRT","H258medianRT",
                             "H0errorRate","H1errorRate","H2errorRate","H258errorRate",
                             "H0meanDiff","H1meanDiff","H2meanDiff","H258meanDiff","subject")
result <- result[c(21,1:20)]

# 4. Cleaning up  --------------------------------------------------------------
resultsHick <- result
# rm(list=setdiff(ls(), c("resultsHick", "H0rtAllCorr", "H1rtAllCorr", "H2rtAllCorr", "H258rtAllCorr",
#                         "resultsSupp2","resultsBIS","resultsFragebogen")))
# cat("\014")
str(resultsHick)

################################################################################
################################################################################

# 5. Plot reaction times for each subject INDIVIDUALLY--------------------------
# xaxis <- c(0,1,2,3)
# plot.new()
# for (i in min(resultsHick$subject):max(resultsHick$subject))
# {subj <- subset(resultsHick,
#                  subject==i,
#                  select=c(H0meanRT, H1meanRT, H2meanRT, H258meanRT))
#   plot(xaxis, subj,
#      ylim=c(100,600),
#      xlim=c(0,3),
#      xlab=expression(log[2]~~of~~the~~number~~of~~stimulus~~response~~alternatives),
#      ylab="RT [ms]",
#      main=paste("subject #", i, sep = ""),
#      bty           = "n",
#      xaxt          = "n",
#      yaxt          = "n",
#      type          = "o")
# 
#   par(mfrow=c(1,1))
# 
# #par(new=T)
# axis(side = 1, at = c(0,1,2,3), labels = c("0","1","2","2.58"))
# axis(side = 2, at = c(100,200,300,400,500), las=2)
# }


# 6. Plot reaction times for ALL SUBJECTS --------------------------------------
# library(plotrix)
# xaxis <- c(0,1,2,3)
# hickList <- list(resultsHick[,2], resultsHick[,3],
#               resultsHick[,4], resultsHick[,5])
# 
# yaxis2 <- lapply(hickList, mean)
# 
# yaxis <- c(mean(resultsHick[,2]), mean(resultsHick[,3]),
#            mean(resultsHick[,4]), mean(resultsHick[,5]))
# 
# plot.new()
# 
# plot(xaxis,yaxis2,
#      xlim=c(0,3),
#      ylim = c(100,550),
#      xlab=expression(log[2]~~of~~the~~number~~of~~stimulus~~response~~alternatives),
#      ylab="RT [ms]",
#      main=expression(paste(mean %+-% sd[mean], " of all subjects")),
#      bty           = "n",
#      xaxt          = "n",
#      yaxt          = "n",
#      type          = "o")
# 
# #par(new=T)
# axis(side = 1, at = c(0,1,2,3), labels = c("0","1","2","2.58"))
# axis(side = 2, at = c(100,200,300,400,500), las=2)
# 
# # dispersion function
# hickSD <- lapply(hickList,sd)
# hickSD2 <- unlist(hickSD)
# dispersion(xaxis, yaxis,
#            ulim = hickSD2,
#            llim = hickSD2,
#            type = "a",
#            fill = TRUE,
#            intervals = TRUE,
#            lty = 2,
#            pch = 6)

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
# cat("\014")
# rm(list = setdiff(ls(), c("resultsHick", "H0rtAllCorr", "H1rtAllCorr", "H2rtAllCorr", "H258rtAllCorr",
#                         "resultsSupp2","resultsBIS")))


# 10. Scatterplots --------------------------------------------------------
# par(mfrow=c(1,4))
# plot(H0rtAllCorr,
#      main = "0 bit",
#      ylim = c(0,2500),
#      yaxt = "n",
#      ylab = "RT [ms]",
#      xlab = "all trials (randomly ordered)",
#      bty = "n",
#      las = 1)
# 
# axis(side = 2, at = c(0,100,200,300,400,500,1000,1500,1600), las=2)
# abline(h = 100, col = "red", lwd = 2 )
# abline(h = 1500, col = "red", lwd = 2 )
# 
# plot(H1rtAllCorr,
#      main = "1 bit",
#      ylim = c(0,2500),
#      yaxt = "n",
#      ylab = "RT [ms]",
#      xlab = "all trials (randomly ordered)",
#      bty = "n",
#      las = 1)
# 
# axis(side = 2, at = c(0,100,200,300,400,500,1000,1500,1600), las=2)
# abline(h = 100, col = "red", lwd = 2 )
# abline(h = 1500, col = "red", lwd = 2 )
# 
# plot(H2rtAllCorr,
#      main = "2 bit",
#      ylim = c(0,2500),
#      yaxt = "n",
#      ylab = "RT [ms]",
#      xlab = "all trials (randomly ordered)",
#      bty = "n",
#      las = 1)
# 
# axis(side = 2, at = c(0,100,200,300,400,500,1000,1500,1600), las=2)
# abline(h = 100, col = "red", lwd = 2)
# abline(h = 1500, col = "red", lwd = 2)
# 
# plot(H258rtAllCorr,
#      main = "2.58 bit",
#      ylim = c(0,2500),
#      yaxt = "n",
#      ylab = "RT [ms]",
#      xlab = "all trials (randomly ordered)",
#      bty = "n",
#      las = 1)
# 
# axis(side = 2, at = c(0,100,200,300,400,500,1000,1500,1600), las=2)
# abline(h = 100, col = "red", lwd = 2 )
# abline(h = 1500, col = "red", lwd = 2 )
# par(mfrow=c(1,1))
# 
# 
# # 11. density plots -------------------------------------------------------
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

