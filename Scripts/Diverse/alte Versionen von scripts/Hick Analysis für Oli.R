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
###    Philipp Thomas, Juli 2015

library(rprime)          #<- needed to run analysis

# 0. Preparatory work ----------------------------------------------------------
# 0.1 Set working directory (wd) to the folder containing all the .txt files ---
# setwd("/Users/philippthomas/Documents/Uni Bern/PDD/Dissertation/Daten/Project/data/base/Hick")

# 0.1 or use 0.2 to access sub folders -----------------------------------------
# 0.2 Select folder of Hick txt files & create a list of file names ------------
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

# 2.2 Replace reaction times <100, >1100, the slowest with NA ---------------------------
list11 <- lapply(list10, function(x){
                          x$`list$H0rtTrim`    <- ifelse(is.na(x$`list$H0rt`),   NA, ifelse(x$`list$H0rt`   < 99, NA, ifelse(x$`list$H0rt`   > 1100, NA, ifelse(x$`list$H0rt` == max(x$`list$H0rt`, na.rm = T),     NA, x$`list$H0rt`))))
                          x$`list$H1rtTrim`    <- ifelse(is.na(x$`list$H1rt`),   NA, ifelse(x$`list$H1rt`   < 99, NA, ifelse(x$`list$H1rt`   > 1100, NA, ifelse(x$`list$H1rt` == max(x$`list$H1rt`, na.rm = T),     NA, x$`list$H1rt`))))
                          x$`list$H2rtTrim`    <- ifelse(is.na(x$`list$H2rt`),   NA, ifelse(x$`list$H2rt`   < 99, NA, ifelse(x$`list$H2rt`   > 1100, NA, ifelse(x$`list$H2rt` == max(x$`list$H2rt`, na.rm = T),     NA, x$`list$H2rt`))))
                          x$`list$H258rtTrim`  <- ifelse(is.na(x$`list$H258rt`), NA, ifelse(x$`list$H258rt` < 99, NA, ifelse(x$`list$H258rt` > 1100, NA, ifelse(x$`list$H258rt` == max(x$`list$H258rt`, na.rm = T), NA, x$`list$H258rt`))))
                          x})
# 2.3 Compute mean reaction times ------------------------------------------------------
list12 <- lapply(list11, function(x){
                         x$`list$H0meanRT`   <- mean(x$`list$H0rtTrim`,   na.rm = T)
                         x$`list$H1meanRT`   <- mean(x$`list$H1rtTrim`,   na.rm = T)
                         x$`list$H2meanRT`   <- mean(x$`list$H2rtTrim`,   na.rm = T)
                         x$`list$H258meanRT` <- mean(x$`list$H258rtTrim`, na.rm = T)
                         x})

# 3. Create target data frame & variable 'subject' --------------------------------------
result                  <- data.frame(matrix(0,numberOfFiles,4))
# result$subject          <- as.numeric(substr(allFiles, 6,8))
result$subject          <- as.numeric(substr(allFiles, 21,23))

# 3.1 Paste mean reaction times into the target data frame -----------------------------
result[1:4] <- do.call(rbind,lapply(list12, function(x)
                                            x[1, c('list$H0meanRT','list$H1meanRT','list$H2meanRT','list$H258meanRT')]))

# 3.2 Reorder columns ------------------------------------------------------------------
colnames(result)        <- c("H0meanRT","H1meanRT","H2meanRT","H258meanRT","subject")
result                  <- result[c(5,1,2,3,4)]

# 4. Print data frame 'result' ---------------------------------------------------------
print(result, digits = 3)

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



# 7. Summary -----------------------------------------------------------------------------
resultsHick <- result
str(resultsHick)
print(resultsHick)
summary(resultsHick)

