################################################################################
################################################################################
###                                                                             
###   R E A D   E P R I M E   T X T - F I L E S   &   C R E A T E   D A T A   F R A M E
###   Duration Discrimination with FILLED intervals (DDF)
###
###   Exercise: 5 trials
###   Test:
###   - a standard and a comparison intervall
###   - the duration of the comparison interval varied according to an adaptive rule (Kaernbach, 1991)
###   - to estimate x.25 and x.75 of the individual psychometric function
###   - initial standard = 50 ms
###   - initial delta = 15 ms
###   - 2 independent series of 32 trials
###      - Series 1: the comparison interval was shorter
###      - Series 2: the comparison interval was longer
###      - Trials from both series were presented in random order
###
###   - Counter25   <= 5        - Counter 25  >= 6
###     - StepUp25   = 3          - StepUp25   = 2
###     - StepDown25 = 9          - StepDown25 = 6
###
###   - Counter75   <= 5        - Counter75   >= 6
###     - StepUp75   = 9          - StepUp75   = 6
###     - StepDown75 = 3          - StepDown75 = 2
###
###   - only the last 20 trials (13-32, and 45-64 are analyzed)

library(rprime)         # <- needed to run analysis
#library(dplyr)         # <- needed to run analysis

# Preparatory work ------------------------------------------------------------------------------------------
# Set working directory (wd) to your folder                                                     
setwd("/Users/philippthomas/Documents/Uni Bern/PDD/Dissertation/Daten/Project/data/base/DDF")

# Select folder of Hick txt files & create a list of file names                                     
dataDirectory <- getwd()
allFiles <- list.files(path = dataDirectory)                                                    

numberOfFiles <- length(allFiles)                    #count files                                   
#result <- data.frame(matrix(0,numberOfFiles,160))   #create an empty dataframe(0,numberOfFiles,24 variables) 

list1 <- lapply(allFiles, read_eprime)
list2 <- lapply(list1, FrameList)
list3 <- lapply(list2, filter_in, key = "Procedure", values = "CourseExp")
list4 <- lapply(list3, to_data_frame)
list5 <- lapply(list4, cbind.data.frame)

clean <- function(list){cbind.data.frame(list$Response.ACC, list$Delta, list$Counter25, list$Counter75)}           #read result section of list     
list6 <- lapply(list5, clean)

list7 <-  lapply(list6, function(x) {
                        x$`list$Delta`       <- as.numeric(as.character(x$`list$Delta`))
                        x$`list$Counter25` <- as.numeric(as.character(x$`list$Counter25`))
                        x$`list$Counter75` <- as.numeric(as.character(x$`list$Counter75`))
                        x}) 

list8 <- lapply(list7, function(x){arrange(x,`list$Counter25`,`list$Counter75`)})

list9 <- lapply(list8, function(x){
                       x$`list$meanDelta25`  <- mean(x[13:32,2])
                       x$`list$meanDelta75`  <- mean(x[45:64,2])
                       x$`list$DDFdl`        <- (abs(x[1,5]) + abs(x[1,6]))/2
                       x})

result <- data.frame(matrix(0,numberOfFiles,67))  

result[1:64]  <- do.call(rbind,lapply(list9, function(x)
                                            x[1:64,'list$Delta']))

result[65]   <- do.call(rbind,lapply(list9, function(x)
                                            x[1,'list$meanDelta25']))

result[66]   <- do.call(rbind,lapply(list9, function(x)
                                            x[1,'list$meanDelta75']))

result[67]   <- do.call(rbind,lapply(list9, function(x)
                                            x[1,'list$DDFdl']))

colnames(result)  <- c("t1a","t2a","t3a","t4a","t5a","t6a","t7a","t8a","t9a","t10a",
                       "t11a","t12a","t13a","t14a","t15a","t16a","t17a","t18a","t19a","t20a",
                       "t21a","t22a","t23a","t24a","t25a","t26a","t27a","t28a","t29a","t30a",
                       "t31a","t32a",
                       "t1b","t2b","t3b","t4b","t5b","t6b","t7b","t8b","t9b","t10b",
                       "t11b","t12b","t13b","t14b","t15b","t16b","t17b","t18b","t19b","t20b",
                       "t21b","t22b","t23b","t24b","t25b","t26b","t27b","t28b","t29b","t30b",
                       "t31b","t32b",
                       "meanDelta25","meanDelta75","DDFdl")

#for plotting the curves change to "as.numeric(subs......)"
result$subject          <- as.numeric(substr(allFiles, 18,20))

result                  <- result[c(68,1:67)]

                        
################### ----------
# Plot delta  for each subject 
xaxis <- c(1:32)

plot.new()
for (i in min(result$subject):max(result$subject))  
{ subj <- subset(result,
                 subject==i, 
                 select=t1a:t32a) 
plot(xaxis,subj,
     ylim=c(-40,40),
     xlim=c(1,32),
     xlab="trial number",
     ylab=expression(paste(Delta, " [ms]")),
     main=i,
     col=6,
     bty           = "n",
     xaxt          = "n",
     yaxt          = "n",
     type          = "o"
     )

subj <- subset(result,
               subject==i, 
               select=t1b:t32b) 
par(new=T)
plot(xaxis,subj,
     ylim=c(-40,40),
     xlim=c(1,32),
     xlab = "",
     ylab = "",
     col=4,
     bty           = "n",
     xaxt          = "n",
     yaxt          = "n",
     type          = "o")

abline(a = 0, b = 0, lty = 2, lwd = .4)
legend(x = "topright",
       legend = c("comparison longer", "comparison shorter"),
       lty = 1,
       bty = "n",
       col = c(4,6))

#par(new=T)
axis(side = 1, at = seq(1,32,1))
axis(side = 2, at = seq(-40,40,10), las=2)
}

# ---------------

resultsDDF <- result[,c(1,66:68)]

str(resultsDDF)
print(resultsDDF)
summary(resultsDDF)




