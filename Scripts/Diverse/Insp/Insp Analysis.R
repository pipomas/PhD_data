################################################################################
################################################################################
###                                                                             
###   R E A D   E P R I M E   F I L E S   &   C R E A T E   D A T A   F R A M E
###   Inspection Time
###
###   - 8 practice trials
###   - 32 trials
###   - Mittelwert über 13-32


library(rprime)          #<- needed to run analysis
# library(dplyr)         <- needed to run analysis


# Preparatory work ------------------------------------------------------------------------------------------
# Set working directory (wd) to your folder                                                     
# setwd("/Users/philippthomas/Documents/Uni Bern/PDD/Dissertation/Daten/Project/data/base/Insp")

allFiles <- paste("data/base/Insp", list.files(path = "data/base/Insp"), sep = "/")
numberOfFiles <- length(allFiles)                  #count files        

#numberOfFiles <- length(allFiles)                  #count files                                   
#result <- data.frame(matrix(0,numberOfFiles,160))   #create an empty dataframe(0,numberOfFiles,24 variables) 

list <- lapply(allFiles, read_eprime)
list2 <- lapply(list, FrameList)
list3 <- lapply(list2, filter_in, key = "Procedure", values = "InspectionTime")
list4 <- lapply(list3, to_data_frame)

trans   <- function(matrix){as.vector(t(matrix))}

list5    <- lapply(list4, trans)
clean <- function(list){list[c(3,17,34,51,68,85,102,119,136,153,170,187,204,221,238,255,
                               272,289,306,323,340,357,374,391,408,425,442,459,476,493,
                               510,527,544)]}

list6 <- lapply(list5, clean)

results <- do.call(rbind.data.frame, list6)

colnames(results)  <- c("subject",
                        "inspt1","inspt2","inspt3","inspt4","inspt5","inspt6",
                        "inspt7","inspt8","inspt9","inspt10","inspt11","inspt12",
                        "inspt13","inspt14","inspt15","inspt16","inspt17","inspt18",
                        "inspt19","inspt20","inspt21","inspt22","inspt23","inspt24",
                        "inspt25","inspt26","inspt27","inspt28","inspt29","inspt30",
                        "inspt31","inspt32")

results[2:33] <- lapply(results[2:33], function(x)as.numeric(levels(x))[x]) 
str(results) 

results$subject          <- as.numeric(substr(allFiles, 12,14))


results$meanInsp <- rowMeans(results[14:33])
summary(results$meanInsp)

############################

# Plot lines for each subject over four conditions
xInspection              <- c(1:32)
results$numbering <- 1:nrow(results)

plot.new()
for (i in 1:nrow(results))  
{ subj <- subset(results,
                 numbering==i, 
                 select=c(2:33)) 
plot(xInspection,subj,
     ylim=c(0,140),
     xlim=c(1,32),
     xlab="trial number",
     ylab="stimulus duration [ms]",
     main=i,
     bty           = "n",
     xaxt          = "n",
     yaxt          = "n",
     type          = "o")

#par(new=T)
axis(side = 1, at = c(1:32))
axis(side = 2, at = seq(0,140,10), las=1)

subjInspection <- subset(results,
               numbering==i, 
               select=34) 

abline(a = subjInspection[1,1],
       b = 0,
       col = 6,
       lty = 5,
       lwd = 3)
}

summary(results$meanInsp)
print("Trials 13-32 werden als AV gemittelt")

resultsInsp <- results[,c(1,34)]

str(resultsInsp)
print(resultsInsp)
summary(resultsInsp)

subset(resultsInsp, subject %in% c(19,23,24,25,26))



