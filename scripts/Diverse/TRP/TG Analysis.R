################################################################################
################################################################################
###                                                                             
###   R E A D   E P R I M E   T X T - F I L E S   &   C R E A T E   D A T A   F R A M E
###   Temporal Generalization
###
###   - Learning phase: 5x 75 ms tone
###   - Testing phase:
###     8 blocks à
###     - 42 ms tone
###     - 53 ms tone
###     - 64 ms tone
###     - 75 ms tone
###     - 75 ms tone
###     - 86 ms tone
###     - 97 ms tone
###     - 108 ms tone

library(rprime)          #<- needed to run analysis
#library(plyr)            #<- needed to run analysis


# Preparatory work ------------------------------------------------------------------------------------------
# Set working directory (wd) to your folder                                                     
# setwd("/Users/philippthomas/Documents/Uni Bern/PDD/Dissertation/Daten/Project/data/base/TG")

# Select folder of TG txt files & create a list of file names                                     
allFiles <- paste("data/base/TG", list.files(path = "data/base/TG"), sep = "/")                                              

numberOfFiles <- length(allFiles)                    #count files                                   
#result <- data.frame(matrix(0,numberOfFiles,160))   #create an empty dataframe(0,numberOfFiles,24 variables) 

list1 <- lapply(allFiles, read_eprime)
list2 <- lapply(list1, FrameList)
list3 <- lapply(list2, filter_in, key = "Procedure", values = "Course")
list4 <- lapply(list3, to_data_frame)

list5 <- lapply(list4, cbind.data.frame)
clean <- function(list){cbind.data.frame(list$Stimulus, list$IndicationStandard)}           #read result section of list     
list6 <- lapply(list5, clean)

list7 <-  lapply(list6, function(x) {
                        x$`list$IndicationStandard` <- as.numeric(as.character(x$`list$IndicationStandard`))
                        x}) 

list8 <- lapply(list7, function(x){arrange(x,`list$Stimulus`)})
  
str(list8)

list9 <- lapply(list8, function(x){
                       x$`list$P42`  <- sum(x[9:16,2])/8
                       x$`list$P53`  <- sum(x[17:24,2])/8
                       x$`list$P64`  <- sum(x[25:32,2])/8
                       x$`list$P75`  <- sum(x[33:48,2])/16
                       x$`list$P86`  <- sum(x[49:56,2])/8
                       x$`list$P97`  <- sum(x[57:64,2])/8
                       x$`list$P108` <- sum(x[1:8,2])/8
                       
                       x$`list$TGdispInd1` <- (x$`list$P75`)/(x$`list$P42`+x$`list$P53`+x$`list$P64`+x$`list$P75`+x$`list$P86`+x$`list$P97`+x$`list$P108`)
                       x$`list$TGdispInd2` <- (x$`list$P64`+x$`list$P75`+x$`list$P86`)/(x$`list$P42`+x$`list$P53`+x$`list$P64`+x$`list$P75`+x$`list$P86`+x$`list$P97`+x$`list$P108`)
                       x})

result                  <- data.frame(matrix(0,numberOfFiles,9))   #create an empty dataframe(0,numberOfFiles,2 variables) 

result[1:9] <- do.call(rbind,lapply(list9, function(x)
                                                       x[1, c('list$P42','list$P53','list$P64','list$P75','list$P86','list$P97','list$P108','list$TGdispInd1',  'list$TGdispInd2')]))
result$subject          <- as.numeric(substr(allFiles, 19,21))
colnames(result)        <- c("P42","P53","P64","P75","P86","P97","P108","TGdispInd1","TGdispInd2","subject")
result                  <- result[c(10,1,2,3,4,5,6,7,8,9)]

print(result)
                        
################### 
# Plot Dispersion for each subject 
xaxis <- c(42, 53, 64, 75, 86, 97, 108)

plot.new()
for (i in min(result$subject):max(result$subject))  
{ subj <- subset(result,
                 subject==i, 
                 select=c(P42, P53, P64, P75, P86, P97, P108)) 
plot(xaxis,subj,
     ylim=c(0,1),
     xlim=c(42,108),
     xlab="stimulus duration [ms]",
     ylab="Proportion of Yes responses",
     main=i,
     bty           = "n",
     xaxt          = "n",
     yaxt          = "n",
     type          = "o")

#par(new=T)
axis(side = 1, at = c(42,53,64,75,86,97,108))
axis(side = 2, at = seq(0,1,.25), las=2)
}

# Plot dispersion for all subjects ---------------------------------------------
yaxis <- c(mean(result$P42),mean(result$P53),mean(result$P64),mean(result$P75),
           mean(result$P86),mean(result$P97),mean(result$P108))

plot(xaxis, yaxis,
     ylim=c(0,1),
     xlim=c(42,108),
     xlab="stimulus duration [ms]",
     ylab="Proportion of Yes responses",
     main="mean of all subjects",
     bty           = "n",
     xaxt          = "n",
     yaxt          = "n",
     type          = "o",
     lwd = 4,
     col = 4)

#par(new=T)
axis(side = 1, at = c(42,53,64,75,86,97,108))
axis(side = 2, at = seq(0,1,.25), las=2)


# cleaning up -------------------------------------------------------------



print(result)

resultsTG <- result[,c(1,9:10)]

str(resultsTG)
print(resultsTG)
summary(resultsTG)





