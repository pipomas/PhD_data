##################################################################################################
##################################################################################################
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
###
###   - Asus VG248QE (144 Hz)
###   - 1080 x 1920 p
###   - Screen luminance ~ 190 cd/m^2
###   - Room luminance (white paper next to the monitor) ~ 9 cd/m^2
###     [left = 10.2, bottom = 8.2, right = 7.7, top = 8.7 --> ~9]
###   - Contrast set to 50 %

# library(R.matlab)      <- needed to run analysis                                                                       
# library(dplyr)         <- needed to run analysis
# library(xlsx)          <- needed to print the xlsx file

# Preparatory work ------------------------------------------------------------------------------------------
# Set working directory (wd) to your folder                                                     
setwd("/Users/philippthomas/Documents/Uni Bern/PDD/Dissertation/Daten/Project/data/base/Supp1")

# Select folder of .mat files & create a list of file names                                     
dataDirectory <- getwd()
allFiles <- list.files(path = dataDirectory)                                                    
                                                               
numberOfFiles <- length(allFiles)                  #count files                                   
result <- data.frame(matrix(0,numberOfFiles,24))   #create an empty dataframe(0,numberOfFiles,24 variables) 
                                                                   
# Read files and create a data frame -------------------------------------------------------------------------
list    <- lapply(allFiles, readMat)               #list apply function readMat on allFiles            
clean   <- function(list){(list$result)}           #read result section of list     
list2   <- lapply(list, clean)                     #list apply function 'clean'            
trans   <- function(matrix){as.vector(t(matrix))}  #transform matrix to vector      
list3   <- lapply(list2, trans)                    #list apply function 'trans'             
result  <- do.call(rbind, list3)                   #row bind list3                  
results <- as.data.frame(cbind(allFiles,result))   #attach file names & create data frame               
                                                                                              
# Rename variables of data frame 'results' ------------------------------------------------------------------                        
results  <- rename(results,                   
                   subject=allFiles,          
                   s1r1p1=V2,   s2r1p1=V8,    s3r1p1=V14,     s4r1p1=V20,    
                   s1r1p2=V3,   s2r1p2=V9,    s3r1p2=V15,     s4r1p2=V21,
                   s1r2p1=V4,   s2r2p1=V10,   s3r2p1=V16,     s4r2p1=V22,  
                   s1r2p2=V5,   s2r2p2=V11,   s3r2p2=V17,     s4r2p2=V23,  
                   s1r3p1=V6,   s2r3p1=V12,   s3r3p1=V18,     s4r3p1=V24,
                   s1r3p2=V7,   s2r3p2=V13,   s3r3p2=V19,     s4r3p2=V25)    

# Arrange data frame by subject -----------------------------------------------------------------------------                                                      
results  <- arrange(results, subject)                     #doesn't work                       
sapply(results, class)                                    #check class of variables:          
                                                          # -> all variables are factors      
factorconvert <- function(f){as.numeric(levels(f))[f]}    #convert factors -> numeric         
results[2:25]  <- lapply(results[2:25], factorconvert)    #apply function                     
sapply(results, class)                                    #check conversion: -> it worked  

# Correct for the hard coded stimulus duration limit of 1000 ms
# NOT SURE IF THIS CODE IS WORKING - CHECK! -----------------                                                                            
results[2:25][results[2:25]>400]  <- 400              #replace all thresholds > 400 with 400 

# Compute approximation of actual stimulus duration (full width at half height
# of the temporal envelope) and create new variables /w prefix x25 for '*2.5'.
# raw data gets multiplied by 2.5, therefore the prefix x25 --------------------
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

# Compute raw thresholds (since THESE values are computed by MATLAB in the 
# first place) and create new variables /w prefix log10.
# These numbers should be used for plotting ------------------------------------
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

# Get a vector of the smallest and highest thresholds
# per stimulus size (s1, s2, s3, s4).
# Argument 1 = indicates rows -------------------------------------------------
subjectMins1 <- apply(results[50:55],1,min)
subjectMaxs1 <- apply(results[50:55],1,max)

subjectMins2 <- apply(results[56:61],1,min)
subjectMaxs2 <- apply(results[56:61],1,max)

subjectMins3 <- apply(results[62:67],1,min)
subjectMaxs3 <- apply(results[62:67],1,max)

subjectMins4 <- apply(results[68:73],1,min)
subjectMaxs4 <- apply(results[68:73],1,max)

# Compute mean threshold without the smallest and highest estimation
# and create new variable /w prefix log10mean ----------------------------------
results$log10means1  <- (rowSums(results[50:55]) - subjectMins1 - subjectMaxs1) /4
results$log10means2  <- (rowSums(results[56:61]) - subjectMins2 - subjectMaxs2) /4
results$log10means3  <- (rowSums(results[62:67]) - subjectMins3 - subjectMaxs3) /4
results$log10means4  <- (rowSums(results[68:73]) - subjectMins4 - subjectMaxs4) /4


# compute ------------
results$means1 <- 10 ^ results$log10means1
results$means2 <- 10 ^ results$log10means2
results$means3 <- 10 ^ results$log10means3
results$means4 <- 10 ^ results$log10means4

# Compute 'suppression index' and create new variable --------------------------
results$SI1  <- results$log10means4 - results$log10means1

# Retain only subject number
results$subject <- as.numeric(substr(allFiles, 1,3))

# Plot Suppression Index -------------------------------
dotchart(results$SI1,
         labels = row.names(results),
         cex = .7,
         ylab = "subject number",
         xlab = "Suppression Index [log10]",
         main = "Streuung der Suppression Indizes",
         bty = "n")

######################################################################
#
# Plot lines for each subject over four conditions
#### ACHTUNG: main = i entspricht nicht der Vpn!!!

xSup              <- c(1.8,3.6,5.4,7.2)


plot.new()
for (i in min(results$subject):max(results$subject))  
{ subj <- subset(results,
                 subject==i, 
                 select=c(means1, means2, means3, means4)) 
plot(xSup,subj,
     ylim=c(0,200),
     xlim=c(1.8,7.2),
     xlab="visual angle [°]",
     ylab="threshold 80% correct [ms]",
     main=i,
     bty           = "n",
     xaxt          = "n",
     yaxt          = "n",
     type          = "o")

#par(new=T)
axis(side = 1, at = c(1.8,3.6,5.4,7.2), labels=T)
axis(side = 2, at = c(0,50,100,150,200), las=1)
}

# Suppression Index summary ---------------------------------------------
#print(results)
#summary(results$SI)

resultsSupp1 <- results[,c(1,78:82)]

str(resultsSupp1)
print(resultsSupp1)
summary(resultsSupp1$SI)

# Write excel file -----------------
#write.xlsx(results, "SuppressionTask.xlsx")













