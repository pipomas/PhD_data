################################################################################
################################################################################
###                                                                             
###   R E A D   Unipark - Fragebogen 
###
###   1. Export csv file out of unipark.de
###   2. Save it into your directory

#library(xlsx)          #<- needed to run analysis
#library(dplyr)         #<- needed to run analysis


# 0.  Preparatory work ---------------------------------------------------------
# 0.1 Set working directory (wd) to your folder                                                     
# setwd("/Users/philippthomas/Documents/Uni Bern/PDD/Dissertation/Daten/Project/data/base/Fragebogen")

# 1. read in the xlsx file -----------------------------------------------------
dat <- read.csv(file = "data/base/Fragebogen/Fragebogen.csv", header = TRUE, sep = ";")

# 2. rename the columns --------------------------------------------------------
dat <- rename(dat,
              subject = v_1058,
              sex = v_872,
              day = v_877,
              month = v_878,
              year = v_879,
              edu = v_85,
              job = v_451,
              hand = v_870,
              typewr = v_873,
              lens = v_874,
              caffein = v_1135,
              games = v_876,
              instru = v_1046,
              gamesfreq = v_1035,
              gamesfps = v_1056,
              gamesname = v_1057,
              instruname = v_1053,
              
              DIS1 = v_1060,
              DIS2 = v_1061,
              DIS3 = v_1062,
              DIS4 = v_1063,
              DIS5 = v_1064,
              DIS6 = v_1065,
              DIS7 = v_1066,
              DIS8 = v_1067,
              DIS9 = v_1068,
              DIS10 = v_1069,
              DIS11 = v_1070,
              DIS12 = v_1071,
              DIS13 = v_1072,
              DIS14 = v_1073,
              DIS15 = v_1074,
              DIS16 = v_1075,
              DIS17 = v_1076,
              DIS18 = v_1077,
              DIS19 = v_1078,
              DIS20 = v_1079,
              DIS21 = v_1080,
              DIS22 = v_1081,
              DIS23 = v_1082,
              
              EPQ1 = v_1085,
              EPQ2 = v_1086,
              EPQ3 = v_1087,
              EPQ4 = v_1088,
              EPQ5 = v_1089,
              EPQ6 = v_1090,
              EPQ7 = v_1091,
              EPQ8 = v_1092,
              EPQ9 = v_1093,
              EPQ10 = v_1094,
              EPQ11 = v_1095,
              EPQ12 = v_1096,
              EPQ13 = v_1097,
              EPQ14 = v_1098,
              EPQ15 = v_1099,
              EPQ16 = v_1100,
              EPQ17 = v_1101,
              EPQ18 = v_1102,
              EPQ19 = v_1103,
              EPQ20 = v_1104,
              EPQ21 = v_1105,
              EPQ22 = v_1106,
              EPQ23 = v_1107,
              EPQ24 = v_1108,
              EPQ25 = v_1109,
              EPQ26 = v_1110,
              EPQ27 = v_1111,
              EPQ28 = v_1112,
              EPQ29 = v_1113,
              EPQ30 = v_1114,
              EPQ31 = v_1115,
              EPQ32 = v_1116,
              EPQ33 = v_1117,
              EPQ34 = v_1118,
              EPQ35 = v_1119,
              EPQ36 = v_1120,
              EPQ37 = v_1121,
              EPQ38 = v_1122,
              EPQ39 = v_1123,
              EPQ40 = v_1124,
              EPQ41 = v_1125,
              EPQ42 = v_1126,
              EPQ43 = v_1127,
              EPQ44 = v_1128,
              EPQ45 = v_1129,
              EPQ46 = v_1130,
              EPQ47 = v_1131,
              EPQ48 = v_1132,
              EPQ49 = v_1133,
              EPQ50 = v_1134,
              
              t1 = datetime)

# 3. Select the renamed columns ------------------------------------------------
dat <- subset(dat,
              select = c(subject,
                         sex,
                         day,
                         month,
                         year,
                         edu,
                         job,
                         hand,
                         typewr,
                         lens,
                         caffein,
                         games,
                         instru,
                         gamesfreq,
                         gamesfps,
                         gamesname,
                         instruname,
                         
                         DIS1,
                         DIS2,
                         DIS3,
                         DIS4,
                         DIS5,
                         DIS6,
                         DIS7,
                         DIS8,
                         DIS9,
                         DIS10,
                         DIS11,
                         DIS12,
                         DIS13,
                         DIS14,
                         DIS15,
                         DIS16,
                         DIS17,
                         DIS18,
                         DIS19,
                         DIS20,
                         DIS21,
                         DIS22,
                         DIS23,
                         
                         EPQ1,
                         EPQ2,
                         EPQ3,
                         EPQ4,
                         EPQ5,
                         EPQ6,
                         EPQ7,
                         EPQ8,
                         EPQ9,
                         EPQ10,
                         EPQ11,
                         EPQ12,
                         EPQ13,
                         EPQ14,
                         EPQ15,
                         EPQ16,
                         EPQ17,
                         EPQ18,
                         EPQ19,
                         EPQ20,
                         EPQ21,
                         EPQ22,
                         EPQ23,
                         EPQ24,
                         EPQ25,
                         EPQ26,
                         EPQ27,
                         EPQ28,
                         EPQ29,
                         EPQ30,
                         EPQ31,
                         EPQ32,
                         EPQ33,
                         EPQ34,
                         EPQ35,
                         EPQ36,
                         EPQ37,
                         EPQ38,
                         EPQ39,
                         EPQ40,
                         EPQ41,
                         EPQ42,
                         EPQ43,
                         EPQ44,
                         EPQ45,
                         EPQ46,
                         EPQ47,
                         EPQ48,
                         EPQ49,
                         EPQ50,
                         
                         t1))

# 4. Drop subjects --------- ---------------------------------------------------
dat <- subset(dat,
              caffein != -66 &
              EPQ50 != -77 &
              subject < 350 &
              job != "wer")

# 5. Compute age of subjects ---------------------------------------------------
# 5.1 Extract substring of t1 --------------------------------------------------
dat$t1 <- substr(dat$t1, 1,10)

# 5.2 Create variable birth out of the variables day, month & year -------------
dat$birth <- as.Date(paste(dat$year , dat$month, dat$day, sep = "-"))
dat[31,92] # wrong date ("0093-08-11") given by a participant
dat[31,92] <- as.Date("1993-08-11")
dat[34,92] # wrong date ("2015-03-15") given by a participant
dat[34,92] <- as.Date("1990-03-15")
dat[76,92] # wrong date ("0094-09-14") given by a participant
dat[76,92] <- as.Date("1994-09-14")
dat[92,92] # wrong date ("0090-03-27") given by a participant
dat[92,92] <- as.Date("1990-03-27")
dat[96,92] # wrong date ("0092-09-17") given by a participant
dat[96,92] <- as.Date("1992-09-17")

# 5.2.1 Edit date for t1 (subjects from Oli's project) -------------------------
dat[51,91] # wrong date, t1 of subject 41 was 2015-10-21 
dat[51,91] <- "2015-10-12" #correct for wrong t1
dat[51,91]

dat[76,92] <- "1994-09-14"
dat[76,92]

dat[92,92] <- "1990-03-27"
dat[92,92]

dat[96,92] <- "1992-09-17"
dat[96,92]


# 5.3 Compute age of subjects --------------------------------------------------
dat$aged <- difftime(as.Date(dat$t1), as.Date(dat$birth))
dat$agey <- floor(dat$age/365.25)

length(dat)

# 5.4 Drop variables day, month & year -----------------------------------------
dat <- subset(dat,, -c(day, month, year))

# 6. Rearrange the data frame --------------------------------------------------
dat <- dat[,c(1,89,88,90,91,2:87)]
dat <- arrange(dat, subject)

# 7. Convert categorical items to factor
dat[,c(6:7,9:11,13:16)] <- lapply(dat[,c(6:7,9:11,13:16)], factor)

# 8. Convert all DIS and EPQ items to numeric
dat[,c(1,19:91)] <- lapply(dat[,c(1,19:91)], as.numeric)








# 9. DIS Scale (cf. Kuhmann & Ising, 1996) -------------------------------------
# Im file enthalten sind Items aus Rebecca Indermühle's Projekt. Fülleritems
# sind darin nicht mehr enthalten. Der Fragebogen besteht noch aus 11 funktion-
# alen Items und 12 dysfunktionalen Items. Einige Items wurden leicht um-
# formuliert (cf. 4, 12, 20). 
# 9.1 Recode Impulsivity Items 1, 4, 5, 11, 18, 19, 20, 21, 22 
names(dat[,c(19,22,23,29,36,37,38,39,40)])
dat[,c(19,22,23,29,36,37,38,39,40)] <- 3-dat[,c(19,22,23,29,36,37,38,39,40)]

# 9.2 Create DISfun ------------------------------------------------------------
names(dat[,c(19, 21, 23, 25, 27, 29, 31, 33, 35, 37, 39)])
dat$DISfun <- rowSums(dat[,c(19, 21, 23, 25, 27, 29, 31, 33, 35, 37, 39)])

# 9.3 Create DISdis -----------------------------------------------------------
names(dat[,c(20,22,24,26,28,30,32,34,36,38,40,41)])
dat$DISdis <- rowSums(dat[,c(20,22,24,26,28,30,32,34,36,38,40,41)])

# 9.4 DIS scales: Reliability Analysis ----------------------------------------
# Es scheinen immer noch Items im dataframe zu sein, die nicht korrekt
# gepolt sind (siehe Fehlermeldung der alpha-Funktion). Der Code unter 9.1 
# scheint aber korrekt zu sein.
#library(psych)
alpha(dat[,c(19, 21, 23, 25, 27, 29, 31, 33, 35, 37, 39)])$total$raw_alpha
alpha(dat[,c(20,22,24,26,28,30,32,34,36,38,40,41)])$total$raw_alpha

corr.test(dat[92:93])







# 10. EPQ-RK (Ruch, 1999) ------------------------------------------------------
# Im file enthalten sind Items aus Rebecca Indermühle's Projekt. Der Fragebogen
# besteht aus 14 Psychotizismus-Items, 12 Extraversions-Items, 12 Neurotizismus-
# Items und 12 Lügen-Items. Die Codierung der Items oder der Fragebogen muss
# nochmals überprüft werden, weil es nach der Umpolung noch falsch codierte
# Items in der P-Scale hat (siehe Fehlermeldung der alpha-Funktion)
# 10.1 recode items ----
names(dat[,c(44,45,47,48,51,53,56,59,63,64,66,69,70,72,74,76,78,80,81,85,86,89,91)])
dat[,c(44,45,47,48,51,53,56,59,63,64,66,69,70,72,74,76,78,80,81,85,86,89,91)] <- 3-dat[,c(44,45,47,48,51,53,56,59,63,64,66,69,70,72,74,76,78,80,81,85,86,89,91)]

# 10.2 P scale (14 items) ------------------
names(dat[,c(44,47,48,51,53,57,63,67,72,78,80,85,88,91)])
dat$EPQpsych <- rowSums(dat[,c(44,47,48,51,53,57,63,67,72,78,80,85,88,91)])
alpha(dat[,c(44,47,48,51,53,57,63,67,72,78,80,85,88,91)])

# 10.3 E scale (12 items) ------------------------------------------------------
names(dat[,c(43,46,49,52,56,65,66,68,71,77,87,90)])
dat$EPQextra <- rowSums(dat[,c(43,46,49,52,56,65,66,68,71,77,87,90)])
alpha(dat[,c(43,46,49,52,56,65,66,68,71,77,87,90)])$total$raw_alpha

# 10.4 N scale (12 items) ------------------
names(dat[,c(42,50,54,58,60,61,62,73,75,79,82,83)])
dat$EPQneuro <- rowSums(dat[,c(42,50,54,58,60,61,62,73,75,79,82,83)])
alpha(dat[,c(42,50,54,58,60,61,62,73,75,79,82,83)])$total$raw_alpha

# 10.5 L scale (12 items) --------------------
names(dat[,c(45,55,59,64,69,70,74,76,81,84,86,89)])
dat$EPQlie <- rowSums(dat[,c(45,55,59,64,69,70,74,76,81,84,86,89)])
alpha(dat[,c(45,55,59,64,69,70,74,76,81,84,86,89)])$total$raw_alpha

corr.test(dat[,94:97])

# 11. Drop unused levels of factors ---------------------
dat <- droplevels(dat)

# 12. check data frame

str(dat)
# 13. Cleaning up
resultsFragebogen <- dat
rm(dat)
str(resultsFragebogen)
lapply(subset(resultsFragebogen, select = c(sex, edu, hand, typewr)), summary)
summary(as.numeric(resultsFragebogen$agey))
nrow(resultsFragebogen)

# 1 = obligatorische Schulzeit,
# 2 = Berufslehre,
# 3 = Berufsmatura,
# 4 = Matura,
# 5 = Bachelor,
# 6 = Master/Lizenziat,
# 7 = anderes





