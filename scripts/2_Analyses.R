# ---
# Title: "2_Analyses.R"
# Description: "This code reproduces the statistics in text and in tables. In
# order for this code to work, you will need to run the script called
# '1_Read_in_data.R' first."
# Author: "Philipp Thomas"
# ---

# 2. Methode ===================================================================
# . . 2.1 Stichprobe -----------------------------------------------------------
nrow(dat)                            # sample size
summary(dat$sex)                     # summary for sex
summary(dat$agey)                    # summary for age
sd(dat$agey) %>% round(digits = 2)   # age sd
summary(dat$edu)                     # summary for education
summary(dat$language)                # summary for language

# . . 2.6 Untersuchungsablauf --------------------------------------------------
summary(dat$tdiff) %>% round(digits = 0)
(dat$tdiff) %>%                            # plot testing interval (i.e.
  hist(breaks = 100)



# 3. Resultate =================================================================
# . . 3.1 Deskriptiv und Inferenzstatistik -------------------------------------
# . . . . 3.1.1 Spatial-Suppression-Aufgabe ------------------------------------
# . . . . . . Tabelle 2 --------------------------------------------------------
# . . . . . . . . . . Mean, SD, Min, Max, Skew, Kurtosis -----------------------
# . . . . . . . . . . log values [reported in the table]
# . . . . . . . . . . Mean -----------------------------------------------------
dat %>%
  select(S1log10mean, S2log10mean, S3log10mean, S4log10mean) %>%
  lapply(., mean) %>%
  lapply(., function(x) 10 ^ x) %>%
  sapply(., round, digits = 0)

# . . . . . . . . . . SD -------------------------------------------------------
sdLog <- dat %>%
  select(S1log10mean, S2log10mean, S3log10mean, S4log10mean) %>%
  lapply(., sd)

meanLog <- dat %>%
  select(S1log10mean, S2log10mean, S3log10mean, S4log10mean) %>%
  lapply(., mean)

highSD <- (10 ^ (unlist(meanLog) +  unlist(sdLog)) %>%
  round(digits = 0)) - 10 ^ unlist(meanLog) %>%
  round()

lowSD <- (10 ^ (unlist(meanLog) -  unlist(sdLog)) %>%
  round(digits = 0)) - 10 ^ unlist(meanLog) %>%
  round()

(abs(highSD) + abs(lowSD)) / 2      # sd of conditions
rm(list = setdiff(ls(), "dat"))     # remove unneeded objects from workspace

# . . . . . . . . . . Min ------------------------------------------------------
dat %>%
  select(S1log10mean, S2log10mean, S3log10mean, S4log10mean) %>%
  lapply(., min) %>%
  lapply(., function(x) 10 ^ x) %>%
  sapply(., round, digits = 0)

# . . . . . . . . . . Max ------------------------------------------------------
dat %>%
  select(S1log10mean, S2log10mean, S3log10mean, S4log10mean) %>%
  lapply(., max) %>%
  lapply(., function(x) 10 ^ x) %>%
  sapply(., round, digits = 0)

# . . . . . . . . . . Skew -----------------------------------------------------
dat %>%
  select(S1log10mean, S2log10mean, S3log10mean, S4log10mean) %>%
  lapply(., skew) %>%
  sapply(., round, digits = 2)

# . . . . . . . . . . Kurtosis -------------------------------------------------
dat %>%
  select(S1log10mean, S2log10mean, S3log10mean, S4log10mean) %>%
  lapply(., kurtosi) %>%
  sapply(., round, digits = 2)

# . . . . . . . . . . Shapiro-Wilk test ----------------------------------------
dat %>%
  select(S1log10mean, S2log10mean, S3log10mean, S4log10mean) %>%
  sapply(., shapiro.test) %>%
  t()

# . . . . . . . . . . Split-half reliabilities ---------------------------------
# Define Spearman-Brown-Correction
spearman.brown.correction <- function(x){
  (2 * x)/(1 + x)
}

# Corrected split-half reliabilty for 1.8° condition (S1)
dat %>%
  select(S1oddMean, S1evenMean) %>%
  corr.test() %>%
  .$ci %>%
  .$r %>%
  spearman.brown.correction() %>%
  round(digits = 2)

# Corrected split-half reliabilty for 3.6° condition (S2)
dat %>%
  select(S2oddMean, S2evenMean) %>%
  corr.test() %>%
  .$ci %>%
  .$r %>%
  spearman.brown.correction() %>%
  round(digits = 2)

# Corrected split-half reliabilty for 5.4° condition (S3)
dat %>%
  select(S3oddMean, S3evenMean) %>%
  corr.test() %>%
  .$ci %>%
  .$r %>%
  spearman.brown.correction() %>%
  round(digits = 2)

# Corrected split-half reliabilty for 7.2° condition (S4)
dat %>%
  select(S4oddMean, S4evenMean) %>%
  corr.test() %>%
  .$ci %>%
  .$r %>%
  spearman.brown.correction() %>%
  round(digits = 2)

# . . . . . . . . . . Inverted values [NOT reported in the table]
# . . . . . . . . . . Mean, SD, Min, Max, Skew, Kurtosis, Shapiro-Wilk-Test ----
descriptives.function <- function(x) {    # define function
  c(M = round(mean(x), digits = 0),
    SD = round(sd(x), digits = 0),
    Min = round(min(x), digits = 0),
    Max = round(max(x), digits = 0),
    Schiefe  = round(skew(x), digits = 2),
    Kurtosis = round(kurtosi(x), digits = 2))
}

dat %>%
  select(S1mean, S2mean, S3mean, S4mean) %>%
  sapply(., descriptives.function) %>%
  t()

dat %>%
  select(S1mean, S2mean, S3mean, S4mean) %>%
  sapply(., shapiro.test) %>%
  t()

rm(list = setdiff(ls(), "dat")) # Remove objects of this paragraph

# . . . . . . Repeated measures ANOVA ------------------------------------------
# Parametric approach: Test for mean difference between conditions.
# . . . . . . . . . . Preparatory work -----------------------------------------
# extract variables of interest
datWide <- dat %>%
  select(subject, S1log10mean, S2log10mean, S3log10mean, S4log10mean)

# convert subject to factor
datWide$subject <- factor(datWide$subject)

# reshape to long format
datLong <- melt(datWide,
                id.vars = "subject",
                variable.name="condition",
                value.name="threshold")

# rearrange data frame and inspect 'datLong'
datLong <- arrange(datLong, subject)
tbl_df(datLong)

# visualise mean differences with boxplot
qplot(condition, threshold, data = datLong, geom = "boxplot")

# . . . . . . . . . . Run repeated measures anova ------------------------------
ez.spatial.suppression <- ezANOVA(data = datLong,
                                   dv = threshold,
                                   wid = subject,
                                   within = condition,
                                   detailed = TRUE,
                                   return_aov = TRUE)

# Mauchly's Test for Sphericity probability (p) value
ez.spatial.suppression %>% .$`Mauchly's Test for Sphericity`

# Compute chi square value of Mauchly's Test
# (see http://www.originlab.com/pdfs/nagcl09/manual/pdf/rmanova/
# Repeated_Measures_ANOVA.pdf)
l  <- 4                                        # number of levels
d  <- l - 1                                    # number of levels - 1
n  <- 177                                      # sample size
((d * (d + 1)) / 2) - 1                        # degree's of freedom
W  <- ez.spatial.suppression[[2]][2]           # Mauchly's W from ezANOVA
(((2 * d ^ 2 + d + 2) / 6 * d) - n - 1) * log(W) # approximate chi square

# Greenhouse-Geisser epsilon
ez.spatial.suppression %>%
  .$`Sphericity Corrections` %>%
  .$GGe %>%
  round(digits = 2)

# Anova output
ez.spatial.suppression %>% .$ANOVA

# Calculate adjusted degree's of freedom
3   * .55   # numerator
528 * .55   # denominator

# Remove unneeded objects from workspace
rm(list = setdiff(ls(), c("dat", "datLong", "ez.spatial.suppression")))

# . . . . . . . . . . Compute post hoc tests -----------------------------------
lme.spatial.suppression <- lme(threshold ~ condition,
                               random = ~1 | subject / condition,
                               data = datLong)  # Define model

anova(lme.spatial.suppression)[2, ]             # same as ez.spatial.suppression
lme.spatial.suppression %>% summary()           # see summary of lme
posthoc.spatial.suppression <- lme.spatial.suppression %>%
  glht(., linfct = mcp(condition = "Tukey"))    # run post hoc tests
posthoc.spatial.suppression %>% summary         # inspect them

# remove unneeded objects from workspace
rm(list = setdiff(ls(), c("dat", "datLong")))

# . . . . . . Friedman test ----------------------------------------------------
# Non parametric approach (see Appendix B)
# . . . . . . . . . . Run Friedman test ----------------------------------------
dat %>%
  select(S1log10mean, S2log10mean, S3log10mean, S4log10mean) %>%
  as.matrix() %>%
  friedman.test()

# . . . . . . . . . . Compute post hoc tests -----------------------------------
# To compute post hoc tests, you need to load the function
# 'friedman.test.with.post.hoc' into your workspace. Run this line.
# The function should then appear under 'Functions' in your workspace.
source("scripts/source_scripts/Friedman_function.R")

# Compute post hoc tests
friedman.test.with.post.hoc(threshold ~ condition | subject, datLong)

# . . . . . . Tabelle 3 --------------------------------------------------------
# Compute effect sizes for all comparisons
dat %>%
  as.data.frame() %>%
  summarize(cohensd_s1_s2 = cohen.d(S1log10mean, S2log10mean, paired = TRUE)$estimate,
            cohensd_s1_s3 = cohen.d(S1log10mean, S3log10mean, paired = TRUE)$estimate,
            cohensd_s1_s4 = cohen.d(S1log10mean, S4log10mean, paired = TRUE)$estimate,
            cohensd_s2_s3 = cohen.d(S2log10mean, S3log10mean, paired = TRUE)$estimate,
            cohensd_s2_s4 = cohen.d(S2log10mean, S4log10mean, paired = TRUE)$estimate,
            cohensd_s3_s4 = cohen.d(S3log10mean, S4log10mean, paired = TRUE)$estimate) %>%
  round(digits = 2) %>%
  abs()

# remove objects created in this section, only keep "dat" and post hoc funtion
rm(list = setdiff(ls(), c("dat")))

# . . . . . . Descriptives for suppression index (si) --------------------------
dat %>% select(si) %>% unlist() %>% mean() %>% round(digits = 3)    # mean
dat %>% select(si) %>% unlist() %>% sd() %>% round(digits = 3)      # sd
dat %>% select(si) %>% unlist() %>% min() %>% round(digits = 3)     # min
dat %>% select(si) %>% unlist() %>% max() %>% round(digits = 3)     # max
dat %>% select(si) %>% unlist() %>% skew() %>% round(digits = 2)    # skew
dat %>% select(si) %>% unlist() %>% kurtosi() %>% round(digits = 2) # kurtosis
dat %>% select(si) %>% unlist() %>% shapiro.test()                  # shapiro

# . . . . 3.1.2 Hick task ------------------------------------------------------
# . . . . . . Tabelle 4 --------------------------------------------------------
# . . . . . . . . . . Mean, SD, Min, Max, Skew, Kurtosis, Shapiro-Wilk-Test ----
descriptives.function <- function(x) {    # define function
  c(M = round(mean(x), digits = 0),
    SD = round(sd(x), digits = 0),
    Min = round(min(x), digits = 0),
    Max = round(max(x), digits = 0),
    Schiefe  = round(skew(x), digits = 2),
    Kurtosis = round(kurtosi(x), digits = 2))
}

dat %>%
  select(H0meanRT, H1meanRT, H2meanRT, H258meanRT) %>%
  sapply(., descriptives.function) %>%
  t()

rm(list = setdiff(ls(), "dat"))     # Remove objects of this paragraph

# . . . . . . . . . . Shapiro-Wilk test ----------------------------------------
dat %>%
  select(H0meanRT, H1meanRT, H2meanRT, H258meanRT) %>%
  sapply(., shapiro.test) %>%
  t()

# . . . . . . . . . . Split-half reliabilities ---------------------------------
# Define Spearman-Brown-Correction
spearman.brown.correction <- function(x){
  (2 * x)/(1 + x)
}

# Corrected split-half reliabilty for 0-bit condition (H0)
dat %>%
  select(H0oddMean, H0evenMean) %>%
  corr.test() %>%
  .$ci %>%
  .$r %>%
  spearman.brown.correction() %>%
  round(digits = 2)

# Corrected split-half reliabilty for 1-bit condition (H1)
dat %>%
  select(H1oddMean, H1evenMean) %>%
  corr.test() %>%
  .$ci %>%
  .$r %>%
  spearman.brown.correction() %>%
  round(digits = 2)

# Corrected split-half reliabilty for 2-bit condition (H2)
dat %>%
  select(H2oddMean, H2evenMean) %>%
  corr.test() %>%
  .$ci %>%
  .$r %>%
  spearman.brown.correction() %>%
  round(digits = 2)

# Corrected split-half reliabilty for 2.58-bit condition (H258)
dat %>%
  select(H258oddMean, H258evenMean) %>%
  corr.test() %>%
  .$ci %>%
  .$r %>%
  spearman.brown.correction() %>%
  round(digits = 2)

rm(list = setdiff(ls(), "dat")) # remove unneeded objects from workspace

# . . . . . . Repeated measures ANOVA ------------------------------------------
# Parametric approach: Test for mean difference between conditions.
# . . . . . . . . . . Preparatory work -----------------------------------------
# extract variables of interest
datWide <- dat %>%
  select(subject, H0meanRT, H1meanRT, H2meanRT, H258meanRT)

# convert subject to factor
datWide$subject <- factor(datWide$subject)

# reshape to long format
datLong <- melt(datWide,
                id.vars = "subject",
                variable.name="condition",
                value.name="reactionTime")

# arrange and inspect 'datLong'
datLong <- arrange(datLong, subject)
tbl_df(datLong)

# visualise mean differences with boxplot
qplot(condition, reactionTime, data = datLong, geom = "boxplot")

# . . . . . . . . . . Run repeated measures anova ------------------------------
# install.packages("ez")
# library(ez)
ez.hick <- ezANOVA(data = datLong,
                   dv = reactionTime,
                   wid = subject,
                   within = condition,
                   detailed = TRUE,
                   return_aov = TRUE)

# Mauchly's Test for Sphericity
ez.hick %>% .$`Mauchly's Test for Sphericity`

# Compute chi square value of Mauchly's Test
# (see http://www.originlab.com/pdfs/nagcl09/manual/pdf/rmanova/
# Repeated_Measures_ANOVA.pdf)
l  <- 4                                        # number of levels
d  <- l - 1                                    # number of levels - 1
n  <- 177                                      # sample size
((d * (d + 1)) / 2) - 1                        # degree's of freedom
W  <- ez.hick[[2]][2]                          # Mauchly's W from ezANOVA
(((2 * d ^ 2 + d + 2) / 6 * d) - n - 1) * log(W) # approximate chi square

# Greenhouse-Geisser epsilon
ez.hick %>%
  .$`Sphericity Corrections` %>%
  .$GGe %>%
  round(digits = 2)

# Anova output
ez.hick %>% .$ANOVA

# Calculate adjusted degree's of freedom
3   * .57   # numerator
528 * .57   # denominator

# remove objects created in this section, only keep "dat", "datLong",
# and post hoc funtion
rm(list = setdiff(ls(), c("dat", "datLong", "friedman.test.with.post.hoc")))

# . . . . . . . . . . Compute post hoc tests -----------------------------------
lme.hick <- lme(reactionTime ~ condition,
                random = ~1 | subject / condition,
                data = datLong)                 # specify model

anova(lme.hick)[2, ]                            # same as ez.spatial.suppression
lme.hick %>% summary()                          # see summary of lme
posthoc.hick <- lme.hick %>%
  glht(., linfct = mcp(condition = "Tukey"))    # run post hoc tests
posthoc.hick %>% summary                        # inspect them

# remove objects created in this section, only keep "dat", "datLong",
# and post hoc funtion
rm(list = setdiff(ls(), c("dat", "datLong", "friedman.test.with.post.hoc")))

# . . . . . . Friedman test ----------------------------------------------------
dat %>%
  select(H0meanRT, H1meanRT, H2meanRT, H258meanRT) %>%
  as.matrix() %>%
  friedman.test()

# . . . . . . . . . . Compute post hoc tests -----------------------------------
# To compute post hoc tests, you need to load the function
# 'friedman.test.with.post.hoc' into your workspace. Run this line.
# The function should then appear under 'Functions' in your workspace.
source("scripts/source_scripts/Friedman_function.R")

# Compute post hoc tests
friedman.test.with.post.hoc(reactionTime ~ condition | subject, datLong)

# remove objects created in this section, only keep "dat"
rm(list = setdiff(ls(), "dat"))

# . . . . . . Tabelle 5 --------------------------------------------------------
# Compute effect sizes for all comparisons
dat %>%
  as.data.frame() %>%
  summarize(cohensd_H0_H1    = cohen.d(H0meanRT, H1meanRT,   paired = TRUE)$estimate,
            cohensd_H0_H2    = cohen.d(H0meanRT, H2meanRT,   paired = TRUE)$estimate,
            cohensd_H0_H2.58 = cohen.d(H0meanRT, H258meanRT, paired = TRUE)$estimate,
            cohensd_H1_H2    = cohen.d(H1meanRT, H2meanRT,   paired = TRUE)$estimate,
            cohensd_H1_H2.58 = cohen.d(H1meanRT, H258meanRT, paired = TRUE)$estimate,
            cohensd_H2_H2.58 = cohen.d(H2meanRT, H258meanRT, paired = TRUE)$estimate) %>%
  round(digits = 2) %>%
  abs()



# . . . . 3.1.3 BIS-Test -------------------------------------------------------
# . . . . . . Tabelle 6 --------------------------------------------------------
# To reproduce the mean, sd, min, max, skew, kurtosis, and shapiro wilk
# p values of this table, you would need raw data (instead of z standardised
# data). The object 'dat' does not contain raw data. You have to trust me
# on this one :)

# . . . . . . Descriptives for BIS z-value -------------------------------------
dat %>% select(zTotal) %>% unlist() %>% mean() %>% round(digits = 2) # mean
dat %>% select(zTotal) %>% unlist() %>% sd() %>% round(digits = 2)   # sd
dat %>% select(zTotal) %>% unlist() %>% min() %>% round(digits = 2)  # min
dat %>% select(zTotal) %>% unlist() %>% max() %>% round(digits = 2)  # max
dat %>% select(zTotal) %>% unlist() %>% skew() %>% round(digits = 2) # skew
dat %>% select(zTotal) %>% unlist() %>% kurtosi() %>% round(digits = 2) # kurto
dat %>% select(zTotal) %>% unlist() %>% shapiro.test() %>% .$p.value %>%  round(digits=2)

# . . . . . . Tabelle 7 --------------------------------------------------------
# . . . . . . . . Product-moment correlations (r) ------------------------------
dat %>%
  select(zOG, zZN, zAN, zXG, zWA, zZP, zTM, zBD,
         zSC, zST, zCH, zTG, zRZ, zWM,zKW, zZZ, zOE, zWE) %>%
  corr.test() %>%
  .$r %>%
  round(digits = 2)

# . . . . . . . . Respective probabilities (p) ------------------------------
dat %>%
  select(zOG, zZN, zAN, zXG, zWA, zZP, zTM, zBD,
         zSC, zST, zCH, zTG, zRZ, zWM,zKW, zZZ, zOE, zWE) %>%
  corr.test() %>%
  .$p %>%
  round(digits = 3)

# . . . . . . Tabelle A2 -------------------------------------------------------
# . . . . . . . . Rank-order correlations (r) ----------------------------------
dat %>%
  select(zOG, zZN, zAN, zXG, zWA, zZP, zTM, zBD,
         zSC, zST, zCH, zTG, zRZ, zWM,zKW, zZZ, zOE, zWE) %>%
  corr.test(., method = "spearman") %>%
  .$r %>%
  round(digits = 3)

# . . . . . . . . Respective probabilites (p) ----------------------------------
dat %>%
  select(zOG, zZN, zAN, zXG, zWA, zZP, zTM, zBD,
         zSC, zST, zCH, zTG, zRZ, zWM,zKW, zZZ, zOE, zWE) %>%
  corr.test(., method = "spearman") %>%
  .$p %>%
  round(digits = 3)

# . . . . . . . . . . Difference between para- and nonparametric version -------
# Create object 'product', which contains Product-moment correlations
product <- dat %>%
  select(zOG, zZN, zAN, zXG, zWA, zZP, zTM, zBD,
         zSC, zST, zCH, zTG, zRZ, zWM,zKW, zZZ, zOE, zWE) %>%
  corr.test() %>%
  .$r

# Do the same for Rank-order correlations (object 'rankorder')
rankorder <- dat %>%
  select(zOG, zZN, zAN, zXG, zWA, zZP, zTM, zBD,
         zSC, zST, zCH, zTG, zRZ, zWM,zKW, zZZ, zOE, zWE) %>%
  corr.test(., method = "spearman") %>%
  .$r

# Subract both vectors, and create data frame
difference <- (product - rankorder) %>% as.data.frame()
difference[lower.tri(difference)] <- 0
sapply(difference, round, digits = 2)

# remove objects created in this section, only keep "dat"
rm(list = setdiff(ls(), "dat"))

# . . . . 3.1.4 Zusammenhänge zwischen den Aufgaben ----------------------------
# . . . . . . Tabelle 8 --------------------------------------------------------
# . . . . . . . . Product-moment correlations (r) ------------------------------
dat %>%
  select(S1log10mean, S2log10mean, S3log10mean, S4log10mean, si,
         H0meanRT, H1meanRT, H2meanRT, H258meanRT,
         zTotal, g) %>%
  corr.test() %>%
  .$r %>%
  round(digits = 2)

# . . . . . . . . Resepctive probabilities (p) ---------------------------------
dat %>%
  select(S1log10mean, S2log10mean, S3log10mean, S4log10mean, si,
         H0meanRT, H1meanRT, H2meanRT, H258meanRT,
         zTotal, g) %>%
  corr.test() %>%
  .$p %>%
  round(digits=2)

# . . . . . . Tabelle A3 -------------------------------------------------------
# . . . . . . . . Rank-order correlations (r) ----------------------------------
dat %>%
  select(S1log10mean, S2log10mean, S3log10mean, S4log10mean, si,
         H0meanRT, H1meanRT, H2meanRT, H258meanRT,
         zTotal, g) %>%
  corr.test(., method = "spearman")

# . . . . . . . . Respective probabilites (p) ----------------------------------
dat %>%
  select(S1log10mean, S2log10mean, S3log10mean, S4log10mean, si,
         H0meanRT, H1meanRT, H2meanRT, H258meanRT,
         zTotal, g) %>%
  corr.test(., method = "spearman") %>%
  .$p %>%
  round(digits=5)


# . . . . . . . . . . Difference between para- and nonparametric version -------
# Create object 'product', which contains Product-moment correlations
product <- dat %>%
  select(S1log10mean, S2log10mean, S3log10mean, S4log10mean, si,
         H0meanRT, H1meanRT, H2meanRT, H258meanRT,
         zTotal, g) %>%
  corr.test() %>%
  .$r %>%
  round(digits = 2)

# Do the same for Rank-order correlations (object 'rankorder')
rankorder <- dat %>%
  select(S1log10mean, S2log10mean, S3log10mean, S4log10mean, si,
         H0meanRT, H1meanRT, H2meanRT, H258meanRT,
         zTotal, g) %>%
  corr.test(., method = "spearman") %>%
  .$r %>%
  round(digits = 2)

# Subract both vectors, and create data frame
difference <- (product - rankorder) %>% as.data.frame()
difference[lower.tri(difference)] <- 0

# remove objects created in this section, only keep "dat"
rm(list = setdiff(ls(), "dat"))

# . . 3.2 1. Fragestellung -----------------------------------------------------
# . . . . . . Correlation between suppression index and BIS z-score ------------
dat %>%
  select(si, zTotal) %>%
  corr.test()

# . . . . . . Scatterplot of relationship between suppression index and BIS z-score
qplot(data = dat, si, zTotal)

# . . . . . . Test if the obtained coefficient is significantly different ------
# from Melnick et al.'s (2013) reported correlation
r.test(n = 12, r12 = .64, n2 = 177, r34 = .00)        # study 1
r.test(n = 53, r12 = .71, n2 = 177, r34 = .00)        # study 2
r.test(n = 53, r12 = -.46, n2 = 177, r34 = -.16)      # small stimuli and IQ

# . . . . . . Semipartial correlation between 1.8° condition and z-score -------
# (controlling for 7.2° condition)
spcor.test(x = dat$zTotal,
           y = dat$S1log10mean,
           z = dat$S4log10mean,
           method = "pearson")

# . . . . . . Semipartial correlation between 7.2° condition and z-score -------
# (controlling for 1.8° condition)
spcor.test(x = dat$zTotal,
           y = dat$S4log10mean,
           z = dat$S1log10mean,
           method = "pearson")

# . . . . . . Test if the obtained coefficients are significantly different ----
# from Melnick et al.'s (2013) reported correlations
r.test(n = 53, r12 = -.71, n2 = 177, r34 = -.11)      # 1.8° condition and IQ
r.test(n = 53, r12 = .55, n2 = 177, r34 = -.04)       # 7.2° condition and IQ



# . . 3.3 2. Fragestellung -----------------------------------------------------
# . . . . . . Tabelle 9 --------------------------------------------------------
# . . . . . . . . . . Mean, SD, Min, Max, Skew, Kurtosis -----------------------
descriptives.function <- function(x) {    # define function
  c(M = round(mean(x), digits = 3),
    SD = round(sd(x), digits = 3),
    Min = round(min(x), digits = 3),
    Max = round(max(x), digits = 3),
    Schiefe  = round(skew(x), digits = 2),
    Kurtosis = round(kurtosi(x), digits = 2))
}

dat %>%
  select(Sasymptote, Sslope)  %>%
  sapply(., descriptives.function) %>%
  t()

rm(list = setdiff(ls(), "dat"))     # Remove objects of this paragraph

# . . . . . . . . . . Shapiro-Wilk test ----------------------------------------
dat %>%
  select(Sasymptote, Sslope) %>%
  sapply(., shapiro.test) %>%
  t()

# . . . . . . Descriptives for RMSE --------------------------------------------
dat %>%
  select(Srmse) %>%
  unlist() %>%
  summary() %>%
  round(digits = 0)

# For more infos about the RMSE cutoff analyses see
# "scripts/tikzDevice/spatial_suppression_rmse_cutoff.R"

# . . . . . . Test if the obtained coefficients are significantly different ----
# from Melnick et al.'s (2013) reported correlation
r.test(n = 177, r12 = .96, n2 = 65, r34 = .996)  # between si and slope
r.test(n = 177, r12 = .00, n2 = 65, r34 = .68)   # between slope and IQ

# . . . . . . Correlation between asymptote and BIS z-score --------------------
dat %>%
  select(Sasymptote, zTotal) %>%
  corr.test()

# . . . . . . Correlation between slope and BIS z-score ------------------------
dat %>%
  select(Sslope, zTotal) %>%
  corr.test()



# . . 3.4 3. Fragestellung -----------------------------------------------------
# . . . . . . Model 1: Congeneric spatial suppression model --------------------
# Define model
model1 <- 'gSupp =~ S1log10mean + S2log10mean + S3log10mean + S4log10mean'

# Estimate model
model1fit <- sem(model1, data = dat, estimator = "MLM", mimic = "Mplus")

# Show summary of model fit
summary(model1fit, fit.measures  = T, standardized  = T, rsquare = T)

# Visualise model
semPaths(model1fit,what= "std",style = "lisrel",layout= "tree", structural = F,
         cut= .60,residScale= 7, sizeMan= 7,sizeLat= 5,intercepts= F,
         optimizeLatRes = T,exoVar= T,nCharNodes= 0,asize= 1.5,esize= 1,
         edge.color= 1,edge.label.cex = .5,cardinal= F,nDigits= 3,rotation= 1)

# Remove objects of this paragraph
rm(model1, model1fit)

# . . . . . . Model X: Congeneric BIS model ------------------------------------
# Define model
congenericBIS <- 'g =~ zCapacity + zSpeed + zMemory'

# Estimate model
congenericBISfit <- sem(congenericBIS, data = dat, estimator = "MLM", mimic = "Mplus")

# Show summary of model fit (0 degrees of freedom!)
summary(congenericBISfit, fit.measures  = T, standardized  = T, rsquare = T)

# Remove objects of this paragraph
rm(congenericBIS, congenericBISfit)

# . . . . . . Model 2: Structural equation model (g on S) ----------------------
# Define model
model2 <- 'gSupp =~ S1log10mean + S2log10mean + S3log10mean + S4log10mean
           g =~ zCapacity + zSpeed + zMemory
           g ~ gSupp'

# Estimate model
model2fit <- sem(model2, data = dat, estimator = "MLM", mimic = "Mplus")

# Show summary of model fit
summary(model2fit, fit.measures  = T,standardized  = T,rsquare= T)

# Visualise model
semPaths(model2fit,what= "std",style = "lisrel",layout= "tree", structural = F,
         cut= .60,residScale= 7, sizeMan= 7,sizeLat= 5,intercepts= F,
         optimizeLatRes = T,exoVar= T,nCharNodes= 0,asize= 1.5,esize= 1,
         edge.color= 1,edge.label.cex = .5,cardinal= F,nDigits= 3,rotation= 2)

# Remove objects of this paragraph
rm(model2, model2fit)



# . . 3.5 4. Fragestellung -----------------------------------------------------
# . . . . . . Model 3: y = e ^ {.103 * x}, x € {1, 2, 3, 4} --------------------
# Compute factor loadings needed for this model (y) and plot function
x <- c(1:4)                                         # x values
exponentialfunction <- function(x){exp(.103 * x)}   # function
y <- exponentialfunction(x)                         # y values
plot(x, y, type = "o", bty = "n")                   # plot x and y values
y                                                   # print y values

# Define model
model3 <- 'kon =~ 1 * S1log10mean +
                  1 * S2log10mean +
                  1 * S3log10mean +
                  1 * S4log10mean
           dyn =~ 1.108491 * S1log10mean +
                  1.228753 * S2log10mean +
                  1.362062 * S3log10mean +
                  1.509834 * S4log10mean
           kon ~~ 0 * dyn'

# Estimate model
model3fit <- sem(model3, data = dat, estimator = "MLM", mimic = "Mplus")

# Show summary of model fit
summary(model3fit, fit.measures  = T, standardized  = T, rsquare = T)

# Visualise model
semPaths(model3fit,what= "std",style = "lisrel",layout= "tree", structural = F,
         cut= .60,residScale= 7, sizeMan= 7,sizeLat= 5,intercepts= F,
         optimizeLatRes = T,exoVar= T,nCharNodes= 0,asize= 1.5,esize= 1,
         edge.color= 1,edge.label.cex = .5,cardinal= F,nDigits= 3,rotation= 4)

# Remove objects of this paragraph
rm(x, y, exponentialfunction, model3, model3fit)

# . . . . . . Model 4: y = x, x € {1, 2, 3, 4} ---------------------------------
# Compute factor loadings needed for this model (y) and plot function
x <- c(1:4)                         # x values
y <- x                              # y values
plot(x, y, type = "o", bty = "n")   # plot x and y values

# Define model
model4 <- 'kon =~ 1 * S1log10mean +
                  1 * S2log10mean +
                  1 * S3log10mean +
                  1 * S4log10mean
           dyn =~ 1 * S1log10mean +
                  2 * S2log10mean +
                  3 * S3log10mean +
                  4 * S4log10mean
           kon ~~ 0 * dyn'

# Estimate model
model4fit <- sem(model4, data = dat, estimator = "MLM", mimic = "Mplus")

# Show summary of model fit
summary(model4fit, fit.measures  = T, standardized  = T, rsquare = T)

# Visualise model
semPaths(model4fit,what= "std",style = "lisrel",layout= "tree", structural = F,
         cut= .60,residScale= 7, sizeMan= 7,sizeLat= 5,intercepts= F,
         optimizeLatRes = T,exoVar= T,nCharNodes= 0,asize= 1.5,esize= 1,
         edge.color= 1,edge.label.cex = .5,cardinal= F,nDigits= 3,rotation= 4)

# Remove objects of this paragraph
rm(x, y, model4, model4fit)

# . . . . . . Model 5: y = 2 ^ x, x € {1, 2, 3, 4} -----------------------------
# Compute factor loadings needed for this model (y) and plot function
x <- c(1:4)                         # x values
y <- 2 ^ x                          # y values
plot(x, y, type = "o", bty = "n")   # plot x and y values

# Define model
model5 <- 'kon =~ 1 * S1log10mean +
                  1 * S2log10mean +
                  1 * S3log10mean +
                  1 * S4log10mean
           dyn =~ 2 * S1log10mean +
                  4 * S2log10mean +
                  8 * S3log10mean +
                  16* S4log10mean
           kon ~~ 0 * dyn'

# Estimate model
model5fit <- sem(model5, data = dat, estimator = "MLM", mimic = "Mplus")

# Show summary of model fit
summary(model5fit, fit.measures  = T, standardized  = T, rsquare = T)

# Visualise model
semPaths(model5fit,what= "std",style = "lisrel",layout= "tree", structural = F,
         cut= .60,residScale= 7, sizeMan= 7,sizeLat= 5,intercepts= F,
         optimizeLatRes = T,exoVar= T,nCharNodes= 0,asize= 1.5,esize= 1,
         edge.color= 1,edge.label.cex = .5,cardinal= F,nDigits= 3,rotation= 4)

# Remove objects of this paragraph
rm(x, y, model5, model5fit)

# . . . . . . Model 6: y = log_{e}x, x € {1, 2, 3, 4} --------------------------
# Compute factor loadings needed for this model (y) and plot function
x <- c(1:4)                         # x values
y <- log(x)                         # y values
plot(x, y, type = "o", bty = "n")   # plot x and y values
y                                   # print y values

# Define model
model6 <- 'kon =~ 1 * S1log10mean +
                  1 * S2log10mean +
                  1 * S3log10mean +
                  1 * S4log10mean
           dyn =~ 0         * S1log10mean +
                  0.6931472 * S2log10mean +
                  1.0986123 * S3log10mean +
                  1.3862944 * S4log10mean
           kon ~~ 0 * dyn'

# Estimate model
model6fit <- sem(model6, data = dat, estimator = "MLM", mimic = "Mplus")

# Show summary of model fit
summary(model6fit, fit.measures  = T, standardized  = T, rsquare = T)

# Visualise model
semPaths(model6fit,what= "std",style = "lisrel",layout= "tree", structural = F,
         cut= .60,residScale= 7, sizeMan= 7,sizeLat= 5,intercepts= F,
         optimizeLatRes = T,exoVar= T,nCharNodes= 0,asize= 1.5,esize= 1,
         edge.color= 1,edge.label.cex = .5,cardinal= F,nDigits= 3,rotation= 4)

# Remove objects of this paragraph
rm(x, y, model6, model6fit)

# . . . . . . Model 7: y = x ^ 2, x € {1, 2, 3, 4} -----------------------------
# Compute factor loadings needed for this model (y) and plot function
x <- c(1:4)                                        # x values
y <- x ^ 2                                         # y values
plot(x, y, type = "o", bty = "n")                  # plot x and y values
y                                                  # print y values

# Define model
model7 <- 'kon =~ 1 * S1log10mean +
                  1 * S2log10mean +
                  1 * S3log10mean +
                  1 * S4log10mean
           dyn =~ 1 * S1log10mean +
                  4 * S2log10mean +
                  9 * S3log10mean +
                  16* S4log10mean
           kon ~~ 0 * dyn'

# Estimate model
model7fit <- sem(model7, data= dat, estimator = "MLM", mimic = "Mplus")

# Show summary of model fit
summary(model7fit, fit.measures  = T, standardized  = T, rsquare = T)

# Visualise model
semPaths(model7fit,what= "std",style = "lisrel",layout= "tree", structural = F,
         cut= .60,residScale= 7, sizeMan= 7,sizeLat= 5,intercepts= F,
         optimizeLatRes = T,exoVar= T,nCharNodes= 0,asize= 1.5,esize= 1,
         edge.color= 1,edge.label.cex = .5,cardinal= F,nDigits= 3,rotation= 4)

# Remove objects of this paragraph
rm(x, y, model7, model7fit)

# . . . . . . Model 8: y = x, x € {0, 1, 2, 3} ---------------------------------
# Compute factor loadings needed for this model (y) and plot function
x <- c(0:3)                                        # x values
y <- x                                             # y values
plot(x, y, type = "o", bty = "n")                  # plot x and y values
y                                                  # print y values

# Define model
model8 <- 'kon =~ 1 * S1log10mean +
                  1 * S2log10mean +
                  1 * S3log10mean +
                  1 * S4log10mean
           dyn =~ 0 * S1log10mean +
                  1 * S2log10mean +
                  2 * S3log10mean +
                  3 * S4log10mean
           kon ~~ 0 * dyn'

# Estimate model
model8fit <- sem(model8, data = dat, estimator = "MLM", mimic = "Mplus")

# Show summary of model fit
summary(model8fit, fit.measures  = T, standardized  = T, rsquare = T)

# Visualise model
semPaths(model8fit,what= "std",style = "lisrel",layout= "tree", structural = F,
         cut= .60,residScale= 7, sizeMan= 7,sizeLat= 5,intercepts= F,
         optimizeLatRes = T,exoVar= T,nCharNodes= 0,asize= 1.5,esize= 1,
         edge.color= 1,edge.label.cex = .5,cardinal= F,nDigits= 3,rotation= 4)

# Remove objects of this paragraph
rm(x, y, model8, model8fit)

# . . . . . . Model 8: Scaled variances ----------------------------------------
# Schweizer (2011), see http://dx.doi.org/10.1080/00273171.2011.625312
# see http://www.psychologie.uni-frankfurt.de/58149832/Instruction.pdf
# Compute factor loadings needed for this model (y) and plot function
(S1scaled <- 0 / sqrt(sum(0 ^ 2, 1 ^ 2, 2 ^ 2, 3 ^ 2)/4))  # scaled loading 1
(S2scaled <- 1 / sqrt(sum(0 ^ 2, 1 ^ 2, 2 ^ 2, 3 ^ 2)/4))  # scaled loading 2
(S3scaled <- 2 / sqrt(sum(0 ^ 2, 1 ^ 2, 2 ^ 2, 3 ^ 2)/4))  # scaled loading 3
(S4scaled <- 3 / sqrt(sum(0 ^ 2, 1 ^ 2, 2 ^ 2, 3 ^ 2)/4))  # scaled loading 4

x <- c(0:3)
y <- c(S1scaled, S2scaled, S3scaled, S4scaled)
plot(x, y, type = "o", bty = "n")

# Define model
model8scaled <- 'kon =~ 1 * S1log10mean +
                        1 * S2log10mean +
                        1 * S3log10mean +
                        1 * S4log10mean
                 dyn =~ 0        * S1log10mean +  # insert scaled loading 1
                        0.534523 * S2log10mean +  # insert scaled loading 2
                        1.069045 * S3log10mean +  # insert scaled loading 3
                        1.603567 * S4log10mean    # insert scaled loading 4
                 kon ~~ 0 * dyn'

# Estimate model
model8scaledfit <- sem(model8scaled, data = dat, estimator = "MLM", mimic = "Mplus")

# Show summary of model fit
summary(model8scaledfit, fit.measures  = T, standardized  = T, rsquare = T)

# Extract scaled variance of constant latent variable
scaledVarKon <- inspect(model8scaledfit, what="est")$psi[1,1]

# Extract scaled variance of dynamic latent variable
scaledVarDyn <- inspect(model8scaledfit, what="est")$psi[2,2]

# Total variance
totalVar <- (scaledVarDyn + scaledVarKon)

# Percentage fo constant variance
(scaledVarKon / totalVar) %>% round(digits = 2)

# Percentage of dynamic variance
(scaledVarDyn / totalVar) %>% round(digits = 2)

# Remove objects of this paragraph
rm(S1scaled, S2scaled, S3scaled, S4scaled,
   x, y,
   model8scaled, model8scaledfit, scaledVarKon, scaledVarDyn, totalVar)

# . . . . . . Model 9: Fixed-links structural equation model (g on S) ----------
# Define model
model9 <- 'kon =~ 1 * S1log10mean +
                  1 * S2log10mean +
                  1 * S3log10mean +
                  1 * S4log10mean
           dyn =~ 0 * S1log10mean +
                  1 * S2log10mean +
                  2 * S3log10mean +
                  3 * S4log10mean
             g =~ zCapacity + zSpeed + zMemory
           kon ~~ 0 * dyn
             g ~ kon + dyn'

# Estimate model
model9fit <- sem(model9, data = dat, estimator = "MLM", mimic = "Mplus")

# Show summary of model fit
summary(model9fit, fit.measures  = T,standardized  = T,rsquare= T)

# Visualise model
semPaths(model9fit,what= "std",style = "lisrel",layout= "tree", structural = F,
         cut= .60,residScale= 7, sizeMan= 7,sizeLat= 5,intercepts= F,
         optimizeLatRes = T,exoVar= T,nCharNodes= 0,asize= 1.5,esize= 1,
         edge.color= 1,edge.label.cex = .5,cardinal= F,nDigits= 3,rotation= 2)

# Remove objects of this paragraph
rm(model9, model9fit)



# . . 3.6 5. Fragestellung -----------------------------------------------------
# . . . . 3.6.1 Analyse auf manifester Ebene -----------------------------------
# . . . . . . Model 10: Multiple Regression (g on Hick) ------------------------
# define and estimate model
model10 <- lm(data = dat,
              zTotal ~ H0meanRT + H1meanRT + H2meanRT + H258meanRT)

# show summary of model 10
summary(model10)

# compute standardised coefficient of model 10
lm.beta(model10) %>% .$standardized.coefficients %>% round(digits = 2)

# . . . . . . Model 11: Multiple Regression (g on Hick and Suppression) --------
# define and estimate model
model11 <- lm(data = dat,
              zTotal ~ H0meanRT + H1meanRT + H2meanRT + H258meanRT +
                       S1log10mean + S2log10mean + S3log10mean + S4log10mean)

# show summary of model 11
summary(model11)

# compute standardised coefficient of model 11
lm.beta(model11) %>% .$standardized.coefficients %>% round(digits = 2)

# . . . . . . Comparison of Model 10 and 11 ------------------------------------
modelCompare(model10, model11)

# . . . . . . Tabelle 12 -------------------------------------------------------
# . . . . . . . . . . Mean, SD, Min, Max, Skew, Kurtosis, Shapiro-Wilk-Test ----
descriptives.function <- function(x) {    # define function
  c(M = round(mean(x), digits = 0),
    SD = round(sd(x), digits = 0),
    Min = round(min(x), digits = 0),
    Max = round(max(x), digits = 0),
    Schiefe  = round(skew(x), digits = 2),
    Kurtosis = round(kurtosi(x), digits = 2))
}

dat %>%
  select(Hinter, Hslope) %>%
  sapply(., descriptives.function) %>%
  t()

dat %>%
  select(Hinter, Hslope) %>%
  sapply(., shapiro.test) %>%
  t()

rm(list = setdiff(ls(), "dat")) # Remove objects of this paragraph

# . . . . . . Descriptives for RMSE --------------------------------------------
dat %>%
  select(Hrmse) %>%
  unlist() %>%
  summary() %>%
  round(digits = 0)

# For more infos about the RMSE cutoff analyses see
# "scripts/tikzDevice/hick_rmse_cutoff.R"

# . . . . . . Correlation between intercept and BIS z-score --------------------
dat %>%
  select(Hinter, zTotal) %>%
  corr.test()

# . . . . . . Correlation between slope and BIS z-score ------------------------
dat %>%
  select(Hslope, zTotal) %>%
  corr.test()

# . . . . . . Tabelle 13 -------------------------------------------------------
dat %>%
  select(Hinter, Hslope, Sasymptote, Sslope, zTotal) %>%
  corr.test()

# . . . . . . Model 12: Multiple Regression (g on Hick) ------------------------
# define and estimate model
model12 <- lm(data = dat,
              zTotal ~ Hinter + Hslope)

# show summary of model 12
summary(model12)

# compute standardised coefficient of model 12
lm.beta(model12) %>% .$standardized.coefficients %>% round(digits = 2)

# . . . . . . Model 13: Multiple Regression (g on Hick and Suppression) --------
# define and estimate model
model13 <- lm(data = dat,
              zTotal ~ Hinter + Hslope + Sasymptote + Sslope)

# show summary of model 13
summary(model13)

# compute standardised coefficient of model 13
lm.beta(model13) %>% .$standardized.coefficients %>% round(digits = 2)

# . . . . . . Comparison of Model 12 and 13 ------------------------------------
modelCompare(model12, model13)

# . . . . 3.6.2 Analyse auf latenter Ebene -------------------------------------
# . . . . . . Model 14: Congeneric hick model ----------------------------------
# Since the variances of the hick conditions are a factor 100 times larger
# than the variances of the spatial suppression task, new variables are built.
# This conversion removes the warning messages produced by the sem() function,
# but does not affect model fit.
dat <- dat %>% mutate(H0meanRT100   = H0meanRT   / 100)
dat <- dat %>% mutate(H1meanRT100   = H1meanRT   / 100)
dat <- dat %>% mutate(H2meanRT100   = H2meanRT   / 100)
dat <- dat %>% mutate(H258meanRT100 = H258meanRT / 100)

# Define model
model14 <- 'H =~ H0meanRT100 + H1meanRT100 + H2meanRT100 + H258meanRT100'

# Estimate model
model14fit <- sem(model14, data = dat, estimator = "MLM", mimic = "Mplus")

# Show summary of model fit
summary(model14fit, fit.measures  = T, standardized  = T, rsquare = T)

# Visualise model
semPaths(model14fit,what= "std",style= "lisrel",layout= "tree", structural = F,
         cut= .60,residScale= 7, sizeMan= 7,sizeLat= 5,intercepts= F,
         optimizeLatRes = T,exoVar= T,nCharNodes= 0,asize= 1.5,esize= 1,
         edge.color= 1,edge.label.cex = .5,cardinal= F,nDigits= 3,rotation= 4)

# Remove objects of this paragraph
rm(list = setdiff(ls(), "dat"))

# . . . . . . Model 15: Structural equation model (g on S and H) ---------------
# Define model
model15 <- 'H =~ H0meanRT100 + H1meanRT100 + H2meanRT100 + H258meanRT100
            S =~ S1log10mean + S2log10mean + S3log10mean + S4log10mean
            g =~ zCapacity + zSpeed + zMemory
            g ~ H + S'

# Estimate model
model15fit <- sem(model15, data = dat, estimator = "MLM", mimic = "Mplus")

# Show summary of model fit
summary(model15fit, fit.measures  = T, standardized  = T, rsquare = T)

# Visualise model
semPaths(model15fit,what= "std",style = "lisrel",layout= "tree", structural = F,
         cut= .60,residScale= 7, sizeMan= 7,sizeLat= 5,intercepts= F,
         optimizeLatRes = T,exoVar= T,nCharNodes= 0,asize= 1.5,esize= 1,
         edge.color= 1,edge.label.cex = .5,cardinal= F,nDigits= 3,rotation= 2)

# Remove objects of this paragraph
rm(list = setdiff(ls(), "dat"))

# . . . . . . Model 16*: y = log_{e}x, x € {1, 2, 3, 4} ------------------------
# Compute factor loadings needed for this model (y) and plot function
x <- c(1:4)                         # x values
(y <- log(x))                       # y values
plot(x, y, type = "o", bty = "n")   # plot x and y values

# Define model
model16 <- 'kon =~ 1 * H0meanRT +
                   1 * H1meanRT +
                   1 * H2meanRT +
                   1 * H258meanRT
            dyn =~ 0         * H0meanRT +
                   0.6931472 * H1meanRT +
                   1.0986123 * H2meanRT +
                   1.3862944 * H258meanRT
            kon ~~ 0         * dyn'

# Estimate model
model16fit <- sem(model16, data = dat, estimator = "MLM", mimic = "Mplus")

# Show summary of model fit
summary(model16fit, fit.measures  = T, standardized  = T, rsquare = T)

# Visualise model
semPaths(model16fit, what= "std", style= "lisrel", layout = "tree", structural= F,
         cut= .60, residScale= 7,sizeMan= 3,sizeLat= 5,intercepts= F,
         optimizeLatRes = T,exoVar= T, nCharNodes= 0, asize= 1.5, esize= 1,
         edge.color= 1, edge.label.cex = .5, cardinal= F,nDigits= 3,rotation= 4)

# Remove objects of this paragraph
rm(list = setdiff(ls(), "dat"))

# . . . . . . Model 17: y = x, x € {1, 2, 3, 4} --------------------------------
# Compute factor loadings needed for this model (y) and plot function
x <- c(1:4)                         # x values
y <- x                              # y values
plot(x, y, type = "o", bty = "n")   # plot x and y values

# Define model
model17 <- 'kon =~ 1 * H0meanRT +
                   1 * H1meanRT +
                   1 * H2meanRT +
                   1 * H258meanRT
            dyn =~ 1 * H0meanRT +
                   2 * H1meanRT +
                   3 * H2meanRT +
                   4 * H258meanRT
            kon ~~ 0 * dyn'

# Estimate model
model17fit <- sem(model17, data = dat, estimator = "MLM", mimic = "Mplus")

# Show summary of model fit
summary(model17fit, fit.measures  = T, standardized  = T, rsquare = T)

# Visualise model
semPaths(model17fit, what= "std", style= "lisrel", layout = "tree", structural= F,
         cut= .60, residScale= 7,sizeMan= 3,sizeLat= 5,intercepts= F,
         optimizeLatRes = T,exoVar= T, nCharNodes= 0, asize= 1.5, esize= 1,
         edge.color= 1, edge.label.cex = .5, cardinal= F,nDigits= 3,rotation= 4)

# Remove objects of this paragraph
rm(list = setdiff(ls(), "dat"))

# . . . . . . Model 18: y = log_{2}x, x € {1, 2, 4, 6} -------------------------
# Compute factor loadings needed for this model (y) and plot function
x <- c(1, 2, 4, 6)                  # x € {1, 2, 4, 6}
y <- log2(x)                        # y values
x <- c(1:4)                         # x values
plot(x, y, type = "o", bty = "n")   # plot x and y values

# Define model
model18 <- 'kon =~ 1 * H0meanRT +
                   1 * H1meanRT +
                   1 * H2meanRT +
                   1 * H258meanRT
            dyn =~ 0    * H0meanRT +
                   1    * H1meanRT +
                   2    * H2meanRT +
                   2.58 * H258meanRT
            kon ~~ 0    * dyn'

# Estimate model
model18fit <- sem(model18, data = dat, estimator = "MLM", mimic = "Mplus")

# Show summary of model fit
summary(model18fit, fit.measures = T, standardized  = T, rsquare = T)

# Visualise model
semPaths(model18fit, what= "std", style= "lisrel", layout = "tree", structural= F,
         cut= .60, residScale= 7,sizeMan= 3,sizeLat= 5,intercepts= F,
         optimizeLatRes = T,exoVar= T, nCharNodes= 0, asize= 1.5, esize= 1,
         edge.color= 1, edge.label.cex = .5, cardinal= F,nDigits= 3,rotation= 4)

# remove objects of this paragraph
rm(list = setdiff(ls(), "dat"))

# . . . . . . Model 19: y = 2 ^ x, x € {1, 2, 3, 4} ----------------------------
# Compute factor loadings needed for this model (y) and plot function
x <- c(1:4)                         # x values
y <- 2 ^ x                          # y values
plot(x, y, type = "o", bty = "n")   # plot x and y values

# Define model
model19 <- 'kon =~ 1 * H0meanRT +
                   1 * H1meanRT +
                   1 * H2meanRT +
                   1 * H258meanRT
            dyn =~ 2 * H0meanRT +
                   4 * H1meanRT +
                   8 * H2meanRT +
                  16 * H258meanRT
            kon ~~ 0 * dyn'

# Estimate model
model19fit <- sem(model19, data = dat, estimator = "MLM", mimic = "Mplus")

# Show summary of model fit
summary(model19fit, fit.measures = T, standardized  = T, rsquare = T)

# Visualise model
semPaths(model19fit, what= "std", style= "lisrel", layout = "tree", structural= F,
         cut= .60, residScale= 7,sizeMan= 3,sizeLat= 5,intercepts= F,
         optimizeLatRes = T,exoVar= T, nCharNodes= 0, asize= 1.5, esize= 1,
         edge.color= 1, edge.label.cex = .5, cardinal= F,nDigits= 3,rotation= 4)

# remove objects of this paragraph
rm(list = setdiff(ls(), "dat"))

# . . . . . . Model 20: y = x ^ 2, x € {1, 2, 3, 4} ----------------------------
# Compute factor loadings needed for this model (y) and plot function
x <- c(1:4)                         # x values
y <- x ^ 2                          # y values
plot(x, y, type = "o", bty = "n")   # plot x and y values

# Define model
model20 <- 'kon =~ 1 * H0meanRT +
                   1 * H1meanRT +
                   1 * H2meanRT +
                   1 * H258meanRT
            dyn =~ 1 * H0meanRT +
                   4 * H1meanRT +
                   9 * H2meanRT +
                  16 * H258meanRT
            kon ~~ 0 * dyn'

# Estimate model
model20fit <- sem(model20, data = dat, estimator = "MLM", mimic = "Mplus")

# Show summary of model fit
summary(model20fit, fit.measures  = T, standardized  = T, rsquare = T)

# Visualise model
semPaths(model20fit, what= "std", style= "lisrel", layout = "tree", structural= F,
         cut= .60, residScale= 7,sizeMan= 3,sizeLat= 5,intercepts= F,
         optimizeLatRes = T,exoVar= T, nCharNodes= 0, asize= 1.5, esize= 1,
         edge.color= 1, edge.label.cex = .5, cardinal= F,nDigits= 3,rotation= 4)

# remove objects of this paragraph
rm(list = setdiff(ls(), "dat"))

# . . . . . . Model 21: y = x, x € {1, 2, 4, 6} --------------------------------
# Compute factor loadings needed for this model (y) and plot function
x <- c(1, 2, 4, 6)                  # x € {1, 2, 4, 6}
y <- x                              # y values
x <- c(1:4)                         # x values
plot(x, y, type = "o", bty = "n")   # plot x and y values

# Define model
model21 <- 'kon =~ 1 * H0meanRT +
                   1 * H1meanRT +
                   1 * H2meanRT +
                   1 * H258meanRT
            dyn =~ 1 * H0meanRT +
                   2 * H1meanRT +
                   4 * H2meanRT +
                   6 * H258meanRT
            kon ~~ 0 * dyn'

# Estimate model
model21fit <- sem(model21, data = dat, estimator = "MLM", mimic = "Mplus")

# Show summary of model fit
summary(model21fit, fit.measures  = T, standardized  = T, rsquare = T)

# Visualise model
semPaths(model21fit, what= "std", style= "lisrel", layout = "tree", structural= F,
         cut= .60, residScale= 7,sizeMan= 3,sizeLat= 5,intercepts= F,
         optimizeLatRes = T,exoVar= T, nCharNodes= 0, asize= 1.5, esize= 1,
         edge.color= 1, edge.label.cex = .5, cardinal= F,nDigits= 3,rotation= 4)

# remove objects of this paragraph
rm(list = setdiff(ls(), "dat"))

# . . . . . . Model 22: y=\dfrac{1}{1 + e^{(-x/.8)}}, x € {-3, -1, 1, 3} -------
# Compute factor loadings needed for this model (y) and plot function
x <- c(-3, -1, 1, 3)                 # x € {-3, -1, 1, 3}
(y <- plogis(x, scale = .8))         # y values
x <- 1:4                             # x values
plot(x, y, type = "o", bty = "n")    # plot x and y values

# Define model
model22 <- 'kon =~ 1 * H0meanRT +
                   1 * H1meanRT +
                   1 * H2meanRT +
                   1 * H258meanRT
            dyn =~ 0.02297737 * H0meanRT +
                   0.22270010 * H1meanRT +
                   0.77729990 * H2meanRT +
                   0.97702260 * H258meanRT
            kon ~~ 0          * dyn'

# Estimate model
model22fit <- sem(model22, data = dat, estimator = "MLM", mimic = "Mplus")

# Show summary of model fit
summary(model22fit, fit.measures  = T, standardized  = T, rsquare = T)

# Visualise model
semPaths(model22fit, what= "std", style= "lisrel", layout = "tree", structural= F,
         cut= .60, residScale= 7,sizeMan= 3,sizeLat= 5,intercepts= F,
         optimizeLatRes = T,exoVar= T, nCharNodes= 0, asize= 1.5, esize= 1,
         edge.color= 1, edge.label.cex = .5, cardinal= F,nDigits= 3,rotation= 4)

# remove objects of this paragraph
rm(list = setdiff(ls(), "dat"))

# . . . . . . Model 22: Scaled variances ---------------------------------------
# Schweizer (2011), see http://dx.doi.org/10.1080/00273171.2011.625312
# see http://www.psychologie.uni-frankfurt.de/58149832/Instruction.pdf
# Compute factor loadings needed for this model (y) and plot function
(H0scaled   <- 0.02297737 / sqrt(sum(0.02297737^2, 0.2227001^2, 0.7772999^2, 0.9770226^2)/4))
(H1scaled   <- 0.2227001  / sqrt(sum(0.02297737^2, 0.2227001^2, 0.7772999^2, 0.9770226^2)/4))
(H2scaled   <- 0.7772999  / sqrt(sum(0.02297737^2, 0.2227001^2, 0.7772999^2, 0.9770226^2)/4))
(H258scaled <- 0.9770226  / sqrt(sum(0.02297737^2, 0.2227001^2, 0.7772999^2, 0.9770226^2)/4))

# Compute factor loadings needed for this model (y) and plot function. This
# is the same increase as in the unscaled version of model 22 (see above), i.e.
# model fit is untouched.
x <- c(1:4)                                      # x values
y <- c(H0scaled, H1scaled, H2scaled, H258scaled) # y values
plot(x, y, type = "o", bty = "n")                # plot x and y values

# Define model
model22scaled <- 'kon =~ 1 * H0meanRT +
                         1 * H1meanRT +
                         1 * H2meanRT +
                         1 * H258meanRT
                  dyn =~ 0.03622988 * H0meanRT +     # inserted scaled loading 1
                         0.3511454  * H1meanRT +     # inserted scaled loading 2
                         1.225618   * H2meanRT +     # inserted scaled loading 3
                         1.540534   * H258meanRT     # inserted scaled loading 4
                  kon ~~ 0          * dyn'

# Estimate model
model22scaledfit <- sem(model22scaled, data = dat, estimator = "MLM", mimic = "Mplus")

# Show summary of model fit
summary(model22scaledfit, fit.measures  = T,  standardized  = T, rsquare = T)

# Extract scaled variance of constant latent variable
scaledVarKon <- inspect(model22scaledfit, what = "est")$psi[1,1]

# Extract scaled variance of dynamic latent variable
scaledVarDyn <- inspect(model22scaledfit, what = "est")$psi[2,2]

# Total variance
totalVar <- (scaledVarDyn + scaledVarKon)

# Percentage fo constant variance
(scaledVarKon / totalVar) %>% round(digits = 2)

# Percentage of dynamic variance
(scaledVarDyn / totalVar) %>% round(digits = 2)

# remove objects of this paragraph
rm(list = setdiff(ls(), "dat"))


# . . . . . . Model 23: Fixed-links structural equation model (g on S + H) -----
# The best fitting fixed-links model of the spatial suppression task (model 8)
# and the best fitting fixed-links model of the hick task (model 22) were used
# to predict g.
# Since the variances of the hick conditions are a factor 100 times larger
# than the variances of the spatial suppression task, new variables are built.
# This conversion removes the warning messages produced by the sem() function,
# but does not affect model fit.
dat <- dat %>% mutate(H0meanRT100   = H0meanRT   / 100)
dat <- dat %>% mutate(H1meanRT100   = H1meanRT   / 100)
dat <- dat %>% mutate(H2meanRT100   = H2meanRT   / 100)
dat <- dat %>% mutate(H258meanRT100 = H258meanRT / 100)

# Define model
model23 <- 'Hkon =~ 1 * H0meanRT100 +
                    1 * H1meanRT100 +
                    1 * H2meanRT100 +
                    1 * H258meanRT100
            Hdyn =~ 0.02297737 * H0meanRT100 +
                    0.22270010 * H1meanRT100 +
                    0.77729990 * H2meanRT100 +
                    0.97702260 * H258meanRT100

            Skon =~ 1 * S1log10mean +
                    1 * S2log10mean +
                    1 * S3log10mean +
                    1 * S4log10mean
            Sdyn =~ 0 * S1log10mean +
                    1 * S2log10mean +
                    2 * S3log10mean +
                    3 * S4log10mean

               g =~ zSpeed + zCapacity + zMemory

               g ~ Hkon + Skon + Hdyn + Sdyn
            Hkon ~~ 0 * Hdyn
            Skon ~~ 0 * Sdyn'

# Estimate model
model23fit <- sem(model23, data = dat, estimator = "MLM", mimic = "Mplus")

# Show summary of model fit
summary(model23fit, fit.measures  = T,standardized  = T,rsquare= T)

# Visualise model
semPaths(model23fit, what= "std", style= "lisrel", layout = "tree", structural=F,
         cut= .60, residScale= 7,sizeMan= 3,sizeLat= 5,intercepts= F,
         optimizeLatRes = T,exoVar= T, nCharNodes= 0, asize= 1.5, esize= 1,
         edge.color= 1, edge.label.cex = .5, cardinal= F,nDigits= 3,rotation= 2)

# remove objects of this paragraph
rm(list = setdiff(ls(), "dat"))



# 4. Diskussion ================================================================
# . . 4.1 Fragestellungen ------------------------------------------------------
# . . . . 4.1.1 Eine Bestätigung des Befunds von Melnick et al. (2013)? --------
# . . . . . . t-Test for mean difference in suppression index ------------------
# Vector of suppression indices from Melnick et al.'s Study 2
MelnickStudy2 <- c(0.362176436,0.702660283,0.344620084,0.290644562,0.149251118,
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

# Independent samples t-test
t.test(MelnickStudy2, dat$si)

# . . . . . . Effect size for mean difference in suppression index -------------
cohen.d(dat$si, MelnickStudy2, paired = FALSE)$estimate %>%
  round(digits = 2) %>%
  abs()

# . . . . . . Levene's test to check for equality of variances in SI -----------
allSuppressionIndices <- c(dat$si, MelnickStudy2)
groupFactor <- c(rep(1, length(dat$si)), rep(2, length(MelnickStudy2))) %>%
  as.factor()
leveneTest(allSuppressionIndices, groupFactor)

# remove objects of this paragraph
rm(list = setdiff(ls(), "dat"))