################################################################################
################################################################################
###                                                                             
###   Supp2 CFA's 
###
###   Philipp Thomas, 18. Januar 2016

library(lavaan)
library(semPlot)

# 0. check 'resultsHick'
ls()

#corr.test(resultsSupp2[12:15])
corr.test(resultsHick[12:15])

# 1. Hick congeneric ------------------------------------------------------------
# .... 1.1 model specification -------------------------------------------------
congenericSupp2 <- 'gSupp2 =~ S1mean + S2mean + S3mean + S4mean'

# .... 1.2 fit -----------------------------------------------------------------
congenericSupp2Fit <- sem(congenericSupp2,
                     data      = resultsHick,        #Datensatz mit den in sem.model verwendeten Variablen
                     estimator = "MLM",      #Schätzmethode ist die robuste Variante von ML
                     mimic     = "Mplus")    #man kann den Output von Mplus (default ist EQS)

# .... 1.3 Anzeigen des Modelltests --------------------------------------------
congenericSupp2Fit

# .... 1.4 Zusammenfassung: Modelltest, Fit Indizes etc. -----------------------
summary(congenericSupp2Fit, 
        fit.measures  = T,                #Fit Indizes
        standardized  = T,                #Parameter werden zusätzlich standardisiert
        rsquare       = T)                #Erklärte Varianz der manifesten Variablen wird ausgegeben
parameterEstimates(congenericSupp2Fit,
                   standardized  =T)     #parameter estimates werden standardisiert aufgeführt

# .... 1.5 Plotten -------------------------------------------------------------
semPaths(congenericSupp2Fit,
         what           = "std",           #std, or est
         style          = "lisrel",        #lisrel, or mx
         layout         = "tree",          #tree, circle, or spring
         structural     = F,               #or T for structural model only
         cut            = .60,            #cut the scaling of the paths (edges) at this value
         residScale     = 7,               #size of residual paths (edges)
         sizeMan        = 7,               #size of the manifest variables
         sizeLat        = 5,               #size of the latent variables
         intercepts     = F,               #should intercepts be included?
         optimizeLatRes = T,               #angle of the incoming residuals is optimized
         exoVar         = T,               #should variances of exo variables be plotted?
         nCharNodes     = 0,               #number of characters to abbreviate node labels
         asize          = 1.5,             #arrow head size
         esize          = 1,               #arrow wheight
         edge.color     = 1,               #path (edge) color (1-255)
         edge.label.cex = .5,              #path (edge) label size
         cardinal       = F,               #or T for arrows pointing in the middle of a variable
         nDigits        = 3,               #number of digits for coeffiecients
         rotation       = 1)               #rotation of the whole model (1,2,3,4)

# 2. Fixed links modelling -----------------------------------------------------
# .... 2.1 model 1 -------------------------------------------------
s1 <- 'C   =~ 1* S1mean + 1* S2mean + 1* S3mean + 1* S4mean
       exp =~ 0*S1mean +  
              0.6931472*S2mean + 
              1.0986123*S3mean + 
              1.3862944*S4mean

       C ~~ 0*exp'

s1fit <- sem(s1,
             data      = resultsHick,        #Datensatz mit den in sem.model verwendeten Variablen
             estimator = "MLM",      #Schätzmethode ist die robuste Variante von ML
             mimic     = "Mplus")    #man kann den Output von Mplus (default ist EQS)

s1fit

summary(s1fit, 
        fit.measures  = T,                #Fit Indizes
        standardized  = T,                #Parameter werden zusätzlich standardisiert
        rsquare       = T)                #Erklärte Varianz der manifesten Variablen wird ausgegeben

semPaths(s1fit,
         what= "std", style= "lisrel", layout = "tree", structural= F, cut= .60,
         residScale= 7,sizeMan= 3,sizeLat= 5,intercepts= F,optimizeLatRes = T,exoVar= T,
         nCharNodes= 0, asize= 1.5, esize= 1,edge.color= 1, edge.label.cex = .5, cardinal= F,
         nDigits= 3,rotation= 1)

# .... 2.2 model 2 -------------------------------------------------
s2 <- 'C   =~ 1* S1mean + 1* S2mean + 1* S3mean + 1* S4mean
       exp =~ 1*S1mean +  
              2*S2mean + 
              3*S3mean + 
              4*S4mean

       C ~~ 0*exp'

s2fit <- sem(s2,
             data      = resultsHick,        #Datensatz mit den in sem.model verwendeten Variablen
             estimator = "MLM",      #Schätzmethode ist die robuste Variante von ML
             mimic     = "Mplus")    #man kann den Output von Mplus (default ist EQS)

s2fit

summary(s2fit, 
        fit.measures  = T,                #Fit Indizes
        standardized  = T,                #Parameter werden zusätzlich standardisiert
        rsquare       = T)                #Erklärte Varianz der manifesten Variablen wird ausgegeben

semPaths(s2fit,
         what= "std", style= "lisrel", layout = "tree", structural= F, cut= .60,
         residScale= 7,sizeMan= 3,sizeLat= 5,intercepts= F,optimizeLatRes = T,exoVar= T,
         nCharNodes= 0, asize= 1.5, esize= 1,edge.color= 1, edge.label.cex = .5, cardinal= F,
         nDigits= 3,rotation= 1)

# .... 2.3 model 3 -------------------------------------------------
s3 <- 'C   =~ 1* S1mean + 1* S2mean + 1* S3mean + 1* S4mean
       exp =~ 0*S1mean +  
              1*S2mean + 
              2*S3mean + 
              4*S4mean

       C ~~ 0*exp'

s3fit <- sem(s3,
             data      = resultsHick,        #Datensatz mit den in sem.model verwendeten Variablen
             estimator = "MLM",      #Schätzmethode ist die robuste Variante von ML
             mimic     = "Mplus")    #man kann den Output von Mplus (default ist EQS)

s3fit

summary(s3fit, 
        fit.measures  = T,                #Fit Indizes
        standardized  = T,                #Parameter werden zusätzlich standardisiert
        rsquare       = T)                #Erklärte Varianz der manifesten Variablen wird ausgegeben

semPaths(s3fit,
         what= "std", style= "lisrel", layout = "tree", structural= F, cut= .60,
         residScale= 7,sizeMan= 3,sizeLat= 5,intercepts= F,optimizeLatRes = T,exoVar= T,
         nCharNodes= 0, asize= 1.5, esize= 1,edge.color= 1, edge.label.cex = .5, cardinal= F,
         nDigits= 3,rotation= 1)

# .... 2.4 model 4 -------------------------------------------------
s4 <- 'C   =~ 1* S1mean + 1* S2mean + 1* S3mean + 1* S4mean
       exp =~ 1*S1mean +  
              2*S2mean + 
              4*S3mean + 
              8*S4mean

       C ~~ 0*exp'

s4fit <- sem(s4,
             data      = resultsHick,        #Datensatz mit den in sem.model verwendeten Variablen
             estimator = "MLM",      #Schätzmethode ist die robuste Variante von ML
             mimic     = "Mplus")    #man kann den Output von Mplus (default ist EQS)

s4fit

summary(s4fit, 
        fit.measures  = T,                #Fit Indizes
        standardized  = T,                #Parameter werden zusätzlich standardisiert
        rsquare       = T)                #Erklärte Varianz der manifesten Variablen wird ausgegeben

semPaths(s4fit,
         what= "std", style= "lisrel", layout = "tree", structural= F, cut= .60,
         residScale= 7,sizeMan= 3,sizeLat= 5,intercepts= F,optimizeLatRes = T,exoVar= T,
         nCharNodes= 0, asize= 1.5, esize= 1,edge.color= 1, edge.label.cex = .5, cardinal= F,
         nDigits= 3,rotation= 1)

# .... 2.5 model 5 -------------------------------------------------
s5 <- 'C   =~ 1* S1mean + 1* S2mean + 1* S3mean + 1* S4mean
       exp =~ 1*S1mean +  
              4*S2mean + 
              9*S3mean + 
              16*S4mean

       C ~~ 0*exp'

s5fit <- sem(s5,
             data      = resultsHick,        #Datensatz mit den in sem.model verwendeten Variablen
             estimator = "MLM",      #Schätzmethode ist die robuste Variante von ML
             mimic     = "Mplus")    #man kann den Output von Mplus (default ist EQS)

s5fit

summary(s5fit, 
        fit.measures  = T,                #Fit Indizes
        standardized  = T,                #Parameter werden zusätzlich standardisiert
        rsquare       = T)                #Erklärte Varianz der manifesten Variablen wird ausgegeben

semPaths(s5fit,
         what= "std", style= "lisrel", layout = "tree", structural= F, cut= .60,
         residScale= 7,sizeMan= 3,sizeLat= 5,intercepts= F,optimizeLatRes = T,exoVar= T,
         nCharNodes= 0, asize= 1.5, esize= 1,edge.color= 1, edge.label.cex = .5, cardinal= F,
         nDigits= 3,rotation= 1)



























































  
#rm(list=setdiff(ls(), c("resultsSupp2", "resultsHick","resultsBIS","resultsFragebogen")))
# cat("\014")