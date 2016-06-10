################################################################################
################################################################################
###                                                                             
###   Hick CFA's 
###
###   Philipp Thomas, 18. Januar 2016

library(lavaan)
library(semPlot)

# 0. check 'resultsHick'
ls()
names(resultsHick)
corr.test(resultsHick[24:27])

# 1. Hick congeneric ------------------------------------------------------------
# .... 1.1 model specification -------------------------------------------------
congenericHick <- 'gHick =~ H0meanRT + H1meanRT + H2meanRT + H258meanRT'

# .... 1.2 fit -----------------------------------------------------------------
congenericHickFit <- sem(congenericHick,
                     data      = resultsHick,        #Datensatz mit den in sem.model verwendeten Variablen
                     estimator = "MLM",      #Schätzmethode ist die robuste Variante von ML
                     mimic     = "Mplus")    #man kann den Output von Mplus (default ist EQS)

# .... 1.3 Anzeigen des Modelltests --------------------------------------------
congenericHickFit

# .... 1.4 Zusammenfassung: Modelltest, Fit Indizes etc. -----------------------
summary(congenericHickFit, 
        fit.measures  = T,                #Fit Indizes
        standardized  = T,                #Parameter werden zusätzlich standardisiert
        rsquare       = T)                #Erklärte Varianz der manifesten Variablen wird ausgegeben
parameterEstimates(congenericHickFit,
                   standardized  =T)     #parameter estimates werden standardisiert aufgeführt

# .... 1.5 Plotten -------------------------------------------------------------
par(mfrow=c(1,1))
semPaths(congenericHickFit,
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
h1 <- '    C   =~ 1* H0meanRT + 1* H1meanRT + 1* H2meanRT + 1* H258meanRT
           exp =~ 0 * H0meanRT + 
                  0.6931472 * H1meanRT + 
                  1.0986123  * H2meanRT + 
                  1.3862944  * H258meanRT

           C ~~ 0*exp'

h1fit <- sem(h1,
             data      = resultsHick,        #Datensatz mit den in sem.model verwendeten Variablen
             estimator = "MLM",      #Schätzmethode ist die robuste Variante von ML
             mimic     = "Mplus")    #man kann den Output von Mplus (default ist EQS)

summary(h1fit, 
        fit.measures  = T,                #Fit Indizes
        standardized  = T,                #Parameter werden zusätzlich standardisiert
        rsquare       = T)                #Erklärte Varianz der manifesten Variablen wird ausgegeben

semPaths(h1fit,
         what= "std", style= "lisrel", layout = "tree", structural= F, cut= .60,
         residScale= 7,sizeMan= 3,sizeLat= 5,intercepts= F,optimizeLatRes = T,exoVar= T,
         nCharNodes= 0, asize= 1.5, esize= 1,edge.color= 1, edge.label.cex = .5, cardinal= F,
         nDigits= 3,rotation= 1)

# .... 2.2 model 2 -------------------------------------------------
h2 <- '    C   =~ 1* H0meanRT + 1* H1meanRT + 1* H2meanRT + 1* H258meanRT
           exp =~ 1 * H0meanRT + 
                  2 * H1meanRT + 
                  3  * H2meanRT + 
                  4  * H258meanRT

           C ~~ 0*exp'

h2fit <- sem(h2,
             data      = resultsHick,        #Datensatz mit den in sem.model verwendeten Variablen
             estimator = "MLM",      #Schätzmethode ist die robuste Variante von ML
             mimic     = "Mplus")    #man kann den Output von Mplus (default ist EQS)

summary(h2fit, 
        fit.measures  = T,                #Fit Indizes
        standardized  = T,                #Parameter werden zusätzlich standardisiert
        rsquare       = T)                #Erklärte Varianz der manifesten Variablen wird ausgegeben

semPaths(h2fit,
         what= "std", style= "lisrel", layout = "tree", structural= F, cut= .60,
         residScale= 7,sizeMan= 3,sizeLat= 5,intercepts= F,optimizeLatRes = T,exoVar= T,
         nCharNodes= 0, asize= 1.5, esize= 1,edge.color= 1, edge.label.cex = .5, cardinal= F,
         nDigits= 3,rotation= 1)

# .... 2.3 model 3 -------------------------------------------------
h3 <- '    C   =~ 1* H0meanRT + 1* H1meanRT + 1* H2meanRT + 1* H258meanRT
           exp =~ 0 * H0meanRT + 
                  1 * H1meanRT + 
                  2  * H2meanRT + 
                  2.58  * H258meanRT

           C ~~ 0*exp'

h3fit <- sem(h3,
             data      = resultsHick,        #Datensatz mit den in sem.model verwendeten Variablen
             estimator = "MLM",      #Schätzmethode ist die robuste Variante von ML
             mimic     = "Mplus")    #man kann den Output von Mplus (default ist EQS)

summary(h3fit, 
        fit.measures  = T,                #Fit Indizes
        standardized  = T,                #Parameter werden zusätzlich standardisiert
        rsquare       = T)                #Erklärte Varianz der manifesten Variablen wird ausgegeben

semPaths(h3fit,
         what= "std", style= "lisrel", layout = "tree", structural= F, cut= .60,
         residScale= 7,sizeMan= 3,sizeLat= 5,intercepts= F,optimizeLatRes = T,exoVar= T,
         nCharNodes= 0, asize= 1.5, esize= 1,edge.color= 1, edge.label.cex = .5, cardinal= F,
         nDigits= 3,rotation= 1)

# .... 2.4 model 4 -------------------------------------------------
h4 <- '    C   =~ 1* H0meanRT + 1* H1meanRT + 1* H2meanRT + 1* H258meanRT
           exp =~ 0 * H0meanRT + 
                  1 * H1meanRT + 
                  2  * H2meanRT + 
                  4  * H258meanRT

           C ~~ 0*exp'

h4fit <- sem(h4,
             data      = resultsHick,        #Datensatz mit den in sem.model verwendeten Variablen
             estimator = "MLM",      #Schätzmethode ist die robuste Variante von ML
             mimic     = "Mplus")    #man kann den Output von Mplus (default ist EQS)

summary(h4fit, 
        fit.measures  = T,                #Fit Indizes
        standardized  = T,                #Parameter werden zusätzlich standardisiert
        rsquare       = T)                #Erklärte Varianz der manifesten Variablen wird ausgegeben

semPaths(h4fit,
         what= "std", style= "lisrel", layout = "tree", structural= F, cut= .60,
         residScale= 7,sizeMan= 3,sizeLat= 5,intercepts= F,optimizeLatRes = T,exoVar= T,
         nCharNodes= 0, asize= 1.5, esize= 1,edge.color= 1, edge.label.cex = .5, cardinal= F,
         nDigits= 3,rotation= 1)

# .... 2.5 model 5 -------------------------------------------------
h5 <- '    C   =~ 1* H0meanRT + 1* H1meanRT + 1* H2meanRT + 1* H258meanRT
           exp =~ 1 * H0meanRT + 
                  4 * H1meanRT + 
                  9  * H2meanRT + 
                  16  * H258meanRT

           C ~~ 0*exp'

h5fit <- sem(h5,
             data      = resultsHick,        #Datensatz mit den in sem.model verwendeten Variablen
             estimator = "MLM",      #Schätzmethode ist die robuste Variante von ML
             mimic     = "Mplus")    #man kann den Output von Mplus (default ist EQS)

summary(h5fit, 
        fit.measures  = T,                #Fit Indizes
        standardized  = T,                #Parameter werden zusätzlich standardisiert
        rsquare       = T)                #Erklärte Varianz der manifesten Variablen wird ausgegeben

semPaths(h5fit,
         what= "std", style= "lisrel", layout = "tree", structural= F, cut= .60,
         residScale= 7,sizeMan= 3,sizeLat= 5,intercepts= F,optimizeLatRes = T,exoVar= T,
         nCharNodes= 0, asize= 1.5, esize= 1,edge.color= 1, edge.label.cex = .5, cardinal= F,
         nDigits= 3,rotation= 1)

# .... 2.6 model 6 -------------------------------------------------
h6 <- '    C   =~ 1* H0meanRT + 1* H1meanRT + 1* H2meanRT + 1* H258meanRT
           exp =~ 1 * H0meanRT + 
                  2 * H1meanRT + 
                  4  * H2meanRT + 
                  6  * H258meanRT

           C ~~ 0*exp'

h6fit <- sem(h6,
             data      = resultsHick,        #Datensatz mit den in sem.model verwendeten Variablen
             estimator = "MLM",      #Schätzmethode ist die robuste Variante von ML
             mimic     = "Mplus")    #man kann den Output von Mplus (default ist EQS)

summary(h6fit, 
        fit.measures  = T,                #Fit Indizes
        standardized  = T,                #Parameter werden zusätzlich standardisiert
        rsquare       = T)                #Erklärte Varianz der manifesten Variablen wird ausgegeben

semPaths(h6fit,
         what= "std", style= "lisrel", layout = "tree", structural= F, cut= .60,
         residScale= 7,sizeMan= 3,sizeLat= 5,intercepts= F,optimizeLatRes = T,exoVar= T,
         nCharNodes= 0, asize= 1.5, esize= 1,edge.color= 1, edge.label.cex = .5, cardinal= F,
         nDigits= 3,rotation= 1)

# .... 2.7 model 7 -------------------------------------------------
h7 <- '    C   =~ 1* H0meanRT + 1* H1meanRT + 1* H2meanRT + 1* H258meanRT
           exp =~ 0.02297737 * H0meanRT + 
                  0.2227001 * H1meanRT + 
                  0.7772999  * H2meanRT + 
                  0.9770226  * H258meanRT

           C ~~ 0*exp'

h7fit <- sem(h7,
             data      = resultsHick,        #Datensatz mit den in sem.model verwendeten Variablen
             estimator = "MLM",      #Schätzmethode ist die robuste Variante von ML
             mimic     = "Mplus")    #man kann den Output von Mplus (default ist EQS)

summary(h7fit, 
        fit.measures  = T,                #Fit Indizes
        standardized  = T,                #Parameter werden zusätzlich standardisiert
        rsquare       = T)                #Erklärte Varianz der manifesten Variablen wird ausgegeben

semPaths(h7fit,
         what= "std", style= "lisrel", layout = "tree", structural= F, cut= .60,
         residScale= 7,sizeMan= 3,sizeLat= 5,intercepts= F,optimizeLatRes = T,exoVar= T,
         nCharNodes= 0, asize= 1.5, esize= 1,edge.color= 1, edge.label.cex = .5, cardinal= F,
         nDigits= 3,rotation= 1)




































# rm(list=setdiff(ls(), c("resultsSupp2", "resultsHick","resultsBIS","resultsFragebogen")))
# cat("\014")