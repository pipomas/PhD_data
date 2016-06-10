
library(lavaan)
library(semPlot)

# 0. check 'resultsHick'
ls()

#corr.test(resultsSupp2[12:15])
corr.test(res[,c("S1mean","S2mean","S3mean","S4mean")])


# 0. adjust variances of indicators ----------------------------------
res$H0meanRT100 <- res$H0meanRT / 100
res$H1meanRT100 <- res$H1meanRT / 100
res$H2meanRT100 <- res$H2meanRT / 100
res$H258meanRT100 <- res$H258meanRT / 100

res$S1mean100 <- res$S1mean / 100
res$S2mean100 <- res$S2mean / 100
res$S3mean100 <- res$S3mean / 100
res$S4mean100 <- res$S4mean / 100






# 3. Model 3: LINEAR increase (1,2,3,4) ----------------------------------------
s2 <- 'C   =~ 1* S1mean + 1* S2mean + 1* S3mean + 1* S4mean
       exp =~ 1*S1mean +  
              2*S2mean + 
              3*S3mean + 
              4*S4mean

       C ~~ 0*exp'

s2fit <- sem(s2,
             data      = res,        #Datensatz mit den in sem.model verwendeten Variablen
             estimator = "MLM",      #Schätzmethode ist die robuste Variante von ML
             mimic     = "Mplus")    #man kann den Output von Mplus (default ist EQS)

s2fit

summary(s2fit, 
        fit.measures  = T,                #Fit Indizes
        standardized  = T,                #Parameter werden zusätzlich standardisiert
        rsquare       = T)                #Erklärte Varianz der manifesten Variablen wird ausgegeben

semPaths(s2fit,what= "std", style= "lisrel", layout = "tree", structural= F, cut= .60,
         residScale= 7,sizeMan= 3,sizeLat= 5,intercepts= F,optimizeLatRes = T,exoVar= T,
         nCharNodes= 0, asize= 1.5, esize= 1,edge.color= 1, edge.label.cex = .5, cardinal= F,
         nDigits= 3,rotation= 1)

# 4. Model 4: EXPONENTIAL regression model -------------------------------------
s2 <- 'C   =~ 1* S1mean + 1* S2mean + 1* S3mean + 1* S4mean
       exp =~ 1.107383*S1mean +  
              1.226298*S2mean + 
              1.357982*S3mean + 
              1.503807*S4mean

       C ~~ 0*exp'

s2fit <- sem(s2,
             data      = res,        #Datensatz mit den in sem.model verwendeten Variablen
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


# 5. Model 5: LINEAR (0,1,2,3) -------------------------------------------------
s3 <- 'C   =~ 1* S1mean + 1* S2mean + 1* S3mean + 1* S4mean
       exp =~ 0*S1mean +  
              1*S2mean + 
              2*S3mean + 
              4*S4mean

       C ~~ 0*exp'

s3fit <- sem(s3,
             data      = res,        #Datensatz mit den in sem.model verwendeten Variablen
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



# 6. Model 6: LOGARITHMIC model ------------------------------------------------
s1 <- 'C   =~ 1* S1mean + 1* S2mean + 1* S3mean + 1* S4mean
       exp =~ 0*S1mean +  
              0.6931472*S2mean + 
              1.0986123*S3mean + 
              1.3862944*S4mean

       C ~~ 0*exp'

s1fit <- sem(s1,
             data      = res,        #Datensatz mit den in sem.model verwendeten Variablen
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


# 7. Model 7: EXPONENTIAL function (2^x) ---------------------------------------------
s4 <- 'C   =~ 1* S1mean + 1* S2mean + 1* S3mean + 1* S4mean
       exp =~ 2*S1mean +  
              4*S2mean + 
              8*S3mean + 
              16*S4mean

       C ~~ 0*exp'

s4fit <- sem(s4,
             data      = res,        #Datensatz mit den in sem.model verwendeten Variablen
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

# 8. Model 8: QUADRATIC function (x^2) ---------------------------------------------
s5 <- 'C   =~ 1* S1mean + 1* S2mean + 1* S3mean + 1* S4mean
       exp =~ 1*S1mean +  
              4*S2mean + 
              9*S3mean + 
              16*S4mean

       C ~~ 0*exp'

s5fit <- sem(s5,
             data      = res,        #Datensatz mit den in sem.model verwendeten Variablen
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

# 9. Scaling of Variances of model 8 (Schweizer, 2011) ------------------------------------------------------------------

(S1scaled <- 1 / sqrt(sum(1^2, 4^2, 9^2, 16^2)/4))  # compute scaled loading of indicator 1
(S2scaled <- 4 / sqrt(sum(1^2, 4^2, 9^2, 16^2)/4))  # compute scaled loading of indicator 2
(S3scaled <- 9 / sqrt(sum(1^2, 4^2, 9^2, 16^2)/4))  # compute scaled loading of indicator 3
(S4scaled <- 16/ sqrt(sum(1^2, 4^2, 9^2, 16^2)/4))  # compute scaled loading of indicator 4

s5 <- 'C   =~ 1* S1mean + 1* S2mean + 1* S3mean + 1* S4mean
       exp =~ 0.1062988*S1mean +  # insert scaled loading 1
              0.4251952*S2mean +  # insert scaled loading 2
              0.9566892*S3mean +  # insert scaled loading 3
              1.700781*S4mean     # insert scaled loading 4

       C ~~ 0*exp'

s5fit <- sem(s5,
             data      = res,        #Datensatz mit den in sem.model verwendeten Variablen
             estimator = "MLM",      #Schätzmethode ist die robuste Variante von ML
             mimic     = "Mplus")    #man kann den Output von Mplus (default ist EQS)

s5fit

summary(s5fit, 
        fit.measures  = T,                #Fit Indizes
        standardized  = T,                #Parameter werden zusätzlich standardisiert
        rsquare       = T)                #Erklärte Varianz der manifesten Variablen wird ausgegeben

# Extract scaled variance of constant latent variable
scaledVarC <- inspect(s5fit,what="est")$psi[1,1]

# Extract scaled variance of experimental latent variable
scaledVarexp <- inspect(s5fit,what="est")$psi[2,2]

# ratio of C and exp Variance
scaledVarexp / scaledVarC


# 10. Modell 9: FLM and g factor ------------------------------------------
m7 <- '  C =~ 1*S1mean100 + 1*S2mean100 + 1*S3mean100 + 1*S4mean100
       exp =~ 1*S1mean100 + 4*S2mean100 + 9*S3mean100 + 16*S4mean100
         g =~ zSpeed + zCapacity + zMemory
         g ~ C + exp
         C ~~ 0*exp'

m7fit <- sem(m7, data = res, estimator = "MLM", mimic = "Mplus")
m2fit
summary(m7fit, fit.measures  = T,standardized  = T,rsquare= T)

semPaths(m7fit,what= "std",style= "lisrel",layout= "tree", structural = F,cut= .60,residScale= 7,
         sizeMan= 7,sizeLat= 5,intercepts= F,optimizeLatRes = T,exoVar= T,nCharNodes= 0,
         asize= 1.5,esize= 1,edge.color= 1,edge.label.cex = .5,cardinal= F,nDigits= 3,rotation= 2)



# test --------------------------------------------------------------------























































  
#rm(list=setdiff(ls(), c("resultsSupp2", "resultsHick","resultsBIS","resultsFragebogen")))
# cat("\014")