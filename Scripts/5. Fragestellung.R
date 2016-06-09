

# 0. check 'resultsHick'
ls()

#corr.test(resultsSupp2[12:15])
corr.test(res[,c("S1mean","S2mean","S3mean","S4mean", "H0meanRT", "H1meanRT", "H2meanRT", "H258meanRT", "zTotal")], method = "spearman")$p

corr.test(res[,c("S1mean","S2mean","S3mean","S4mean", "H0meanRT", "H1meanRT", "H2meanRT", "H258meanRT")], method = "spearman")$p


round(summary(res$Hinter), digits=0)
round(sd(res$Hinter), digits=0)
round(skew(res$Hinter), digits = 2)
round(kurtosi(res$Hinter), digits = 2)

round(summary(res$Hslope), digits=0)
round(sd(res$Hslope), digits=0)
round(skew(res$Hslope), digits = 2)
round(kurtosi(res$Hslope), digits = 2)

corr.test(res[,c("Sinter","Sslope","Hinter","Hslope","zTotal")], method = "spearman")


# Hick FL modelle ---------------------------------------------------------

# 2. Fixed links modelling -----------------------------------------------------
# .... 2.1 model 10 -------------------------------------------------
h1 <- '    C   =~ 1* H0meanRT + 1* H1meanRT + 1* H2meanRT + 1* H258meanRT
           exp =~ 0 * H0meanRT + 
                  0.6931472 * H1meanRT + 
                  1.0986123  * H2meanRT + 
                  1.3862944  * H258meanRT

           C ~~ 0*exp'

h1fit <- sem(h1,
             data      = res,        #Datensatz mit den in sem.model verwendeten Variablen
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

# .... 2.2 model 11 -------------------------------------------------
h2 <- '    C   =~ 1* H0meanRT + 1* H1meanRT + 1* H2meanRT + 1* H258meanRT
           exp =~ 1 * H0meanRT + 
                  2 * H1meanRT + 
                  3  * H2meanRT + 
                  4  * H258meanRT

           C ~~ 0*exp'

h2fit <- sem(h2,
             data      = res,        #Datensatz mit den in sem.model verwendeten Variablen
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
             data      = res,        #Datensatz mit den in sem.model verwendeten Variablen
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

# .... 2.4 model 13 -------------------------------------------------
h4 <- '    C   =~ 1* H0meanRT + 1* H1meanRT + 1* H2meanRT + 1* H258meanRT
           exp =~ 0 * H0meanRT + 
                  1 * H1meanRT + 
                  2  * H2meanRT + 
                  4  * H258meanRT

           C ~~ 0*exp'

h4fit <- sem(h4,
             data      = res,        #Datensatz mit den in sem.model verwendeten Variablen
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
             data      = res,        #Datensatz mit den in sem.model verwendeten Variablen
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
             data      = res,        #Datensatz mit den in sem.model verwendeten Variablen
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

# .... 2.7 model 15 -------------------------------------------------
h7 <- '    C   =~ 1* H0meanRT + 1* H1meanRT + 1* H2meanRT + 1* H258meanRT
           exp =~ 0.02297737 * H0meanRT + 
                  0.2227001 * H1meanRT + 
                  0.7772999  * H2meanRT + 
                  0.9770226  * H258meanRT

           C ~~ 0*exp'

h7fit <- sem(h7,
             data      = res,        #Datensatz mit den in sem.model verwendeten Variablen
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


# 9. Scaling of Variances of model 15 (Schweizer, 2011) ------------------------------------------------------------------

(S1scaled <- 0.02297737 / sqrt(sum(0.02297737^2, 0.2227001^2, 0.7772999^2, 0.9770226^2)/4))  # compute scaled loading of indicator 1
(S2scaled <- 0.2227001  / sqrt(sum(0.02297737^2, 0.2227001^2, 0.7772999^2, 0.9770226^2)/4))  # compute scaled loading of indicator 2
(S3scaled <- 0.7772999  / sqrt(sum(0.02297737^2, 0.2227001^2, 0.7772999^2, 0.9770226^2)/4))  # compute scaled loading of indicator 3
(S4scaled <- 0.9770226  / sqrt(sum(0.02297737^2, 0.2227001^2, 0.7772999^2, 0.9770226^2)/4))  # compute scaled loading of indicator 4

s5 <- 'C   =~ 1* H0meanRT + 1* H1meanRT + 1* H2meanRT + 1* H258meanRT
       exp =~ 0.03622988 * H0meanRT +     # insert scaled loading 1
              0.3511454  * H1meanRT +     # insert scaled loading 2
              1.225618   * H2meanRT +     # insert scaled loading 3
              1.540534   * H258meanRT     # insert scaled loading 4

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


# SEM ---------------------------------------------------------------------

# .... 2.4.1 Einzelprädiktoren -------------------------
m8 <- '  CH =~ 1*H0meanRT100 + 1*H1meanRT100 + 1*H2meanRT100 + 1*H258meanRT100
       expH =~ 0.02297737*H0meanRT100 + 0.2227001*H1meanRT100 + 0.7772999*H2meanRT100 + 0.9770226*H258meanRT100

         CS =~ 1*S1mean100 + 1*S2mean100 + 1*S3mean100 + 1*S4mean100
       expS =~ 1*S1mean100 + 4*S2mean100 + 9*S3mean100 + 16*S4mean100

         CH ~~ 0* expH
         CS ~~ 0* expS

          g =~ zSpeed + zCapacity + zMemory
          g ~ CH + CS + expH + expS'

m8fit <- sem(m8, data = res, estimator = "MLM", mimic = "Mplus")
m8fit
summary(m8fit, fit.measures  = T,standardized  = T,rsquare= T)

semPaths(m8fit,what= "std",style= "lisrel",layout= "tree", structural = T,cut= .60,residScale= 7,
         sizeMan= 7,sizeLat= 5,intercepts= F,optimizeLatRes = T,exoVar= T,nCharNodes= 0,
         asize= 1.5,esize= 1,edge.color= 1,edge.label.cex = .5,cardinal= F,nDigits= 3,rotation= 2,
         curve = 3)
