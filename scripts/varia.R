rm(list = setdiff(ls(), c("resultsHick","resultsSupp2","resultsBIS","resultsFragebogen")))

# 1 read csv --------------------------------------------------------------
res <- read.csv("data/processed/res.csv")
res <- as.data.frame(res[,-1])
is.data.frame(res)

# 2 correct variances of indicators ----------------------------------
res$H0meanRT100 <- res$H0meanRT / 100
res$H1meanRT100 <- res$H1meanRT / 100
res$H2meanRT100 <- res$H2meanRT / 100
res$H258meanRT100 <- res$H258meanRT / 100

res$S1mean100 <- res$S1mean / 100
res$S2mean100 <- res$S2mean / 100
res$S3mean100 <- res$S3mean / 100
res$S4mean100 <- res$S4mean / 100


# 1 Traditionelle Analyse ------------------------------------------------------
# .... 1.1 Hick und Spatial Suppression ----------------
m1 <- 'gHick =~ H0meanRT100 + H1meanRT100 + H2meanRT100 + H258meanRT100
      gSupp2 =~ S1mean100 + S2mean100 + S3mean100 + S4mean100

      S3mean100 ~~ a1*S3mean100 ; a1 > 0'

m1fit <- sem(m1,, data = res, estimator = "MLM", mimic = "Mplus")
m1fit
summary(m1fit, fit.measures  = T,standardized  = T,rsquare= T)
semPaths(m1fit,what= "std",style= "lisrel",layout= "tree", structural = F,cut= .60,residScale= 7,
         sizeMan= 7,sizeLat= 5,intercepts= F,optimizeLatRes = T,exoVar= T,nCharNodes= 0,
         asize= 1.5,esize= 1,edge.color= 1,edge.label.cex = .5,cardinal= F,nDigits= 3,rotation= 1)

# .... 1.2 Hick und g ----------------------------------
m2 <- 'gHick =~ H0meanRT100 + H1meanRT100 + H2meanRT100 + H258meanRT100
           g =~ zSpeed + zCapacity + zMemory
           g ~ gHick'

m2fit <- sem(m2, data = res, estimator = "MLM", mimic = "Mplus")
m2fit
summary(m2fit, fit.measures  = T,standardized  = T,rsquare= T)

semPaths(m2fit,what= "std",style= "lisrel",layout= "tree", structural = F,cut= .60,residScale= 7,
         sizeMan= 7,sizeLat= 5,intercepts= F,optimizeLatRes = T,exoVar= T,nCharNodes= 0,
         asize= 1.5,esize= 1,edge.color= 1,edge.label.cex = .5,cardinal= F,nDigits= 3,rotation= 2)

# .... 1.3 Spatial Suppression und g -------------------
m3 <- 'gSupp2 =~ S1mean100 + S2mean100 + S3mean100 + S4mean100
            g =~ zSpeed + zCapacity + zMemory
            g ~ gSupp2'

m3fit <- sem(m3, data = res, estimator = "MLM", mimic = "Mplus")
m3fit
summary(m3fit, fit.measures  = T,standardized  = T,rsquare= T)

semPaths(m3fit,what= "std",style= "lisrel",layout= "tree", structural = F,cut= .60,residScale= 7,
         sizeMan= 7,sizeLat= 5,intercepts= F,optimizeLatRes = T,exoVar= T,nCharNodes= 0,
         asize= 1.5,esize= 1,edge.color= 1,edge.label.cex = .5,cardinal= F,nDigits= 3,rotation= 2)

# .... 1.4 Hick, Spatial Suppression und g -------------
# .... 1.4.1 Einzelprädiktoren
m4 <- 'gHick =~ H0meanRT100 + H1meanRT100 + H2meanRT100 + H258meanRT100
      gSupp2 =~ S1mean100 + S2mean100 + S3mean100 + S4mean100

           g =~ zSpeed + zCapacity + zMemory
           g ~ gHick + gSupp2'

m4fit <- sem(m4, data = res, estimator = "MLM", mimic = "Mplus")
m4fit
summary(m4fit, fit.measures  = T,standardized  = T,rsquare= T)

semPaths(m4fit,what= "std",style= "lisrel",layout= "tree", structural = F,cut= .60,residScale= 7,
         sizeMan= 7,sizeLat= 5,intercepts= F,optimizeLatRes = T,exoVar= T,nCharNodes= 0,
         asize= 1.5,esize= 1,edge.color= 1,edge.label.cex = .5,cardinal= F,nDigits= 3,rotation= 2)

# .... 1.4.2 Mediation über Hick -------------------------
m9 <- 'gHick =~ H0meanRT100 + H1meanRT100 + H2meanRT100 + H258meanRT100
      gSupp2 =~ S1mean100 + S2mean100 + S3mean100 + S4mean100

           g =~ zSpeed + zCapacity + zMemory

           # direct effect
           g ~ c*gSupp2

           # mediator
           gHick ~ a*gSupp2
           g ~ b*gHick

           # indirect effect (a*b)
           ind := a*b

           dir := c

           # total effect
           total := c + (a*b)

'

m9fit <- sem(m9, data = res, estimator = "MLM", mimic = "Mplus")
m9fit
summary(m9fit, fit.measures  = T,standardized  = T,rsquare= T)

semPaths(m9fit,what= "std",style= "lisrel",layout= "tree", structural = F,cut= .60,residScale= 7,
         sizeMan= 7,sizeLat= 5,intercepts= F,optimizeLatRes = T,exoVar= T,nCharNodes= 0,
         asize= 1.5,esize= 1,edge.color= 1,edge.label.cex = .5,cardinal= F,nDigits= 3,rotation= 2)



# 2 fixed-links Analyse --------------------------------------------------------
# .... 2.1 Hick und Spatial Suppression ----------------
m5 <- '  CH =~ 1*H0meanRT100 + 1*H1meanRT100 + 1*H2meanRT100 + 1*H258meanRT100
       expH =~ 0.02297737*H0meanRT100 + 0.2227001*H1meanRT100 + 0.7772999*H2meanRT100 + 0.9770226*H258meanRT100

         CS =~ 1*S1mean100 + 1*S2mean100 + 1*S3mean100 + 1*S4mean100
       expS =~ 1*S1mean100 + 4*S2mean100 + 9*S3mean100 + 16*S4mean100

         CH ~~ 0* expH
         CS ~~ 0* expS'

m5fit <- sem(m5, data = res, estimator = "MLM", mimic = "Mplus")
m5fit
summary(m5fit, fit.measures  = T,standardized  = T,rsquare= T)

semPaths(m5fit,what= "std",style= "lisrel",layout= "tree", structural = F,cut= .60,residScale= 7,
         sizeMan= 7,sizeLat= 5,intercepts= F,optimizeLatRes = T,exoVar= T,nCharNodes= 0,
         asize= 1.5,esize= 1,edge.color= 1,edge.label.cex = .5,cardinal= F,nDigits= 3,rotation= 1,
         curve = 2)

# .... 2.2 Hick und g ----------------------------------
m6 <- '  C =~ 1*H0meanRT100 + 1*H1meanRT100 + 1*H2meanRT100 + 1*H258meanRT100
       exp =~ 0.02297737*H0meanRT100 + 0.2227001*H1meanRT100 + 0.7772999*H2meanRT100 + 0.9770226*H258meanRT100
         g =~ zSpeed + zCapacity + zMemory
         g ~ C + exp
         C ~~ 0*exp'

m6fit <- sem(m6, data = res, estimator = "MLM", mimic = "Mplus")
m6fit
summary(m6fit, fit.measures  = T,standardized  = T,rsquare= T)

semPaths(m6fit,what= "std",style= "lisrel",layout= "tree", structural = F,cut= .60,residScale= 7,
         sizeMan= 7,sizeLat= 5,intercepts= F,optimizeLatRes = T,exoVar= T,nCharNodes= 0,
         asize= 1.5,esize= 1,edge.color= 1,edge.label.cex = .5,cardinal= F,nDigits= 3,rotation= 2)

# .... 2.3 Spatial Suppression und g -------------------
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

# .... 2.4 Hick, Spatial Suppression und g -------------
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

# .... 2.4 Hick, Spatial Suppression und g -------------
# .... 2.4.2 Hick als Mediator, expS als UV  --------------------------
m10 <- '  CH =~ 1*H0meanRT100 + 1*H1meanRT100 + 1*H2meanRT100 + 1*H258meanRT100
       expH =~ 0.02297737*H0meanRT100 + 0.2227001*H1meanRT100 + 0.7772999*H2meanRT100 + 0.9770226*H258meanRT100

         CS =~ 1*S1mean100 + 1*S2mean100 + 1*S3mean100 + 1*S4mean100
       expS =~ 1*S1mean100 + 4*S2mean100 + 9*S3mean100 + 16*S4mean100

         CH ~~ 0* expH
         CS ~~ 0* expS

          g =~ zSpeed + zCapacity + zMemory

          #direct effect
          g ~ c*expS

          #indirect effect (a1*b1)
          expH ~ a1*expS
          g    ~ b1*expH

          #indirect effect (a2*b2)
          CH ~ a2*expS
           g ~ b2*CH

          #indirect effect (a*b)
           ind1 := a1*b1
           ind2 := a2*b2

           #direct effect c
           direct := c

           # total effect
           total := (a1*b1) + (a2*b2) + c
          '

m10fit <- sem(m10, data = res, estimator = "MLM", mimic = "Mplus")
m10fit
summary(m10fit, fit.measures  = T,standardized  = T,rsquare= T)

semPaths(m10fit,what= "std",style= "lisrel",layout= "spring", structural = T,cut= .0001,residScale= 7,
         sizeMan= 7,sizeLat= 5,intercepts= F,optimizeLatRes = T,exoVar= T,nCharNodes= 0,
         asize= 1.5,esize= 1,edge.color= 1,edge.label.cex = .5,cardinal= F,nDigits= 3,rotation= 1,
         curve = 3)

# .... 2.4.2 Hick als Mediator, CS als UV  --------------------------
m11 <- '  CH =~ 1*H0meanRT100 + 1*H1meanRT100 + 1*H2meanRT100 + 1*H258meanRT100
       expH =~ 0.02297737*H0meanRT100 + 0.2227001*H1meanRT100 + 0.7772999*H2meanRT100 + 0.9770226*H258meanRT100

         CS =~ 1*S1mean100 + 1*S2mean100 + 1*S3mean100 + 1*S4mean100
       expS =~ 1*S1mean100 + 4*S2mean100 + 9*S3mean100 + 16*S4mean100

         CH ~~ 0* expH
         CS ~~ 0* expS

          g =~ zSpeed + zCapacity + zMemory

          #direct effect
          g ~ c*CS

          #indirect effect (a1*b1)
          expH ~ a1*CS
          g    ~ b1*expH

          #indirect effect (a2*b2)
          CH ~ a2*CS
           g ~ b2*CH

          #indirect effect (a*b)
           ind1 := a1*b1
           ind2 := a2*b2

           #direct effect c
           direct := c

           # total effect
           total := (a1*b1) + (a2*b2) + c
          '

m11fit <- sem(m11, data = res, estimator = "MLM", mimic = "Mplus")
m11fit
summary(m11fit, fit.measures  = T,standardized  = T,rsquare= T)

semPaths(m11fit,what= "std",style= "lisrel",layout= "spring", structural = T,cut= .0001,residScale= 7,
         sizeMan= 7,sizeLat= 5,intercepts= F,optimizeLatRes = T,exoVar= T,nCharNodes= 0,
         asize= 1.5,esize= 1,edge.color= 1,edge.label.cex = .5,cardinal= F,nDigits= 3,rotation= 1,
         curve = 3)

