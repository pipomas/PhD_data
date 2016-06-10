rm(list = setdiff(ls(), c("resultsHick","resultsSupp2","resultsBIS","resultsFragebogen")))

# 1 read csv --------------------------------------------------------------
res <- read.csv("data/processed/res.csv")
res <- as.data.frame(res[,-1])
is.data.frame(res)

# 2 correct variances of hick indicators ----------------------------------
res$H0meanRT100 <- res$H0meanRT / 100
res$H1meanRT100 <- res$H1meanRT / 100
res$H2meanRT100 <- res$H2meanRT / 100
res$H258meanRT100 <- res$H258meanRT / 100


# 3 SEM classic -----------------------------------------------------------
m1 <- 'gHick =~ H0meanRT + H1meanRT + H2meanRT + H258meanRT
           g =~ zSpeed + zCapacity + zMemory
           g ~ gHick'

m1fit <- sem(m1,, data = res, estimator = "MLM", mimic = "Mplus")
m1fit
summary(m1fit, fit.measures  = T,standardized  = T,rsquare= T)

par(mfrow=c(1,1))
semPaths(m1fit,what= "std",style= "lisrel",layout= "tree", structural = F,cut= .60,residScale= 7,
         sizeMan= 7,sizeLat= 5,intercepts= F,optimizeLatRes = T,exoVar= T,nCharNodes= 0,
         asize= 1.5,esize= 1,edge.color= 1,edge.label.cex = .5,cardinal= F,nDigits= 3,rotation= 2)

# 3 SEM fixed links  -----------------------------------------------------------
m2 <- '  C =~ 1*H0meanRT100 + 1*H1meanRT100 + 1*H2meanRT100 + 1*H258meanRT100
       exp =~ 0.02297737*H0meanRT100 + 0.2227001*H1meanRT100 + 0.7772999*H2meanRT100 + 0.9770226*H258meanRT100
         g =~ zSpeed + zCapacity + zMemory
         g ~ C + exp
         C ~~ 0*exp'

m2fit <- sem(m2,, data = res, estimator = "MLM", mimic = "Mplus")
m2fit
summary(m2fit, fit.measures  = T,standardized  = T,rsquare= T)

par(mfrow=c(1,1))
semPaths(m2fit,what= "std",style= "lisrel",layout= "tree", structural = F,cut= .60,residScale= 7,
         sizeMan= 7,sizeLat= 5,intercepts= F,optimizeLatRes = T,exoVar= T,nCharNodes= 0,
         asize= 1.5,esize= 1,edge.color= 1,edge.label.cex = .5,cardinal= F,nDigits= 3,rotation= 2)




