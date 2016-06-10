rm(list = setdiff(ls(), c("resultsHick","resultsSupp2","resultsBIS","resultsFragebogen")))

# 1 read csv --------------------------------------------------------------
res <- read.csv("data/processed/res.csv")
res <- as.data.frame(res[,-1])
is.data.frame(res)

# 2 correct variances of indicators ----------------------------------
res$S1mean100 <- res$S1mean / 100
res$S2mean100 <- res$S2mean / 100
res$S3mean100 <- res$S3mean / 100
res$S4mean100 <- res$S4mean / 100


# 3 SEM classic -----------------------------------------------------------
m1 <- 'gSupp2 =~ S1mean + S2mean + S3mean + S4mean
            g =~ zSpeed + zCapacity + zMemory
            g ~ gSupp2'

m1fit <- sem(m1,, data = res, estimator = "MLM", mimic = "Mplus")
m1fit
summary(m1fit, fit.measures  = T,standardized  = T,rsquare= T)

par(mfrow=c(1,1))
semPaths(m1fit,what= "std",style= "lisrel",layout= "tree", structural = F,cut= .60,residScale= 7,
         sizeMan= 7,sizeLat= 5,intercepts= F,optimizeLatRes = T,exoVar= T,nCharNodes= 0,
         asize= 1.5,esize= 1,edge.color= 1,edge.label.cex = .5,cardinal= F,nDigits= 3,rotation= 1)

# 3 SEM fixed links  -----------------------------------------------------------
m2 <- '  C =~ 1*S1mean + 1*S2mean + 1*S3mean + 1*S4mean
       exp =~ 1*S1mean + 4*S2mean + 9*S3mean + 16*S4mean
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




