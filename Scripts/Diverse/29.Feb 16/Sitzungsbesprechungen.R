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


# 1 Extract g facotr scores ------------------------------------------------------
m1 <- 'g =~ zFigural + zSpeed + zCapacity'

m1fit <- sem(m1,, data = res, estimator = "MLM", mimic = "Mplus")
m1fit
summary(m1fit, fit.measures  = T,standardized  = T,rsquare= T)
semPaths(m1fit,what= "std",style= "lisrel",layout= "tree", structural = F,cut= .60,residScale= 7,
         sizeMan= 7,sizeLat= 5,intercepts= F,optimizeLatRes = T,exoVar= T,nCharNodes= 0,
         asize= 1.5,esize= 1,edge.color= 1,edge.label.cex = .5,cardinal= F,nDigits= 3,rotation= 1)


res$gfs <- predict(m1fit)
names(res)


# correlations ------------------------------------------------------------

x <- cor(res[c(140:143,173:176,181,192,43)])

corr.test(res[c(140:143,173:176,181,192,43)])
library(corrplot)
corrplot(x,
         order = "original",
         type = "upper",
         diag = TRUE,
         addCoef.col = "white",
         insig = "blank",
         tl.srt=30,
         tl.col="black",
         tl.cex = .6,
         cl.cex = .6
         )


