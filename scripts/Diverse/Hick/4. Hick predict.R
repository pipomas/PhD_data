

dat <- data.frame(matrix(0,nrow(resultsHick),0))
str(dat)

predict(flHickFit)

# dat$Cmodel1  <- predict(h1fit)[,1]
# dat$Emodel1  <- predict(h1fit)[,2]


dat$Cmodel2  <- predict(h2fit)[,1]
dat$Emodel2  <- predict(h2fit)[,2]

dat$Cmodel3  <- predict(h3fit)[,1]
dat$Emodel3  <- predict(h3fit)[,2]

dat$Cmodel4  <- predict(h4fit)[,1]
dat$Emodel4  <- predict(h4fit)[,2]

dat$Cmodel5  <- predict(h5fit)[,1]
dat$Emodel5  <- predict(h5fit)[,2]

dat$Cmodel6  <- predict(h6fit)[,1]
dat$Emodel6  <- predict(h6fit)[,2]

dat$Cmodel7  <- predict(h7fit)[,1]
dat$Emodel7  <- predict(h7fit)[,2]

str(dat)

corr.test(dat[c("Cmodel7", "Cmodel5", "Emodel7", "Emodel5")])

corr.test((cbind(dat[c("Cmodel5", "Emodel5")], resultsHick$S1mean, resultsHick$S2mean, resultsHick$S3mean, resultsHick$S4mean, resultsHick$zTotal)))

x <- cor((cbind(dat[c("Cmodel5", "Emodel5")], resultsHick$S1mean, resultsHick$S2mean, resultsHick$S3mean, resultsHick$S4mean, resultsHick$zTotal)))

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