library(ggplot2)
names(res)


# 3.3 2. Fragestellung ---------------------------------------------------------
summary(res$Sinter)
sd(res$Sinter)
round(kurtosi(res$Sinter), digits=2)
round(skew(res$Sinter), digits = 2)

summary(res$Sslope)
sd(res$Sslope)
round(kurtosi(res$Sslope), digits=2)
round(skew(res$Sslope), digits = 2)

qplot(res$Sslope)

round(summary(res$zTotal), digits = 2)
sd(res$zTotal)
round(skew(res$zTotal), digits = 2)
round(kurtosi(res$zTotal), digits = 2)

qplot(res$zTotal)
qplot(res$Sinter)
qplot(res$Sslope)
corr.test(res[,c("Sinter","Sslope","zTotal")], method = "spearman")



# Weiteres ----------------------------------------------------------------
# install.packages("ppcor")
library(ppcor)

spcor.test(res$S1mean, res$zTotal, res$S4mean, method = "spearman")
spcor.test(res$S4mean, res$zTotal, res$S1mean, method = "spearman")
