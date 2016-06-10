res <- read.csv("data/processed/res.csv")
res <- as.data.frame(res[,-1])


names(res)
# Scatterplot
plot(res$SI2, res$zTotal,
     xlim=c(-.2,1),
     ylim=c(-2,2),
     xlab="SI",
     ylab="zTotal",
     main="",
     bty= "n",
     xaxt= "n",
     yaxt= "n",
     pch = 20)

axis(side = 1, at = c(-.2,0,.5,1), labels=T)
axis(side = 2, at = seq(-2,2,1), las=1)
abline(lm(res$zTotal ~ res$SI2), col = 6)

# Correlationskoeffizient
corr.test(res[c("SI2", "zTotal")], method = "spearman")

plot(res$SI2, res$zTotal,
     xlim=c(-.2,1),
     ylim=c(-2,2),
     xlab="SI",
     ylab="zTotal",
     main="",
     bty= "n",
     xaxt= "n",
     yaxt= "n",
     pch = 20)

axis(side = 1, at = c(-.2,0,.5,1), labels=T)
axis(side = 2, at = seq(-2,2,1), las=1)
abline(lm(res$zTotal ~ res$SI2), col = 6)





library(corrplot)
x <- cor(res[,c(181:183,37:42)])

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

# Regressionsmethode

mymodel <- lm(scale(zTotal) ~ scale(Sinter) * scale(Sslope), data = res)

#3.1 BIS Operationen und Inhalte
library(corrplot)
x <- cor(res[,c(181:183,37:42)])
corr.test(res[,c(181:183,37:42)])
corrplot(x,
         order = "original",
         type = "upper",
         diag = TRUE,
         addCoef.col = "black",
         insig = "blank",
         pch.col = "purple",
         tl.srt=30,
         tl.col="black",
         tl.cex = .6,
         cl.cex = .6
         )

#3.1 Hick
library(corrplot)
x <- cor(res[,c(181:183,140:143,160:161)])
corr.test(res[,c(181:183,140:143,160:161)])
corrplot(x,
         order = "original",
         type = "upper",
         diag = TRUE,
         addCoef.col = "black",
         insig = "blank",
         pch.col = "purple",
         tl.srt=30,
         tl.col="black",
         tl.cex = .6,
         cl.cex = .6
         )

