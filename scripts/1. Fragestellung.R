
# 3.2 1. Fragestellung ---------------------------------------------------------
qplot(res$zTotal, res$SI2)
names(res)

# ... Suppression-Index ----------
library(magrittr)
round(mean(res$SI2), digits = 2)
round(sd(res$SI2), digits = 2)
res$SI2 %>% skew %>% round(digits=2)
round(skew(res$SI2), digits = 2)
round(kurtosi(res$SI2), digits = 2)

# ... correlation between SI and z-score ----------
corr.test(res[c("zTotal", "SI2")], method = "spearman")

summary(res$SI2)

# ... Abbildung 4 ----------
par(pty="s")
plot(res$zTotal, res$SI2,
     #main = "\\textnormal{0-bit}",
     #cex.main=2,
     xlim = c(-2,2),
     ylim = c(-.2,1),
     yaxt = "n",
     xaxt = "n",
     ylab = "Suppression-Index",
     xlab = "z-Wert",
     #cex.lab=2,
     # cex.sub=3,
     col = "black",
     pch = 20,
     cex = 1,
     bty = "n",
     las = 1)

axis(side = 1, at = c(-2,-1,0,1,2), las=1)#, cex.axis = 1.5)
axis(side = 2, at = seq(-.20,1,.20), las=2)#, cex.axis = 1.5)
# abline(h = mean(res$H0meanRT), col = "black", lwd = 1, lty=1)
#title(ylab = "Suppression-Index", line = 3.5)#,cex.lab = 2)

# Semipartialkorrelationen ----------------------------------------------------------------
# install.packages("ppcor")
library(ppcor)

spcor.test(res$S1mean, res$zTotal, res$S4mean, method = "spearman")
spcor.test(res$S4mean, res$zTotal, res$S1mean, method = "spearman")
