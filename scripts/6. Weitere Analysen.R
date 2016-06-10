plot(c(.02, .22, .77, .97), type = "o", bty = "n")



lines(c(.1,.2, .4))


plot(res$H0meanRT, res$zTotal)

corr.test(subset(res, H0meanRT < 300, select = c("H0meanRT","zTotal")))

corr.test(subset(res, H0meanRT < 300, select = c("H258meanRT","zTotal")))

corr.test(subset(res, select = c("SI2", "Sslope")), method = "spearman")
