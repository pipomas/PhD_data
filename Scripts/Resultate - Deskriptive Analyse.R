
# 3.3.1 Spatial-Suppression-Aufgabe --------------------------------------------
# .... Tabelle 1 --------
sapply(subset(res, select = c(S1mean, S2mean, S3mean, S4mean)), min)
round(sapply(subset(res, select = c(S1mean, S2mean, S3mean, S4mean)), mean), digits = 0)
round(sapply(subset(res, select = c(S1mean, S2mean, S3mean, S4mean)), sd), digits = 0)
sapply(subset(res, select = c(S1mean, S2mean, S3mean, S4mean)), max)
round(sapply(subset(res, select = c(S1mean, S2mean, S3mean, S4mean)), skew), digits = 2)
round(sapply(subset(res, select = c(S1mean, S2mean, S3mean, S4mean)), kurtosi), digits = 2)
#sapply(subset(res, select = c(S1mean, S2mean, S3mean, S4mean)), shapiro.test)

# .... Abbildung 2 ------


# ... Tabelle 2 --------
corr.test(res[,c("S1mean", "S2mean", "S3mean", "S4mean")], method = "spearman")

# 3.1.2 Hick task --------------------------------------------------------------
# ... Abbildung 3 ------


# ... Tabelle 3 --------
sapply(subset(res, select = c(H0meanRT, H1meanRT, H2meanRT, H258meanRT)), min)
sapply(subset(res, select = c(H0meanRT, H1meanRT, H2meanRT, H258meanRT)), max)
round(sapply(subset(res, select = c(H0meanRT, H1meanRT, H2meanRT, H258meanRT)), mean), digits = 0)
round(sapply(subset(res, select = c(H0meanRT, H1meanRT, H2meanRT, H258meanRT)), sd), digits = 0)
round(sapply(subset(res, select = c(H0meanRT, H1meanRT, H2meanRT, H258meanRT)), skew), digits = 2)
round(sapply(subset(res, select = c(H0meanRT, H1meanRT, H2meanRT, H258meanRT)), kurtosi), digits = 2)
#sapply(subset(res, select = c(H0meanRT, H1meanRT, H2meanRT, H258meanRT)), shapiro.test)

# ... Tabelle 4 --------
names(res)
corr.test(res[,c("H0meanRT", "H1meanRT", "H2meanRT", "H258meanRT")], method = "spearman")


# 3.1.3 BIS-Test ---------------------------------------------------------------
# ... Tabelle 5 --------
dat <- merge(res, resultsBIS, by = "subject")
names(dat)

sapply(subset(dat, select = c(OGraw, ZNraw, ANraw, XGraw, WAraw, ZPraw, TMraw,
                              BDraw, SCraw, STraw, CHraw, TGraw, RZraw, WMraw,
                              KWraw, ZZraw, OEraw, WEraw)), min)

round(sapply(subset(dat, select = c(OGraw, ZNraw, ANraw, XGraw, WAraw, ZPraw, TMraw,
                              BDraw, SCraw, STraw, CHraw, TGraw, RZraw, WMraw,
                              KWraw, ZZraw, OEraw, WEraw)), mean), digits = 2)

round(sapply(subset(dat, select = c(OGraw, ZNraw, ANraw, XGraw, WAraw, ZPraw, TMraw,
                              BDraw, SCraw, STraw, CHraw, TGraw, RZraw, WMraw,
                              KWraw, ZZraw, OEraw, WEraw)), sd), digits = 2)


sapply(subset(dat, select = c(OGraw, ZNraw, ANraw, XGraw, WAraw, ZPraw, TMraw,
                              BDraw, SCraw, STraw, CHraw, TGraw, RZraw, WMraw,
                              KWraw, ZZraw, OEraw, WEraw)), max)

round(sapply(subset(dat, select = c(OGraw, ZNraw, ANraw, XGraw, WAraw, ZPraw, TMraw,
                              BDraw, SCraw, STraw, CHraw, TGraw, RZraw, WMraw,
                              KWraw, ZZraw, OEraw, WEraw)), skew), digits = 2)

round(sapply(subset(dat, select = c(OGraw, ZNraw, ANraw, XGraw, WAraw, ZPraw, TMraw,
                              BDraw, SCraw, STraw, CHraw, TGraw, RZraw, WMraw,
                              KWraw, ZZraw, OEraw, WEraw)), kurtosi), digits = 2)

sapply(subset(dat, select = c(OGraw, ZNraw, ANraw, XGraw, WAraw, ZPraw, TMraw,
                              BDraw, SCraw, STraw, CHraw, TGraw, RZraw, WMraw,
                              KWraw, ZZraw, OEraw, WEraw)), shapiro.test)

rm(dat)

