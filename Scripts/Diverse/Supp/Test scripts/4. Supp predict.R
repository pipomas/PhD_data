dat <- data.frame(matrix(0,nrow(resultsHick),0))
str(dat)

dat$Cmodel2 <- predict(s2fit)[,1]
dat$Emodel2 <- predict(s2fit)[,2]

dat$Cmodel4 <- predict(s4fit)[,1]
dat$Emodel4 <- predict(s4fit)[,2]

dat$Cmodel5 <- predict(s5fit)[,1]
dat$Emodel5 <- predict(s5fit)[,2]
