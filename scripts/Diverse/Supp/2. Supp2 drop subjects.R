



resultsSupp2 <- subset(resultsSupp2, S1mean < mean(resultsSupp2$S1mean) + 3*sd(resultsSupp2$S1mean))

rm(list = setdiff(ls(), c("resultsHick", "resultsFragebogen", "resultsSupp2", "resultsBIS")))
