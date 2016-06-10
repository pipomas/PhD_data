rm(list = setdiff(ls(), c("resultsFragebogen","resultsHick", "resultsSupp2","resultsBIS")))


# Remove subjects with age < 18 -------------------------------------------
summary(as.numeric(resultsFragebogen$agey))
plot(as.numeric(resultsFragebogen$agey))
resultsFragebogen <- subset(resultsFragebogen, !agey < 18)
summary(as.numeric(resultsFragebogen$agey))
