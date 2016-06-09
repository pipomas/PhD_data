


resultsBIS <- subset(resultsBIS, !subject == 138) # Jean-Michel, z-Wert sehr tief im figuralen Inhalt (?)

rm(list = setdiff(ls(), c("resultsHick", "resultsFragebogen", "resultsSupp2","resultsBIS")))

