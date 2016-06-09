rm(list = setdiff(ls(), c("resultsHick","resultsSupp2")))


# Diese Versuche funktionieren nicht --------------------------------------
setdiff((resultsSupp2$subject), (resultsHick$subject))
length(which(resultsHick$subject %in% resultsSupp2$subject))
'%ni%' <- Negate('%in%')
which(resultsSupp2$subject %ni% resultsHick$subject)
length(which(resultsHick$subject %ni% resultsSupp2$subject))


# Das schon ---------------------------------------------------------------
tset <- resultsHick$subject
pop <- resultsSupp2$subject
pop[which(!(pop %in% tset))]


# merge -------------------------------------------------------------------
dat1<- merge(merge(merge(resultsSupp2, resultsHick, by = "subject"),
                         resultsBIS, by = "subject"),
                         resultsFragebogen, by = "subject")


write.table(dat1, "data/processed/hicksupp.csv", row.names=FALSE, col.names=TRUE, sep = ",") 











