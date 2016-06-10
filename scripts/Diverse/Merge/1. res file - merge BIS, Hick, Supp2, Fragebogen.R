# 1 retain relevant objects ---------------------------------------------------
rm(list = setdiff(ls(), c("resultsHick","resultsSupp2","resultsBIS","resultsFragebogen")))
# rm(list = setdiff(ls(), c("res")))

# 2 Check which subject has Supp2 data but no Hick data -----------------------
tset <- resultsHick$subject
pop <- resultsSupp2$subject
pop[which(!(pop %in% tset))]

# 2 Check which subject has Supp2 data but no BIS data -----------------------
tset <- resultsBIS$subject
pop <- resultsSupp2$subject
pop[which(!(pop %in% tset))]

# 3 Merge objects -------------------------------------------------------------
# this works --------------------------------------------------------------
res <- merge(merge(merge(resultsBIS, resultsFragebogen, by ="subject"),
                         resultsHick, by = "subject"),
                         resultsSupp2, by = "subject")

# 4 Rearrange data frame --------------------------------------------------
# test <- res[,c(1:2,163,45,164,40:43,140:162,165:184,3:39,44,46:39)]
# names(test)

# 5 write csv -------------------------------------------------------------
write.csv(res, "data/processed/res.csv") 
#write.table(res, "data/processed/res.csv", col.names=TRUE, sep = ",") 