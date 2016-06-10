
# Dataframe to csv save ---------------------------------------------------

names(resultsBIS)


# 18 subtests -------------------------------------------------------------

names(resultsBIS[9:27])
write.table(resultsBIS[9:27], "data/base/BIS/18subtests.csv", row.names=FALSE, col.names=FALSE, sep = ",") 


# 9 parcels -------------------------------------------------------------

names(resultsBIS[28:36])
write.table(resultsBIS[28:36], "data/base/BIS/9parcels.csv", row.names=FALSE, col.names=FALSE, sep = ",") 


# 7 operations and contents -----------------------------------------------

names(resultsBIS[37:42])
write.table(resultsBIS[37:42], "data/base/BIS/7operationscontents.csv", row.names=FALSE, col.names=FALSE, sep = ",") 
