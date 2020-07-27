library('missForest')

dm_traindf[dm_traindf==""] <- NA

imp <- missForest(dm_traindf, maxiter = 2, ntree = 30) 

dm_scoreddf <- imp$ximp
dm_scoreddf$BAD <- as.numeric(dm_scoreddf$BAD) - 1

print("/nInput")
head(dm_traindf$BAD)

print("/nScored")
head(dm_scoreddf$BAD)

print("/nDataFrame")
head(dm_scoreddf)
