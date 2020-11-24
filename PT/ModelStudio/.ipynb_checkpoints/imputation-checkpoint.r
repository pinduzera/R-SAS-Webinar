library('missForest')

## Transformando espacios blancos en NA
dm_inputdf[dm_inputdf==""] <- NA

## Aplicando imputacion
imp <- missForest(dm_inputdf, maxiter = 2, ntree = 30) 

## sacando los datos imputados
dm_scoreddf <- imp$ximp
dm_scoreddf$BAD <- as.numeric(dm_scoreddf$BAD) - 1

## Output BAD
print("/nInput")
head(dm_traindf$BAD)

## Data frame output
print("/nScored")
head(dm_scoreddf$BAD)

## Data frame print
print("/nDataFrame")
head(dm_scoreddf)
