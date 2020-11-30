library('missForest')

## Transformando espacios blancos en NA
dm_inputdf[dm_inputdf==""] <- NA

## Aplicando Imputacion
imp <- missForest(dm_inputdf, maxiter = 2, ntree = 30) 

## Sacando los datos imputados
dm_scoreddf <- imp$ximp
dm_scoreddf$BAD <- as.numeric(dm_scoreddf$BAD) - 1

## Output BAD
print("/nInput")
head(dm_inputdf$BAD)

## Data frame output
print("/nScored")
head(dm_scoreddf$BAD)

## Data frame print
print("/nDataFrame")
head(dm_scoreddf)
