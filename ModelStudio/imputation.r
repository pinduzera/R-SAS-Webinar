library('missForest')

## Transformando espaco em branco em NA
dm_traindf[dm_traindf==""] <- NA

## Aplicando Inputacao
imp <- missForest(dm_traindf, maxiter = 2, ntree = 30) 

## retirando dados imputados
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
