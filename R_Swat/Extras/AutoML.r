library('swat')
library('dplyr')
library('readr')
library('stringr')

conn <- CAS('hostname.com', port=8777, 
            caslib = 'casuser',
            username = 'username',
            password = 'password', protocol = "http")


data <- read.csv('./data/hmeq.csv', stringsAsFactors = FALSE)

ctbl <- as.casTable(conn,
                    data, 
                    casOut = list(name="hmeq", 
                               replace=FALSE,
                               promote = TRUE))

tail(ctbl, n = 5)

actionsets <- c('dataSciencePilot', 'explainModel', 'aStore')
for(i in actionsets){
  loadActionSet(conn, i)
}

colinfo <- head(cas.table.columnInfo(conn, table = 'hmeq')$ColumnInfo, -1)

# Variavel target
target <- colinfo$Column[1]
inputs <- colinfo$Column[-1]
nominals <- c(target, subset(colinfo, Type == 'varchar')$Column)

aml <- cas.dataSciencePilot.dsAutoMl(
  conn,
  table = list(name ='hmeq'),
  target = "BAD",
  explorationPolicy  = list(nominal = list(nominals = nominals)),
  event = '1',
  transformationPolicy  = list(missing = FALSE, cardinality = TRUE,
                               entropy = TRUE, iqv = TRUE,
                               skewness = TRUE, kurtosis = TRUE, 
                               Outlier = TRUE),
  modelTypes  = list("DECISIONTREE", "FOREST", "GLM", "GRADBOOST", "LOGISTIC", "NEURALNET"),
  objective = "AUC",
  sampleSize  = 20,
  topKPipelines = 20,
  kFolds  = 5,
  transformationOut = list(name= "TRANSFORMATION_OUT", replace = TRUE),
  featureOut  = list(name= "FEATURE_OUT", replace = TRUE),
  pipelineOut = list(name= "PIPELINE_OUT", replace = TRUE),
  saveState = list(name= "ASTORE_OUT", replace = TRUE)  
)

best_models <- aml$keyedList[names(aml$keyedList) %>% str_subset('BestConfi')]
best_models

best_names <- objects(best_models)
best_rocs <- str_replace(best_names,pattern = 'BestConfiguration', replacement = 'ROCInfo')

roc.df <- data.frame()

for (i in 1:length(best_rocs)){
    tmp <- get(best_rocs[i], aml$keyedList)
    tmp$Model <- best_names[i]
    roc.df <- rbind(roc.df, tmp)
}

head(roc.df)

library('ggplot2')

# Cria curva ROC
options(repr.plot.width=14, repr.plot.height=6)

ggplot(data = roc.df[c('FPR', 'Sensitivity', 'Model')],
    aes(x = as.numeric(FPR), y = as.numeric(Sensitivity), colour = Model)) +
    geom_line(size =1.2) +
    labs(x = 'False Positive Rate', y = 'True Positive Rate')

df_scored <- cas.astore.score(
        conn, 
        out = list(name = "hmeq_scored", replace = TRUE), 
        rstore = "ASTORE_OUT", 
        table = "hmeq"
       )


df_scored$OutputCasTables

df_scored

## best features
scored <- defCasTable(conn, 'hmeq_scored')
head(scored)






cas.terminate(conn)
