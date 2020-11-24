
# Load necessary packages
library('swat')
library('ggplot2')
library('reshape2')
options(cas.print.messages = FALSE)

conn <- CAS('servername.com', 
            port=8777, 
            caslib = 'casuser',
            username = 'username',
            password = 'password',
            protocol = "http")

actionsets <- c('sampling', 'fedsql', 'decisionTree', 'neuralNet', 'percentile')
for(i in actionsets){
    loadActionSet(conn, i)
}

# checking available tables
cas.table.tableInfo(conn, caslib = 'public')

# checking available caslibs
cas.table.caslibInfo(conn)

# Carregando dados para CAS
data_dir <- 'data'
castbl <- cas.read.csv(conn, paste(data_dir, 'hmeq.csv', sep = '/'))

class(castbl)

head(castbl)
# Table summary 
table <- cas.simple.summary(castbl)

class(castbl)
# Trazer dados para maquina local
df <- to.casDataFrame(castbl, obs = nrow(castbl))

# Formatacao de dados
d <- melt(df[sapply(df, is.numeric)], id.vars=NULL)
ggplot(d, aes(x = value)) +
    facet_wrap(~variable,scales = 'free_x') +
    geom_histogram(fill = 'blue', bins = 25)

# Ver dados faltantes de todas variaveis
tbl <- cas.simple.distinct(castbl)$Distinct[,c('Column', 'NMiss')]
tbl


# Cogiendo datos missing
cas.nmiss(castbl)

# Visualizacion de missing
tbl$PctMiss <- tbl$NMiss/nrow(castbl)
ggplot(tbl, aes(Column, PctMiss)) +
    geom_col(fill = 'blue') +
    ggtitle('Pct Missing Values') +
    theme(plot.title = element_text(hjust = 0.5))

# Umputacions de datos missing
cas.dataPreprocess.impute(castbl,
    methodContinuous = 'MEDIAN',
    methodNominal = 'MODE',
    inputs = colnames(castbl)[-1],
    copyAllVars = TRUE,
    casOut = list(name = 'hmeq', 
                replace = TRUE)
)

# Particionamento de datos
cas.sampling.srs(conn,
    table = 'hmeq',
    samppct = 30,
    partind = TRUE,
    output = list(casOut = list(name = 'hmeq', replace = T),
                  copyVars = 'ALL')
)


hmeq1 <- defCasTable(conn, 'hmeq')
head(hmeq1)


indata <- 'hmeq'

# Cogiendo informacion de las variaveis
colinfo <- head(cas.table.columnInfo(conn, table = indata)$ColumnInfo, -1)

# Variable target
target <- colinfo$Column[1]

# Separacion para modelos que saben utilizar missing
inputs <- colinfo$Column[-1]
nominals <- c(target, subset(colinfo, Type == 'varchar')$Column)

# Separacao para modelos que no saben utilizar missing
imp.inputs <- grep('IMP_', inputs, value = T)
imp.nominals <- c(target, grep('IMP_', nominals, value = T))

# Entrenando modelos
cas.decisionTree.dtreeTrain(conn,
    table = list(name = indata, where = '_PartInd_ = 0'),
    target = target,
    inputs = inputs,
    nominals = nominals,
    varImp = TRUE,
    casOut = list(name = 'dt_model', replace = TRUE)
)

cas.decisionTree.forestTrain(conn,
    table = list(name = indata, where = '_PartInd_ = 0'),
    target = target,
    inputs = inputs,
    nominals = nominals,
    casOut = list(name = 'rf_model', replace = TRUE)
)

cas.decisionTree.gbtreeTrain(conn,
    table = list(name = indata, where = '_PartInd_ = 0'),
    target = target,
    inputs = inputs,
    nominals = nominals,
    casOut = list(name = 'gbt_model', replace = TRUE)
)

cas.neuralNet.annTrain(conn,
    table = list(name = indata, where = '_PartInd_ = 0'),
    target = target,
    inputs = imp.inputs,
    hidden = 7,
    nominals = imp.nominals,
    casOut = list(name = 'nn_model', replace = TRUE)
)

models <- c('dt','rf','gbt','nn')
scores <- c(cas.decisionTree.dtreeScore, cas.decisionTree.forestScore, 
            cas.decisionTree.gbtreeScore, cas.neuralNet.annScore)
names(scores) <- models

# Funcion para automatizar el processo de predicion en novos datos
score.params <- function(model){return(list(
    object       = defCasTable(conn, indata),
    modelTable   = list(name = paste0(model, '_model')),
    copyVars     = list(target, '_PartInd_'),
    assessonerow = TRUE,
    casOut       = list(name = paste0(model, '_scored'), replace = T)
))}
lapply(models, function(x) {do.call(scores[[x]], score.params(x))})

# Carga actionset para scoring
loadActionSet(conn, 'percentile')

# Funcion para comparacion de modelos
assess.model <- function(model){
    cas.percentile.assess(conn,
        table    = list(name = paste0(model,'_scored'), 
                        where = '_PartInd_ = 1'),
        inputs   = paste0('_', model, '_P_           1'),
        response = target,
        event    = '1')
}

model.names <- c('Decision Tree', 'Random Forest', 
                 'Gradient Boosting', 'Neural Network')
roc.df <- data.frame()
for (i in 1:length(models)){
    tmp <- (assess.model(models[i]))$ROCInfo
    tmp$Model <- model.names[i] 
    roc.df <- rbind(roc.df, tmp)
}

# Manipulacion del data.frame
compare <- subset(roc.df, round(roc.df$CutOff, 2) == 0.5)
rownames(compare) <- NULL
compare[,c('Model','TP','FP','FN','TN')]

# Crea data.frame para comparar missclassification
compare$Misclassification <- 1 - compare$ACC
miss <- compare[order(compare$Misclassification), c('Model','Misclassification')]
rownames(miss) <- NULL
miss

# Añadir nueva coluna para ser usada como label en la curva Roc
roc.df$Models <- paste(roc.df$Model, round(roc.df$C, 3), sep = ' - ')

# Crea curva ROC
ggplot(data = roc.df[c('FPR', 'Sensitivity', 'Models')],
    aes(x = as.numeric(FPR), y = as.numeric(Sensitivity), colour = Models)) +
    geom_line() +
    labs(x = 'False Positive Rate', y = 'True Positive Rate')

library('magrittr')
p <- plotly::ggplotly(plot) %>% plotly::layout(plot, hovermode = "x")

p

# Fim sessao
cas.session.endSession(conn)
