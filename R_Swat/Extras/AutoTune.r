# Load necessary packages
library('swat')

#options(cas.print.messages = FALSE)

conn <- CAS('hostname.com', 
            port=8777, protocol = "http",
            caslib = 'casuser', 
            authinfo = './.authinfo')

## Carregando Actionsets no CAS
actionsets <- c('sampling', 'decisionTree', 'autotune', 'percentile')

for(i in actionsets){
    loadActionSet(conn, i)
}

# Carregando dados para CAS
castbl <- cas.read.csv(conn, './data/hmeq.csv')

# Particionamento de dados
cas.sampling.srs(conn,
    table = 'hmeq',
    samppct = 30,
    partind = TRUE,
    output = list(casOut = list(name = 'hmeq', replace = T), 
                  copyVars = 'ALL')
)

indata <- 'hmeq'

# Pega infromacao das variaveis
colinfo <- head(cas.table.columnInfo(conn, table = indata)$ColumnInfo, -1)

# Variavel target
target <- colinfo$Column[1]


# Separacao para modelos que lidam com missing
inputs <- colinfo$Column[-1]
nominals <- c(target, subset(colinfo, Type == 'varchar')$Column)

result <- cas.autotune.tuneGradientBoostTree(conn,
           trainOptions = list(
              table   = list("name"= "hmeq", where = '_PartInd_ = 0'),
              inputs  = inputs,
              target = target,
              nominal = nominals,
              casout  = list(name ="tune_boost_model", replace = TRUE)
           ),
           tunerOptions=list(seed = 12345)
      )

print(result$TunerInfo)

  print(result$TunerResults)

  print(result$IterationHistory)

  print(result$IterationHistory)

  print(result$EvaluationHistory)

  print(result$BestConfiguration)

  print(result$TunerSummary)

  print(result$TunerTiming)

  print(result$TunerCasOutputTables)

  print(result$HyperparameterImportance)


### Prevendo um unico modelo
cas.decisionTree.gbtreeScore(conn,
    table = list(name = 'hmeq'),
    modelTable   = list(name = 'tune_boost_model'),
    copyVars     = list(target, '_PartInd_'),
    assessonerow = TRUE,
    casOut       = list(name = 'gb_tune_scored', replace = T)
)

dt_scores <- defCasTable(conn, 'gb_tune_scored')

head(dt_scores)

asses_info <- cas.percentile.assess(conn,
        table    = list(name = paste0('gb_tune_scored'), 
                        where = '_PartInd_ = 1'),
        inputs   = paste0('_GBT_P_           1'),
        response = target,
        event    = '1')

roc <- asses_info$ROCInfo

# Manipulacao do DF
compare <- subset(roc, round(roc$CutOff, 2) == 0.49)
rownames(compare) <- NULL
compare[,c('TP','FP','FN','TN')]

library('ggplot2')
library("plotly")

# Cria curva ROC
options(repr.plot.width=14, repr.plot.height=6)

plt <- ggplot(data = roc[c('FPR', 'Sensitivity')],
    aes(x = FPR, y = Sensitivity)) +
    geom_line(size =1.2) +
    labs(x = 'False Positive Rate', y = 'True Positive Rate') +
    theme_bw()
plt

ggplotly(plt)

plt2 <- ggplot(data = roc[,c('ACC', 'CutOff')],
    aes(y = ACC, x = CutOff, color = ACC)) +
    geom_segment(aes(x=CutOff, xend=dplyr::lead(CutOff), y=ACC, yend=dplyr::lead(ACC))) +
  scale_colour_gradient2(low="red", mid = 'red', high="green")+
    labs(x = 'CutOff', y = 'Accuracy') +
    theme_bw()
plt2

ggplotly(plt2)
#embed_notebook(ggplotly(plt2))

cas.session.endSession(conn)
