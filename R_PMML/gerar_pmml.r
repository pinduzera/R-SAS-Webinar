library(fastDummies)
library(pmml)

raw <- read.csv('C://Users/ludepa/OneDrive - SAS/Apresentacoes/Datasets/hmeq.csv', header=T, na.string=c(""), sep = ";")

raw <- raw[1:13]

raw <- data.frame( CLAGE = as.numeric(raw$CLAGE),
                 CLNO = as.numeric(raw$CLNO),
                 DEBTINC = as.numeric(raw$DEBTINC),
                 DELINQ = as.numeric(raw$DELINQ),
                 DEROG = as.numeric(raw$DEROG),
                 LOAN = as.numeric(raw$LOAN),
                 MORTDUE = as.numeric(raw$MORTDUE),
                 NINQ = as.numeric(raw$NINQ),
                 VALUE = as.numeric(raw$VALUE),
                 YOJ = as.numeric(raw$YOJ),
                 JOB = as.character(raw$JOB),
                 REASON = as.character(raw$REASON),
                 BAD = as.character(raw$BAD),
                 stringsAsFactors = FALSE)

raw_dummies <- dummy_cols(raw)

abt <- within(raw_dummies, rm("JOB", "REASON", "BAD_1", "BAD_0"))
abt$BAD = as.factor(abt$BAD)

model <- glm(formula = BAD ~ ., family = binomial(link = "logit"), data = abt)

s <- summary(model)

print(s)

saveXML(pmml(model, model.name="General_Regression_Model",
             app.name="Rattle/PMML",
             description="Generalized Linear Regression Model"), "R_HMEQ.xml")
