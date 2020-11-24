library('fastDummies')
library('pmml')
library('missForest')

data <- read.csv('C://Users/ludepa/OneDrive - SAS/Apresentacoes/Datasets/hmeq.csv', header=T, na.string=c(""), sep = ";")

data <- raw[1:13]

data <- data.frame( CLAGE = as.numeric(data$CLAGE),
                 CLNO = as.numeric(data$CLNO),
                 DEBTINC = as.numeric(data$DEBTINC),
                 DELINQ = as.numeric(data$DELINQ),
                 DEROG = as.numeric(data$DEROG),
                 LOAN = as.numeric(data$LOAN),
                 MORTDUE = as.numeric(data$MORTDUE),
                 NINQ = as.numeric(data$NINQ),
                 VALUE = as.numeric(data$VALUE),
                 YOJ = as.numeric(data$YOJ),
                 JOB = as.factor(data$JOB),
                 REASON = as.factor(data$REASON),
                 BAD = as.factor(data$BAD),
                 stringsAsFactors = FALSE)

imp <- missForest(data, maxiter = 2, ntree = 10)

imp$OOBerror

imp_f <- imp$ximp

data_dummies <- dummy_cols(imp_f)

head(data_dummies)

final_df <- within(data_dummies, rm("JOB", "REASON", "BAD_1", "BAD_0"))
final_df$BAD = data_dummies$BAD

head(final_df)

model <- glm(formula = BAD ~ ., family = binomial(link = "logit"), data = final_df)

s <- summary(model)

print(s)

saveXML(pmml(model, model.name="General_Regression_Model",
             app.name="Rattle/PMML",
             description="Generalized Linear Regression Model"), "R_HMEQ.xml")

write.csv(final_df, './hmeq_dummies.csv', row.names=FALSE)