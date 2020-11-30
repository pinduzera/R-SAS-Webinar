library("randomForest")

# RandomForest
dm_model <- randomForest(dm_model_formula, ntree=100, mtry=5, data=dm_traindf, importance=TRUE)

# Score
pred <- predict(dm_model, dm_inputdf, type="prob")
dm_scoreddf <- data.frame(pred)
colnames(dm_scoreddf) <- c("P_BAD0", "P_BAD1")

# Print/plot model output
png("rpt_forestMsePlot1.png")
plot(dm_model, main='randomForest MSE Plot')
legend("topright", colnames(dm_model$err.rate),col=1:3,cex=0.8,fill=1:3)
dev.off()

write.csv(importance(dm_model), 
          file="rpt_forestIMP.csv", 
          row.names=TRUE)