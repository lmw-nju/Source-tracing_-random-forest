rm(list=ls())
install.packages("randomForest")
library(randomForest)
setwd("E:/R/JLrandomforest")
river<-read.table('cec.txt',sep="\t",header = TRUE)
river$Class<-as.factor(river$Class)

n<-length(names(river))
rate=1
for(i in 1:(n-1)){
  set.seed(233)
  river_rf<-randomForest(Class~.,data=river,mtry=i,ntree=500,importance=TRUE)
  rate[i]<-mean(river_rf$err.rate)
  print(river_rf)    
}
rate
plot(rate)

set.seed(233)
num_iterations <- 10
importance_list <- list()
for (i in 1:num_iterations) {
  model <- randomForest(Class~.,data=river,mtry=34,ntree=500,importance=TRUE,proximity=TRUE)
  importance <- importance(model)
  importance_list[[i]] <- importance
}
importance_mean <- Reduce(function(x, y) {x + y}, importance_list) / num_iterations
print(importance_mean)
write.csv(importance_mean,"cec.csv")
print(model)
plot(model)