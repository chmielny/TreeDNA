library(RWeka)
library("ROCR")

trainData = read.csv("donory_trening.csv")
testData =  read.csv("donory_test.csv")
trainData = within(trainData, rm(X))
testData = within(testData, rm(X))

bestAUC = 0;
bestAB = data.frame(A = integer(), B = integer())
for (A in 5:10){
  for (B in 5:10)	{
    cutTrainData = trainData[,c( (51-A):(51+B), 101)]
    tree = J48(as.factor(Y)~., cutTrainData)
    pred = prediction(predict(tree, testData, type = "probability")[, 2], testData$Y)
    auc = performance(pred, measure = "auc")
    auc <- auc@y.values[[1]]
    if (auc >= bestAUC) {
      bestAUC = auc
      bestAB[nrow(bestAB) + 1,] = c(A, B)
    }	
  }
}
print(bestAUC)
print(bestAB)
  
