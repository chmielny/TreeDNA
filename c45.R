library(RWeka)
library("ROCR")

C45test = function(trainCSV, testCSV) {
    trainData = read.csv(trainCSV)
    testData =  read.csv(testCSV)
	trainData = within(trainData, rm(X))
	testData = within(testData, rm(X))

	bestAUC = 0;
	bestAB = data.frame(A = integer(), B = integer(), AUC = double())
	for (A in 5:50){
	  for (B in 5:49)	{
		cutTrainData = trainData[,c( (51-A):(51+B), 101)]
		tree = J48(as.factor(Y)~., cutTrainData)
		pred = prediction(predict(tree, testData, type = "probability")[, 2], testData$Y)
		auc = performance(pred, measure = "auc")
		auc <- auc@y.values[[1]]
		if (auc >= bestAUC) {
		  bestAUC = auc
		  bestAB[nrow(bestAB) + 1,] = c(A, B, auc)
		}	
	  }
	}
    print(bestAUC)
	print(bestAB[bestAB$AUC == bestAUC, ])
}
 
print("Test algorytmu C4.5 dla donorów.")
C45test("donory_trening.csv", "donory_test.csv")
print("Test algorytmu C4.5 dla akceptorow.")
C45test("akceptory_trening.csv", "akceptory_test.csv")
