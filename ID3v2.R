library(RWeka)
library("ROCR")

WPM("refresh-cache")
WPM("list-packages", "available")
WPM("install-package", "simpleEducationalLearningSchemes")
WPM("load-package", "simpleEducationalLearningSchemes")


ID3test = function(trainCSV, testCSV) {
	ID3 = make_Weka_classifier("weka/classifiers/trees/Id3")

    trainData = read.csv(trainCSV)
    testData =  read.csv(testCSV)
	trainData = within(trainData, rm(X))                            
	testData = within(testData, rm(X))

	bestAUC = 0;
	bestAB = data.frame(A = integer(), B = integer(), AUC = double())
	for (A in 5:50){
	  for (B in 5:49)	{
		cutTrainData = trainData[,c( (51-A):(51+B), 101)]
		tree = ID3(as.factor(Y)~ ., cutTrainData)
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


print("Test algorytmu ID3 dla donorów.")
ID3test("donory_trening.csv", "donory_test.csv")
print("Test algorytmu ID3 dla akceptorow.")
ID3test("akceptory_trening.csv", "akceptory_test.csv")
