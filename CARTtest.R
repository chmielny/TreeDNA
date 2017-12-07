CARTtest = function(trainCSV, testCSV) {
    trainData = read.csv(trainCSV)
    testData =  read.csv(testCSV)
    trainData = within(trainData, rm(X))                            
    testData = within(testData, rm(X)) 
	
	tmpData = trainData[,c(10:60,101)]						#wycinanie kolumn z dataframe, X10-X60, 101 to Ye                     
    tree = rpart(Y ~ ., data = tmpData, method = "class")
    pred = predict(tree, newdata = testData,type = "class")
#	print(predict(tree, type = "prob")[, 2]) 
	pred = prediction(predict(tree, newdata = testData, type = "prob")[, 2], testData$Y)
    print(tree)
	plot(performance(pred, "tpr", "fpr"))
	abline(0, 1, lty = 2)
}

library(rpart)
library("ROCR")
CARTtest("donory_trening.csv", "donory_test.csv")
