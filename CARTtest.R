CARTtest = function(trainCSV, testCSV) {
    trainData = read.csv(trainCSV)
    testData =  read.csv(testCSV)
    trainData = within(trainData, rm(X))                            
    testData = within(testData, rm(X)) 
	
    cutTrainData = trainData[,c(10:60,101)]							# wycinanie kolumn z dataframe, X10-X60, 101 to Ye                     
    tree = rpart(Y ~ ., data = cutTrainData, method = "class")					# tworzenie modelu za pomoca algorytmu CART
    pred = prediction(predict(tree, newdata = testData, type = "prob")[, 2], testData$Y)	# predykcja prawdopodobienstw w lisciach dla danych testowych
    print(tree)
    auc = performance(pred, measure = "auc")							# obliczenie auc
    auc <- auc@y.values[[1]]  
    print(auc)
}

library(rpart)
library("ROCR")
CARTtest("donory_trening.csv", "donory_test.csv")
