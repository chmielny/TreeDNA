CARTtest = function(trainCSV, testCSV) {
    trainData = read.csv(trainCSV)
    testData =  read.csv(testCSV)
    trainData = within(trainData, rm(X))                            
    testData = within(testData, rm(X))
    bestAUC = 0;
    bestAB = data.frame(A = integer(), B = integer(), AUC = double())
    for (A in 5:50){
        for (B in 5:49)	{
            cutTrainData = trainData[,c( (51-A):(51+B), 101)]						# wycinanie kolumn z dataframe - badanie param. A i B, 101 to Ye                     
            tree = rpart(Y ~ ., data = cutTrainData, method = "class")					# tworzenie modelu za pomoca algorytmu CART
            pred = prediction(predict(tree, newdata = testData, type = "prob")[, 2], testData$Y)	# predykcja prawdopodobienstw w lisciach dla danych testowych
            auc = performance(pred, measure = "auc")							# obliczenie auc
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

library(rpart)
library("ROCR")
print("Test algorytmu CART dla donorów.")
CARTtest("donory_trening.csv", "donory_test.csv")
print("Test algorytmu CART dla akceptorow.")
CARTtest("akceptory_trening.csv", "akceptory_test.csv")
