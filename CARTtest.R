CARTtest = function(trainCSV, testCSV) {
    trainData = read.csv(trainCSV)
    testData =  read.csv(testCSV)
    trainData = within(trainData, rm(X))                            
    testData = within(testData, rm(X))                            
    tree = rpart(Y ~ ., data = trainData, method = "class")
    pred = predict(tree, newdata = testData,type = "class") 
    print(tree)
}

library(rpart)
CARTtest("donory_trening.csv", "donory_test.csv")
