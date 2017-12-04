#skrypt do podzialu danych na trenujace i walidacyjne


partitionData = function(inpCSV, outTrain, outCheck) {
    data = read.csv(inpCSV)
    test = createDataPartition(y=data$Y,p=0.75, list=FALSE)
    print(test)
}

library(caret)
partitionData("akceptory.csv", "a", "b")
