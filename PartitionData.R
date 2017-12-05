#skrypt do podzialu danych na trenujace i walidacyjne
#potrzebny jest pakiet caret

partitionData = function(inpCSV, outTrain, outCheck) {
    data = read.csv(inpCSV)
    trainIndex = createDataPartition(y=data$Y, p=0.7, list=FALSE)           # wykorzystanie funkcji z pakietu caret
    trainData = within(data[trainIndex,], rm(X))                            # ktora dzieli zbior w stosunku 0.7/0.3
    rownames(trainData) <- NULL                                             # zachowujac w przyblizeniu stosunek prawdziwych do falszywych donorow/akceptorow
    validData = within(data[-trainIndex,], rm(X))
    rownames(validData) <- NULL
    write.csv(file=outTrain, x=trainData)                                   # zapis dataframe do plikow
    write.csv(file=outCheck, x=validData)                                   # zapis dataframe do plikow
}

library(caret)
partitionData("donory.csv", "donory_trening.csv", "donory_test.csv")
partitionData("akceptory.csv", "akceptory_trening.csv", "akceptory_test.csv")
