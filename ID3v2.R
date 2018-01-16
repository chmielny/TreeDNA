IsPure = function(data) {
  length(unique(data[,ncol(data)])) == 1
}

Entropy = function( vls ) {
  res = vls/sum(vls) * log2(vls/sum(vls))
  res[vls == 0] = 0
  -sum(res)
}

InformationGain = function( tble ) {
  tble = as.data.frame.matrix(tble)
  entropyBefore = Entropy(colSums(tble))
  s = rowSums(tble)
  entropyAfter = sum (s / sum(s) * apply(tble, MARGIN = 1, FUN = Entropy ))
  informationGain = entropyBefore - entropyAfter
  return (informationGain)
}

TrainID3 = function(node, data) {
  
  node$obsCount = nrow(data)
  
  if (IsPure(data)) {
    child = node$AddChild(unique(data[,ncol(data)]))
    node$feature = tail(names(data), 1)
    child$obsCount = nrow(data)
    child$feature = ''
  } else {
    ig = sapply(colnames(data)[-ncol(data)], 
    function(x) InformationGain(table(data[,x], data[,ncol(data)])))
    feature = names(ig)[ig == max(ig)][1]
    
    node$feature = feature
    
    childObs = split(data[,!(names(data) %in% feature)], data[,feature], drop = TRUE)
    
    for(i in 1:length(childObs)) {
      child = node$AddChild(names(childObs)[i])
            
      TrainID3(child, childObs[[i]])
    }
  }
}

Predict = function(tree, features) {
  if (tree$children[[1]]$isLeaf) return (tree$children[[1]]$name)
  child = tree$children[[features[[tree$feature]]]]
  return ( Predict(child, features))
}

###################################
trainData = read.csv("donory_trening.csv")
testData =  read.csv("donory_test.csv")
trainData = within(trainData, rm(X))                            
testData = within(testData, rm(X))
#bestAUC = 0;
#bestAB = data.frame(A = integer(), B = integer())

#for (A in 5:50){
#  for (B in 5:49)	{
    cutTrainData = trainData[,c( (51-5):(51+5), 101)] 
    tree = Node$new()
    TrainID3(tree, cutTrainData)
    #print(tree)
    #Predict(tree, testData)
    #auc = performance(pred, measure = "auc")
    #auc <- auc@y.values[[1]]  
    #if (auc >= bestAUC) {
      #bestAUC = auc
      #bestAB[nrow(bestAB) + 1,] = c(A, B)
    #}	
 # }
#}
#print(bestAUC)
#print(bestAB)
    
#library("ROCR")
