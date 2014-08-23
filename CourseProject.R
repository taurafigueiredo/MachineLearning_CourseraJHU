download.file(url = "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", destfile = "pml-training.csv")
download.file(url = "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv", destfile = "pml-testing.csv")

pmlTraining <- read.csv("pml-training.csv")
pmlTesting <- read.csv("pml-testing.csv")

# Remove empty, NA and not useful variables
ProcessData <- function (dataframe){
  col <- vector(mode = "numeric")
  count <- 0
  for(i in 1:ncol(dataframe))
  {
    if(colnames(dataframe)[i]=='classe'){next;}
    
    total <- length(dataframe[,i]);
    
    NAs <- length(dataframe[is.na(dataframe[,i]),i]);
    empty <- length(dataframe[dataframe[,i]=='',i])
    notNumber <- length(dataframe[!is.numeric(dataframe[,i]),i]);
    
    if(round(NAs/total)==1 | round(empty/total)==1 | round(notNumber/total)==1){
      col <- rbind(col, i)
      count <- count+1;
    }
  }
  
  process <- dataframe[,as.numeric(col)*(-1)]
  for(i in 1:(ncol(process)-1)){ process[,i] <- as.numeric(process[,i]); }
  process[,c(1,2,3,4)*(-1)]
}

cleanedData <- ProcessData(pmlTraining)

library(caret)
library(randomForest)

inTrain <- createDataPartition(y = cleanedData$classe, p = 0.7, list = FALSE)

trainSet <- cleanedData[inTrain,]
validationSet <- cleanedData[-inTrain,]

fit <- randomForest(classe ~ ., data = trainSet)

confusionMatrix(predict(fit, validationSet[,-53]), validationSet$classe)

testSet <- ProcessData(pmlTesting)

answers2 <- predict(fit, testSet)

confusionMatrix(answers, predict(fit,testSet))

