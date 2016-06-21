library(dplyr)
library(caret)

rawdata <- read.csv("E:/Test Projects/test/soccer/TrainingData_Sample2.csv", stringsAsFactors=FALSE)
set.seed(3456)
trainIndex <- createDataPartition(rawdata$ID, p = .8,
                                  list = FALSE,
                                  times = 1)
head(trainIndex)

train <- rawdata[ trainIndex,]
rest  <- rawdata[-trainIndex,]
