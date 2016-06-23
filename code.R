library(dplyr)
library(caret)
library(forecast)

rawdata <- read.csv("E:/Test Projects/test/soccer/TrainingData_Sample2.csv", stringsAsFactors=FALSE)
set.seed(3456)
trainIndex <- createDataPartition(unique(rawdata$ID), p = .8,
                                  list = FALSE,
                                  times = 1)
unique(rawdata$ID)[-trainIndex]

train <- filter(rawdata, ID %in% unique(rawdata$ID)[trainIndex])
test <- filter(rawdata, ID %in% unique(rawdata$ID)[-trainIndex])

model <- glm(rbS.FinalScoreH ~ MeanH + MeanA + Minute + TotalScore + ScoreDiff,
             family = poisson(link = log), data = train)

model2 <- glm(rbS.FinalScoreA ~ MeanH + MeanA + Minute + TotalScore + ScoreDiff,
             family = poisson(link = log), data = train)
summary(model)
varImp(model)

plot(model)

test1 = select(test, MeanH , MeanA , Minute , TotalScore ,ScoreDiff)

a = predict(model, test1, type="response")

accuracy()

plotrange <- 0:6
dpois(0:7,a)

b = predict(model2, test1[1,], type="response")
dpois(0:7,b)
