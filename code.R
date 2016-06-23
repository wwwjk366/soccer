library(dplyr)
library(caret)
library(scales)

rawdata <- read.csv("TrainingData_Sample2.csv", stringsAsFactors=FALSE)


# discriptive -------------------------------------------------------------
mycolor <- c(rep(c("#FF5A5F","#FFB400", "#007A87", "#8CE071", "#7B0051", "#00D1C1",
                    "#FFAA91", "#B4A76C", "#9CA299", "#565A5C", "#00A04B","#E54C20"), 100))

summary(rawdata)

goaltime <- rawdata %>% 
  group_by(ID) %>%
  mutate(gtime = TotalScore - lag(TotalScore)) %>%
  select(ID, Minute, TotalScore, gtime) %>%
  filter(gtime == 1)

ggplot(goaltime, aes(Minute)) + 
  geom_histogram(aes(y = ..density..),binwidth = 1,fill = mycolor[1], color = mycolor[2]) +
  geom_density(color=mycolor[3],size = 1.5) +
    ggtitle("Histogram of Goals")


ggplot(rawdata, aes(x = DrawProb, y = League)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90))  + 
  coord_flip()

sort(table(rawdata$League))



# model training ----------------------------------------------------------


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
