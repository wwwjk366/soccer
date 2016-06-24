library(dplyr)
library(caret)
library(scales)
library(xgboost)

rm(list=ls())
rawdata <- read.csv("TrainingData_Sample2.csv", stringsAsFactors=FALSE)


# discriptive -------------------------------------------------------------
mycolor <- c(rep(c("#FF5A5F","#FFB400", "#007A87", "#8CE071", "#7B0051", "#00D1C1",
                    "#FFAA91", "#B4A76C", "#9CA299", "#565A5C", "#00A04B","#E54C20"), 100))

summary(rawdata)

goaltime <- rawdata %>% 
  group_by(ID) %>%
  mutate(gtime = TotalScore - lag(TotalScore)) %>%
  select(ID, Minute, TotalScore, gtime, HasPenaltyShootout) %>%
  filter(gtime == 1)

ggplot(goaltime, aes(Minute)) + 
  geom_histogram(aes(y = ..density..),binwidth = 1,fill = mycolor[1], color = mycolor[2]) +
  geom_density(color=mycolor[3],size = 1.5) +
    ggtitle("Histogram of Goals")


ggplot(rawdata %>% group_by(ID) %>% summarise(home = max(MeanH), away = max(MeanA)), aes(home)) + 
  geom_histogram(aes(y = ..density..),binwidth = 0.1,fill = mycolor[1], color = mycolor[2]) +
  geom_density(color=mycolor[3],size = 1) +
  ggtitle("Histogram of MeanH")

ggplot(rawdata %>% group_by(ID) %>% summarise(home = max(MeanH), away = max(MeanA)), aes(away)) + 
  geom_histogram(aes(y = ..density..),binwidth = 0.1,fill = mycolor[1], color = mycolor[2]) +
  geom_density(color=mycolor[3],size = 1) +
  ggtitle("Histogram of MeanA")


ggplot(rawdata, aes(DrawProb)) + 
  geom_histogram(aes(y = ..density..),binwidth = 0.01,fill = mycolor[1], color = mycolor[2]) +
  geom_density(color=mycolor[3],size = 1) +
  facet_wrap( ~ LeagueRanking)

ggplot(rawdata, aes(x = DrawProb, y = LeagueRanking)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90))  + 
  coord_flip()






ggplot(rawdata, aes(x = ScoreDiff, y = RCDiff)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90))  + 
  coord_flip()


cleandata <- rawdata %>%
  select(-NeutralField) %>%
  mutate(RC_H = (RCTotal + RCDiff)/2, RC_A = (RCTotal - RCDiff)/2, 
         CScore_H = (TotalScore + ScoreDiff)/2, CScore_A = (TotalScore - ScoreDiff)/2)




sort(table(rawdata$League))

df1 = rawdata %>% group_by(League, ID) %>%
  summarise(drawp = max(DrawProb)) %>%
  group_by(League) %>%
  summarise(meam_draw = mean(drawp, na.rm = T))

# stats model training ----------------------------------------------------------


set.seed(3456)
trainIndex <- createDataPartition(unique(cleandata$ID), p = .8,
                                  list = FALSE,
                                  times = 1)
unique(cleandata$ID)[-trainIndex]

train <- filter(cleandata, ID %in% unique(cleandata$ID)[trainIndex])
test <- filter(cleandata, ID %in% unique(cleandata$ID)[-trainIndex])

train_a <- filter(train, Minute == 91)
test_a <- filter(test, Minute == 91) %>% select(MeanH , RC_H ,YC_H, SoG_H, DAT_H, CR_H, FK_H, rbS.FinalScoreH)


# all factors
model_a <- glm(rbS.FinalScoreH ~ MeanH + RC_H + YC_H + SoG_H + DAT_H + CR_H + FK_H,
             family = "poisson", data = train_a)
summary(model_a)
pred_a = predict(model_a, test_a , type="response")
postResample(pred_a, test_a$rbS.FinalScoreH)


# two factors
model_a <- glm(rbS.FinalScoreH ~ MeanH + SoG_H, family = "poisson", data = train_a)
summary(model_a)
pred_a = predict(model_a, test_a %>% select(MeanH, SoG_H) , type="response")
postResample(pred_a, test_a$rbS.FinalScoreH)


#xgboost
{
train.y = train_a$rbS.FinalScoreH
train.m = as.matrix(select(train_a, MeanH , RC_H ,YC_H, SoG_H, DAT_H, CR_H, FK_H))



#xgboost22

train.y = train$rbS.FinalScoreH
train.m = as.matrix(select(train, MeanH , DrawProb, LeagueRanking, RC_H ,YC_H, SoG_H, DAT_H, CR_H, FK_H))
test.m = as.matrix(select(test, MeanH , DrawProb, LeagueRanking, RC_H ,YC_H, SoG_H, DAT_H, CR_H, FK_H))

dtrain <- xgb.DMatrix(data=train.m, label=train.y,missing = NA)
dtest <- xgb.DMatrix(data=test.m, label = test$rbS.FinalScoreH,missing = NA)

watchlist <- list(train=dtrain)
param <- list(  objective           = "reg:linear", 
                booster             = "gblinear",
                eval_metric         = "rmse",
                alpha               = 0.0001, 
                lambda              = 1
                
)

clf <- xgb.train(   params              = param, 
                    data                = dtrain, 
                    nrounds             = 500, 
                    early.stop.round    = 5,
                    verbose             = 1,
                    watchlist           = watchlist
                    )


postResample(predict(clf,train.m), train.y)
postResample(predict(clf,dtest), test$rbS.FinalScoreH)
}

###


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


# score parameter ---------------------------------------------------------





# time parameter ----------------------------------------------------------




# generate matrix ---------------------------------------------------------


