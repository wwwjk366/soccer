library(dplyr)
library(caret)
library(scales)
library(gridExtra)

rm(list=ls())
gc()
rawdata <- read.csv("TrainingData_Sample2.csv", stringsAsFactors=FALSE)



theme_custom <- function(base_size = 11) {
  theme_grey(base_size = base_size) %+replace%
    theme(
      line =              element_line(colour = '#DADADA', size = 0.75, 
                                       linetype = 1, lineend = "butt"),
      rect =              element_rect(fill = "#F0F0F0", colour = "#F0F0F0", 
                                       size = 0.5, linetype = 1),
      text =              element_text(family = '', face = "plain",
                                       colour = "#656565", size = base_size,
                                       hjust = 0.5, vjust = 0.5, angle = 0, 
                                       lineheight = 0.9,margin = margin(), debug = FALSE),
      plot.title =        element_text(size = rel(1.5), 
                                       face = 'bold', hjust = -0.05, 
                                       vjust = 1.5, colour = '#3B3B3B'),
      axis.title.x =      element_blank(),
      axis.title.y =      element_blank(),
      axis.text =         element_text(),
      axis.ticks =        element_line(),
      panel.grid.major =  element_line(),
      panel.grid.minor =  element_blank(),
      plot.background =   element_rect(),
      panel.background =  element_rect(),
      legend.key =        element_rect(colour = '#DADADA'),
      legend.position = 'none'
    )
}
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
    ggtitle("Histogram of Goals") + 
  theme_custom()


ggplot(rawdata %>% group_by(ID) %>% summarise(home = max(MeanH), away = max(MeanA)), aes(home)) + 
  geom_histogram(aes(y = ..density..),binwidth = 0.1,fill = mycolor[1], color = mycolor[2]) +
  geom_density(color=mycolor[3],size = 1) +
  ggtitle("Histogram of MeanH") + 
  theme_custom()

ggplot(rawdata %>% group_by(ID) %>% summarise(home = max(MeanH), away = max(MeanA)), aes(away)) + 
  geom_histogram(aes(y = ..density..),binwidth = 0.1,fill = mycolor[1], color = mycolor[2]) +
  geom_density(color=mycolor[3],size = 1) +
  ggtitle("Histogram of MeanA") + 
  theme_custom()




a = rawdata %>% group_by(ID) %>% summarise(home = max(rbS.FinalScoreH))
b = as.data.frame(rpois(9999,mean(a$home)))
names(b)[1] = "simu"


p1 = ggplot(a, aes(home)) + 
  geom_histogram(aes(y = ..density..),binwidth = 1,fill = mycolor[1], color = mycolor[7]) +
  ggtitle("Histogram of number of HGoals") + 
  theme_custom()
  

p2 = ggplot(b, aes(simu)) + 
  geom_histogram(aes(y = ..density..),binwidth = 1,fill = mycolor[1], color = mycolor[7]) +
  ggtitle("Histogram of Simulated") + 
  theme_custom()

grid.arrange(p1,p2)




ggplot(rawdata, aes(x = DrawProb, y = LeagueRanking)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90))  + 
  coord_flip() + 
  theme_custom()


ggplot(rawdata, aes(x = ScoreDiff, y = RCDiff)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90))  + 
  coord_flip()+ 
  theme_custom()


cleandata <- rawdata %>%
  select(-NeutralField) %>%
  mutate(RC_H = (RCTotal + RCDiff)/2, RC_A = (RCTotal - RCDiff)/2, 
         CScore_H = (TotalScore + ScoreDiff)/2, CScore_A = (TotalScore - ScoreDiff)/2,
         RMScore_H = rbS.FinalScoreH - CScore_H, MScore_A = rbS.FinalScoreA - CScore_A)


sort(table(rawdata$League))

df1 = rawdata %>% group_by(League, ID) %>%
  summarise(drawp = max(DrawProb)) %>%
  group_by(League) %>%
  summarise(meam_draw = mean(drawp, na.rm = T))

# stats model training ----------------------------------------------------------


set.seed(3456)
trainIndex <- createDataPartition(unique(cleandata$ID), p = .5,
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

# two factor MLE ----------------------------------------------------------


# two factors maximum likelihood
dat <- train_a #%>% select(rbS.FinalScoreH, MeanH, SoG_H)

LogLike <- function(dat, par) {
  beta0 <- par[1]
  beta1 <- par[2]
  beta2 <- par[3]
  lambda <- exp(beta0 + beta1 * dat$MeanH + beta2 * dat$SoG_H)
  LL <- -sum(dpois(dat$rbS.FinalScoreH, lambda, log = TRUE))
  return(LL)
}

  
beta0 <- rnorm(1)
beta1 <- rnorm(1)
beta2 <- rnorm(1)
par <- c(beta0, beta1, beta2)


m.like <- optim(par = par, fn = LogLike, dat = dat)
m.like
new.predict <- exp(m.like$par[1] + m.like$par[2] * test_a$MeanH + m.like$par[3] * test_a$SoG_H)
postResample(new.predict, test_a$rbS.FinalScoreH)


# real time model ---------------------------------------------------------

train %>% filter(ID == 51033 & Minute !=0) %>% select(Minute, CScore_H, CScore_A) %>% distinct(CScore_H, CScore_A)

temp <- train %>% filter( Minute !=0)

Llike <- function(dat, par) {
  
  lDf <- split(dat, list(dat$ID, dat$TotalScore), drop =  TRUE)
  beta0 <- par[1]
  beta1 <- par[2]
  beta2 <- par[3]
  beta10 <- par[4]
  beta01 <- par[5]
  LL <- 0
  for(i in 1:length(lDf)) {
    
    if(max(lDf[[i]]$ScoreDiff) > 0) {
          lambda <- exp(beta0 + beta1 * lDf[[i]]$MeanH + beta2 * lDf[[i]]$SoG_H + beta10)
      } else if(max(lDf[[i]]$ScoreDiff) < 0){
        lambda <- exp(beta0 + beta1 * lDf[[i]]$MeanH + beta2 * lDf[[i]]$SoG_H + beta01)
          } else {
            lambda <- exp(beta0 + beta1 * lDf[[i]]$MeanH + beta2 * lDf[[i]]$SoG_H)
          }
          LL <- sum(dpois(lDf[[i]]$RMScore_H, lambda, log = TRUE)) + LL
        
      }
      return(-LL)  
    }
  
beta0 <- rnorm(1)
beta1 <- rnorm(1)
beta2 <- rnorm(1)
beta10 <- rnorm(1)
beta01 <- rnorm(1)

par <- c(beta0, beta1, beta2, beta10, beta01)

ptm <- proc.time()
m.like <- optim(par = par, fn = Llike, dat = temp)
m.like

proc.time() - ptm


new.predict <-  function(dat, m.like) {
    x = NULL
    lDf <- split(dat, list(dat$ID, dat$TotalScore), drop =  TRUE)
      for(i in 1:length(lDf)) {
        if(max(lDf[[i]]$ScoreDiff) > 0) {
        x <- append(x,exp(m.like$par[1] + m.like$par[2] * lDf[[i]]$MeanH + m.like$par[3] * lDf[[i]]$SoG_H + m.like$par[4]))
      } else if(max(lDf[[i]]$ScoreDiff) < 0){
        x <- append(x,exp(m.like$par[1] + m.like$par[2] * lDf[[i]]$MeanH + m.like$par[3] * lDf[[i]]$SoG_H + m.like$par[5]))
      } else {
        x <- append(x,exp(m.like$par[1] + m.like$par[2] * lDf[[i]]$MeanH + m.like$par[3] * lDf[[i]]$SoG_H))
      }
        }
        return(x)  
}

a = new.predict(test,m.like)

postResample(a, test$RMScore_H)




