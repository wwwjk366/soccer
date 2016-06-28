library(dplyr)
library(caret)
library(scales)
library(gridExtra)

rm(list=ls())
gc()
rawdata <- read.csv("TrainingData_Sample2.csv", stringsAsFactors=FALSE)

cleandata <- rawdata %>%
  select(-NeutralField) %>%
  mutate(RC_H = (RCTotal + RCDiff)/2, RC_A = (RCTotal - RCDiff)/2, 
         CScore_H = (TotalScore + ScoreDiff)/2, CScore_A = (TotalScore - ScoreDiff)/2,
         RMScore_H = rbS.FinalScoreH - CScore_H, RMScore_A = rbS.FinalScoreA - CScore_A)



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

b = as.data.frame(rpois(99999,mean(cleandata$RMScore_H)))
names(b)[1] = "simu"

p3 = ggplot(cleandata, aes(RMScore_H)) + 
  geom_histogram(aes(y = ..density..),binwidth = 1,fill = mycolor[1], color = mycolor[7]) +
  ggtitle("Histogram of number of HGoals") + 
  theme_custom()


p4 = ggplot(b, aes(simu)) + 
  geom_histogram(aes(y = ..density..),binwidth = 1,fill = mycolor[1], color = mycolor[7]) +
  ggtitle("Histogram of Simulated") + 
  theme_custom()
grid.arrange(p3,p4)

# still poisson!

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



sort(table(rawdata$League))

df1 = rawdata %>% group_by(League, ID) %>%
  summarise(drawp = max(DrawProb)) %>%
  group_by(League) %>%
  summarise(meam_draw = mean(drawp, na.rm = T))

# stats model training ----------------------------------------------------------


set.seed(1234)
trainIndex <- createDataPartition(unique(cleandata$ID), p = .6,
                                  list = FALSE,
                                  times = 1)


train <- filter(cleandata, ID %in% unique(cleandata$ID)[trainIndex])
test <- filter(cleandata, ID %in% unique(cleandata$ID)[-trainIndex])

train_a <- filter(train, Minute == 91)

test_a <- filter(test, Minute == 91) %>% select(MeanH , RC_H ,YC_H, SoG_H, DAT_H, CR_H, FK_H, RMScore_H)

cor(test_a)
# all factors
model_h <- glm(RMScore_H ~ MeanH + RC_H + YC_H + SoG_H + DAT_H + CR_H + FK_H,
             family = "poisson", data = train)
summary(model_h)
pred_h = predict(model_h, test, type="response")
postResample(pred_h, test$RMScore_H)

model_a <- glm(RMScore_A ~ MeanA + RC_A + YC_A + SoG_A + DAT_A + CR_A + FK_A,
               family = "poisson", data = train)
summary(model_a)
pred_a = predict(model_a, test, type="response")
postResample(pred_a, test$RMScore_H)

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

########### model for home team
ptm <- proc.time()
#train %>% filter(ID == 51033 & Minute !=0) %>% select(Minute, CScore_H, CScore_A) %>% distinct(CScore_H, CScore_A)

temp <- train %>% filter( Minute !=0)
test <- test %>% filter( Minute !=0)

Llike_h <- function(dat, par) {
  
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

#est_h <- nlm(Llike_h, par,dat = temp)
est_h <- optim(par = par, fn = Llike_h, dat = temp)
est_h

new.predict_h <-  function(dat, est) {
    x = NULL
    lDf <- split(dat, list(dat$ID, dat$TotalScore), drop =  TRUE)
      for(i in 1:length(lDf)) {
        if(max(lDf[[i]]$ScoreDiff) > 0) {
        x <- append(x,exp(est$par[1] + est$par[2] * lDf[[i]]$MeanH + est$par[3] * lDf[[i]]$SoG_H + est$par[4]))
      } else if(max(lDf[[i]]$ScoreDiff) < 0){
        x <- append(x,exp(est$par[1] + est$par[2] * lDf[[i]]$MeanH + est$par[3] * lDf[[i]]$SoG_H + est$par[5]))
      } else {
        x <- append(x,exp(est$par[1] + est$par[2] * lDf[[i]]$MeanH + est$par[3] * lDf[[i]]$SoG_H))
      }
        }
        return(x)  
}

predict_h = new.predict_h(test,est_h)

RMSE(predict_h, test$RMScore_H)

########### model for away team


Llike_a <- function(dat, par) {
  
  lDf <- split(dat, list(dat$ID, dat$TotalScore), drop =  TRUE)
  gamma0 <- par[1]
  gamma1 <- par[2]
  gamma2 <- par[3]
  gamma10 <- par[4]
  gamma01 <- par[5]
  LL <- 0
  for(i in 1:length(lDf)) {
    
    if(max(lDf[[i]]$ScoreDiff) < 0) {
      mu <- exp(gamma0 + gamma1 * lDf[[i]]$MeanA + gamma2 * lDf[[i]]$SoG_A + gamma10)
    } else if(max(lDf[[i]]$ScoreDiff) >0){
      mu <- exp(gamma0 + gamma1 * lDf[[i]]$MeanA + gamma2 * lDf[[i]]$SoG_A + gamma01)
    } else {
      mu <- exp(gamma0 + gamma1 * lDf[[i]]$MeanA + gamma2 * lDf[[i]]$SoG_A)
    }
    LL <- sum(dpois(lDf[[i]]$RMScore_A, mu, log = TRUE)) + LL
  }
  return(-LL)  
}

gamma0 <- rnorm(1)
gamma1 <- rnorm(1)
gamma2 <- rnorm(1)
gamma10 <- rnorm(1)
gamma01 <- rnorm(1)

par <- c(gamma0, gamma1, gamma2, gamma10, gamma01)


#est_a <- nlm(Llike_a, par,dat = temp)
est_a <- optim(par = par, fn = Llike_a, dat = temp)
est_a

new.predict_a <-  function(dat, est) {
  x = NULL
  lDf <- split(dat, list(dat$ID, dat$TotalScore), drop =  TRUE)
  for(i in 1:length(lDf)) {
    if(max(lDf[[i]]$ScoreDiff) < 0) {
      x <- append(x,exp(est_a$par[1] + est_a$par[2] * lDf[[i]]$MeanA + est_a$par[3] * lDf[[i]]$SoG_A + est_a$par[4]))
    } else if(max(lDf[[i]]$ScoreDiff) > 0){
      x <- append(x,exp(est_a$par[1] + est_a$par[2] * lDf[[i]]$MeanA + est_a$par[3] * lDf[[i]]$SoG_A + est_a$par[5]))
    } else {
      x <- append(x,exp(est_a$par[1] + est_a$par[2] * lDf[[i]]$MeanA + est_a$par[3] * lDf[[i]]$SoG_A))
    }
  }
  return(x)  
}

predict_a = new.predict_a(test,est_a)

RMSE(predict_a, test$RMScore_A)

proc.time() - ptm

# more complex real time model ---------------------------------------------------------

set.seed(1234)
trainIndex <- createDataPartition(unique(cleandata$ID), p = .6,
                                  list = FALSE,
                                  times = 1)


train <- filter(cleandata, ID %in% unique(cleandata$ID)[trainIndex])
test <- filter(cleandata, ID %in% unique(cleandata$ID)[-trainIndex])

preObj <- preProcess(train[, 14:22], method=c("center", "scale"))

trainBC <- predict(preObj, train)
testBC <- predict(preObj, test)

########### model for home team
ptm <- proc.time()
#train %>% filter(ID == 51033 & Minute !=0) %>% select(Minute, CScore_H, CScore_A) %>% distinct(CScore_H, CScore_A)

temp <- trainBC %>% filter( Minute !=0)
test <- testBC %>% filter( Minute !=0)

Llike_h <- function(dat, par) {
  
  lDf <- split(dat, list(dat$ID, dat$TotalScore), drop =  TRUE)
  beta0 <- par[1]
  beta1 <- par[2]
  beta2 <- par[3]
  beta10 <- par[4]
  beta01 <- par[5]
  beta3 <- par[6]
  beta4 <- par[7]
  beta5 <- par[8]
  beta6 <- par[9]

  LL <- 0
  for(i in 1:length(lDf)) {
    
    if(max(lDf[[i]]$ScoreDiff) > 0) {
      lambda <- exp(beta0 * sqrt(lDf[[i]]$Minute/91)  + beta1 * lDf[[i]]$MeanH + beta2 * lDf[[i]]$SoG_H +
                      beta3*lDf[[i]]$DAT_H + beta4*lDf[[i]]$YC_H  + beta5*lDf[[i]]$FK_H + beta6 + beta10)
    } else if(max(lDf[[i]]$ScoreDiff) < 0){
      lambda <- exp(beta0 *sqrt(lDf[[i]]$Minute/91)  + beta1 * lDf[[i]]$MeanH + beta2 * lDf[[i]]$SoG_H +
                      beta3*lDf[[i]]$DAT_H + beta4*lDf[[i]]$YC_H  + beta5*lDf[[i]]$FK_H + beta6 + beta01)
    } else {
      lambda <- exp(beta0 *sqrt(lDf[[i]]$Minute/91)  + beta1 * lDf[[i]]$MeanH + beta2 * lDf[[i]]$SoG_H +
                      beta3*lDf[[i]]$DAT_H + beta4*lDf[[i]]$YC_H  + beta5*lDf[[i]]$FK_H + beta6)
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
beta3 <- rnorm(1)
beta4 <- rnorm(1)
beta5 <- rnorm(1)
beta6 <- rnorm(1)

par <- c(beta0, beta1, beta2, beta10, beta01,beta3, beta4, beta5,beta6)

est_h <- nlm(Llike_h, par,dat = temp)
names(est_h)[2] = "par"

#est_h <- optim(par = par, fn = Llike_h, dat = temp, control = c(maxit = 1000))
est_h

new.predict_h <-  function(dat, est) {
  x = NULL
  lDf <- split(dat, list(dat$ID, dat$TotalScore), drop =  TRUE)
  for(i in 1:length(lDf)) {
    if(max(lDf[[i]]$ScoreDiff) > 0) {
      x <- append(x,exp(est$par[1] * sqrt(lDf[[i]]$Minute/91) + est$par[2] * lDf[[i]]$MeanH + est$par[3] * lDf[[i]]$SoG_H + 
                          est$par[6]*lDf[[i]]$DAT_H + est$par[7]*lDf[[i]]$YC_H + est$par[8]*lDf[[i]]$FK_H +est$par[9]+ est$par[4]))
    } else if(max(lDf[[i]]$ScoreDiff) < 0){
      x <- append(x,exp(est$par[1] * sqrt(lDf[[i]]$Minute/91) + est$par[2] * lDf[[i]]$MeanH + est$par[3] * lDf[[i]]$SoG_H + 
                          est$par[6]*lDf[[i]]$DAT_H + est$par[7]*lDf[[i]]$YC_H + est$par[8]*lDf[[i]]$FK_H +est$par[9]+ est$par[5]))
    } else {
      x <- append(x,exp(est$par[1] * sqrt(lDf[[i]]$Minute/91) + est$par[2] * lDf[[i]]$MeanH + est$par[3] * lDf[[i]]$SoG_H + 
                          est$par[6]*lDf[[i]]$DAT_H + est$par[7]*lDf[[i]]$YC_H + est$par[8]*lDf[[i]]$FK_H +est$par[9]))
    }
  }
  return(x)  
}

predict_h = round(new.predict_h(test,est_h),4)

RMSE(predict_h, test$RMScore_H)

########### model for away team


Llike_a <- function(dat, par) {
  
  lDf <- split(dat, list(dat$ID, dat$TotalScore), drop =  TRUE)
  
  gamma0 <- par[1]
  gamma1 <- par[2]
  gamma2 <- par[3]
  gamma10 <- par[4]
  gamma01 <- par[5]
  gamma3 <- par[6]
  gamma4 <- par[7]
  gamma5 <- par[8]
  gamma6 <- par[9]

  LL <- 0
  for(i in 1:length(lDf)) {
    
    if(max(lDf[[i]]$ScoreDiff) < 0) {
      mu <- exp(gamma0 * sqrt(lDf[[i]]$Minute/91) + gamma1 * lDf[[i]]$MeanA + gamma2 * lDf[[i]]$SoG_A + 
                      gamma3*lDf[[i]]$DAT_A + gamma4*lDf[[i]]$YC_A + gamma5*lDf[[i]]$FK_A + gamma6+ gamma10)
    } else if(max(lDf[[i]]$ScoreDiff) > 0){
      mu <- exp(gamma0 * sqrt(lDf[[i]]$Minute/91)  + gamma1 * lDf[[i]]$MeanA + gamma2 * lDf[[i]]$SoG_A + 
                  gamma3*lDf[[i]]$DAT_A + gamma4*lDf[[i]]$YC_A + gamma5*lDf[[i]]$FK_A + gamma6+ gamma01)
    } else {
      mu <- exp(gamma0 * sqrt(lDf[[i]]$Minute/91)  + gamma1 * lDf[[i]]$MeanA + gamma2 * lDf[[i]]$SoG_A + 
                  gamma3*lDf[[i]]$DAT_A + gamma4*lDf[[i]]$YC_A + gamma5*lDf[[i]]$FK_A + gamma6)
    }
    LL <- sum(dpois(lDf[[i]]$RMScore_A, mu, log = TRUE)) + LL
  }
  return(-LL)  
}

gamma0 <- rnorm(1)
gamma1 <- rnorm(1)
gamma2 <- rnorm(1)
gamma10 <- rnorm(1)
gamma01 <- rnorm(1)
gamma3 <- rnorm(1)
gamma4 <- rnorm(1)
gamma5 <- rnorm(1)
gamma6 <- rnorm(1)


par <- c(gamma0, gamma1, gamma2, gamma10, gamma01,gamma3, gamma4, gamma5,gamma6)

est_a <- nlm(Llike_a, par,dat = temp)
names(est_a)[2] = "par"
#est_a <- optim(par = par, fn = Llike_a, dat = temp)
est_a

new.predict_a <-  function(dat, est) {
  x = NULL
  lDf <- split(dat, list(dat$ID, dat$TotalScore), drop =  TRUE)
  for(i in 1:length(lDf)) {
    if(max(lDf[[i]]$ScoreDiff) < 0) {
      x <- append(x,exp(est$par[1] * sqrt(lDf[[i]]$Minute/91) + est$par[2] * lDf[[i]]$MeanA + est$par[3] * lDf[[i]]$SoG_A + 
                          est$par[6]*lDf[[i]]$DAT_A + est$par[7]*lDf[[i]]$YC_A + est$par[8]*lDf[[i]]$FK_A + est$par[9] +est$par[4]))
    } else if(max(lDf[[i]]$ScoreDiff) > 0){
      x <- append(x,exp(est$par[1] * sqrt(lDf[[i]]$Minute/91) + est$par[2] * lDf[[i]]$MeanA + est$par[3] * lDf[[i]]$SoG_A + 
                          est$par[6]*lDf[[i]]$DAT_A + est$par[7]*lDf[[i]]$YC_A + est$par[8]*lDf[[i]]$FK_A + est$par[9]  +est$par[5]))
    } else {
      x <- append(x,exp(est$par[1] * sqrt(lDf[[i]]$Minute/91) + est$par[2] * lDf[[i]]$MeanA + est$par[3] * lDf[[i]]$SoG_A + 
                          est$par[6]*lDf[[i]]$DAT_A + est$par[7]*lDf[[i]]$YC_A + est$par[8]*lDf[[i]]$FK_A + est$par[9]))
    }
  }
  return(x)  
}

predict_a = round(new.predict_a(test,est_a),4)

RMSE(predict_a, test$RMScore_A)

proc.time() - ptm



# generating matrix -------------------------------------------------------

mats = list()

for(i in 1:length(test$ID)) {
  m = round(dpois(0:8,predict_a[i]) %o% dpois(0:8,predict_h[i]),4)
  rownames(m) = c(0:8) + test$CScore_A[i]
  colnames(m) = c(0:8) + test$CScore_H[i]
  mats[[i]] = m
}


mats2 = list()

for(i in 1:length(test$ID)) {
  m = round(dpois(0:8,pred_a[i]) %o% dpois(0:8,pred_h[i]),4)
  rownames(m) = c(0:8) + test$CScore_A[i]
  colnames(m) = c(0:8) + test$CScore_H[i]
  mats2[[i]] = m
}


# PCA ---------------------------------------------------------------------

ir.pca <- prcomp(log.ir,
                 center = TRUE,
                 scale. = TRUE) 
