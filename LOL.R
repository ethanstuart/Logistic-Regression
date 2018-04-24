#####LOL######

#Model for interpretation: What skills do players need to be successful?


#Response: Win or Loss
# Explanatory Variables
#Kills: More kills, more likely to win
# Errors: More deaths, less likely to win
# Assists: 
# Time: minutes of gameplay. Do long games favor...?

# 2017 games data from Leaguepedia
source('http://grimshawville.byu.edu/eSports2017.R')


##GAMEPLAN####
# Create train + test
# EDA plot, summary stats. 

#Model
# data diagnostics
# plot
#interpretation of effects
# Inference
# Hypothesis test on assists having no effect
#       on time.
# Prediction. check overfit

#create train+ test
set.seed(2319)
train.rows <- sample(3881,3100)
players.train <- all.players[train.rows,]
players.test <- all.players[-train.rows,]

#EDA
summary(players.train$Kills) # Median of 2 kills. Very right skewed. 50% get between 4 and 1. 
summary(players.train$Deaths) # Typical player gets around 2 deaths per game. Right skewed. 50% get between 1 and 3. 
summary(players.train$Assists) # Typical player gets about 6.1 assists per game. The data is right skewed. 50% between 3 and 9.
summary(players.test$Time) #Typical game is 35 minutes. Approximately normal. 50% of data between 30 and 40

#plots
par(mfrow=c(2,2))
boxplot(Kills~Win,data=players.train)
boxplot(Deaths~Win,data=players.train)
boxplot(Assists~Win,data=players.train)
boxplot(Time~Win,data=players.train)
par(mfrow=c(1,1))


# Model:
# log[p(win|kills,deaths,assists,time)/p(notwin|kills,deaths,assits,time))]=  * I guess I don't really need that conditional stuff.
#Bo + B1Kills+ B2Deaths+B3Assits+B4Time
# Use glm function with family="binomial"

out.lol <- glm(Win~Kills+Deaths+Assists+Time,data=players.train,family="binomial")
summary(out.lol)

#interpret change in odds
# B1= .43 means for each additional kill we estimate an increase of 0.43 in log odds of winnning holding all else constant.
#exp(.43)= 1.537 means for each additional kill we estimate the odds of winning increasing 1.54 times holding all esle constant.
# OR a 54% increase in odds.
exp(coef(out.lol))[-1]  # as long as it's less than 2, go with a percent form probably.
exp(confint(out.lol))[-1,]

#1.537 means for each additional kill we estimate the odds of winning increasing 1.54 times holding all esle constant.
# OR a 54% increase in odds. 95% CI (45%,63%)

# For each additional death we estimate a (1-.44) 56% decrease in odds of winning, holding all else constant.
#95% CI(52.3%,59%)

# For each additional assist we estimate a 66% increase in odds of winning, holding all else constant.
#95% CI(1.591,1.731)

# For each additional minute played, we estimate a 1-.923 7.7% decrease in odds of winning, holding all else constant.
#95% CI(.063,.092)




#holding all other factors AT THE MEDIAN to plot one factor's effect. x is the factor, y is predict outcome.

# Kills
x.star <- data.frame(Kills=seq(0,10,length.out = 100),Deaths=2,Assists=6,Time=35)
pred <- predict(out.lol,newdata=x.star,type="response",se.fit = TRUE)
plot(x.star$Kills,pred$fit
     ,type="l",col="midnightblue",ylim=c(0,1),
     xlab="Kills",ylab="P(Win) holding all else at median",
     main="Effect of Kills")
loogit.l <- pred$fit-1.96*pred$se.fit
loogit.u <- pred$fit+1.96*pred$se.fit
lines(x.star$Kills,loogit.u,lty=2,col="aquamarine4")
lines(x.star$Kills,loogit.l,lty=2,col="coral4")
legend("bottomleft",legend=c("Estimate","Upper","Lower"),
       col=c("midnightblue","aquamarine4"
      ,"coral4"),lty=c(1,2,2))

# Deaths
x.star <- data.frame(Deaths=seq(0,10,length.out = 100),Kills=2,Assists=6,Time=35)
pred <- predict(out.lol,newdata=x.star,type="response",se.fit = TRUE)
plot(x.star$Deaths,pred$fit
     ,type="l",col="midnightblue",ylim=c(0,1),
     xlab="Deaths",ylab="P(Win) holding all else at median",
     main="Effect of Deaths")
loogit.l <- pred$fit-1.96*pred$se.fit
loogit.u <- pred$fit+1.96*pred$se.fit
lines(x.star$Deaths,loogit.u,lty=2,col="aquamarine4")
lines(x.star$Deaths,loogit.l,lty=2,col="coral4")
legend("topright",legend=c("Estimate","Upper","Lower"),
       col=c("midnightblue","aquamarine4"
             ,"coral4"),lty=c(1,2,2))

# Assists
x.star <- data.frame(Assists=seq(0,12,length.out = 100),Deaths=2,Kills=2,Time=35)
pred <- predict(out.lol,newdata=x.star,type="response",se.fit = TRUE)
plot(x.star$Assists,pred$fit
     ,type="l",col="midnightblue",ylim=c(0,1),
     xlab="Assists",ylab="P(Win) holding all else at median",
     main="Effect of Assists")
loogit.l <- pred$fit-1.96*pred$se.fit
loogit.u <- pred$fit+1.96*pred$se.fit
lines(x.star$Assists,loogit.u,lty=2,col="aquamarine4")
lines(x.star$Assists,loogit.l,lty=2,col="coral4")
legend("bottomright",legend=c("Estimate","Upper","Lower"),
       col=c("midnightblue","aquamarine4"
             ,"coral4"),lty=c(1,2,2))

# Time ADD CONFIDENCE INTERVALS TO ALL
x.star <- data.frame(Time=seq(25,60,length.out = 100),Deaths=2,Assists=6,Kills=2)
pred <- predict(out.lol,newdata=x.star,type="response",se.fit = TRUE)
plot(x.star$Time,pred$fit
     ,type="l",col="midnightblue",ylim=c(0,1),
     xlab="Time",ylab="P(Win) holding all else at median",
     main="Effect of Time")
loogit.l <- pred$fit-1.96*pred$se.fit
loogit.u <- pred$fit+1.96*pred$se.fit
lines(x.star$Time,loogit.u,lty=2,col="aquamarine4")
lines(x.star$Time,loogit.l,lty=2,col="coral4")
legend("bottomleft",legend=c("Estimate","Upper","Lower"),
       col=c("midnightblue","aquamarine4"
             ,"coral4"),lty=c(1,2,2))


#Ho: Aggressive strategy has no effect
#Ho: Time has no effect
summary(out.lol)

#LRT X^2
reduced.lol <- glm(Win~Kills+Deaths+Assists,data=players.train,family="binomial")
anova(reduced.lol,out.lol,test="Chisq") #same p-value basically so probably just share wald test
# We reject the hypothesis that Time has no effect on the probability of winning (pval<0.00001)


# Ambition and Ambition
predict(out.lol,newdata=data.frame(Kills=2,Deaths=3,Assists=8,Time=40),type="response")
predict(out.lol,newdata=data.frame(Kills=2,Deaths=2,Assists=14,Time=40),type="response")
#95% CI's
logit.Ambition <- predict(out.lol,newdata=data.frame(Kills=2,Deaths=3,Assists=8,Time=40),type="link",se.fit=TRUE)
logit.L <- logit.Ambition$fit - 1.96*logit.Ambition$se.fit
logit.U <- logit.Ambition$fit + 1.96*logit.Ambition$se.fit
phat.L.Ambition <- 1/(1+exp(-logit.L))
phat.U.Ambition <- 1/(1+exp(-logit.U))
c(phat.L.Ambition,phat.U.Ambition)


logit.Ambition <- predict(out.lol,newdata=data.frame(Kills=2,Deaths=2,Assists=14,Time=40),type="link",se.fit=TRUE)
logit.L <- logit.Ambition$fit - 1.96*logit.Ambition$se.fit
logit.U <- logit.Ambition$fit + 1.96*logit.Ambition$se.fit
phat.L.Ambition <- 1/(1+exp(-logit.L))
phat.U.Ambition <- 1/(1+exp(-logit.U))
c(phat.L.Ambition,phat.U.Ambition)


#ROC curves. judgement on how well the model is classifying. 
    #1) Sensitivity: proportion of true positives. said win, did win. 
    #2) Specificity: proportion of true negatives. said lose, did lose.
# as the threshold increases/ descreases one does better than the other
#y= sensitivity, 1-specificity on the x axis.Best case is high. worst case is y=x
#each dot is a threshold. 
library(ROCR)
train.pred <- prediction(predict(out.lol,type="response"), players.train$Win)
train.perf <-performance(train.pred,measure="tpr",x.measure="fpr") #tpr true positive rate, false positive rate on x
plot(train.perf,xlab="1-specificity",ylab="sensitivity",main="ROC Curve")
abline(0,1,col="indianred")

#AUC: Area Under Curve. Bad is .5, good is close to 1

performance(train.pred,measure="auc")

#overlay the training ROC curve (out of sample validation)
test.pred <- prediction(predict(out.lol,newdata=players.test,type="response"), players.test$Win)
test.perf <-performance(train.pred,measure="tpr",x.measure="fpr") #tpr true positive rate, false positive rate on x
plot(test.perf,col="turquoise",add=TRUE)

performance(test.pred,measure="auc")


#Our research task was the evaluate the value of skills in LOL. As we had a lot of informative
# data about that task our analysis was very strong showing which skills were most valuable. 

#Weakness: We can't ever guarantee that a player will win a match based on their stats, we can only give
# a very informed liklihood.

#Challenge:: Predict the probability of winning based on number of blocks, steals, assits of the starting centers of NBA
# teams.

