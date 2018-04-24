source("http://grimshawville.byu.edu/STEMgetdata.R")
dim(STEM)
tail(STEM)
#EDA
par(mfrow=c(3,2))
boxplot(STEM$prevCalc~STEM$y,main="PrevCalc")
boxplot(STEM$nST~STEM$y,main="nST")
boxplot(STEM$newMJ~STEM$y,main="Major")
boxplot(STEM$new.teach~STEM$y,main="Teachers")
boxplot(STEM$new.stu~STEM$y,main="Student")
boxplot(STEM$gender~STEM$y,main="Gender")

#these are the categorical
table(STEM$y, STEM$gender)
table(STEM$y, STEM$prevCalc)
table(STEM$y, STEM$newMJ)

with(STEM,table(y,prevCalc))
with(STEM,table(y,newMJ))
with(STEM,table(y,gender))

#Response:switchers: 0 stay, 1 leave
#explanatory: GENDER (most important),
#adjusting for (confounding): nST, new.teach, new.stu,prevCalc $ ,newMJ $

#We need to transform categorical explanatory variables into R class factors with a comparison case. 
STEM$gender <- factor(STEM$gender)
STEM$gender <- relevel(STEM$gender,ref="1") # we are comparing women TO men. R chooses one anyways though
STEM$prevCalc <- factor(STEM$prevCalc) # so we don't really need it there.
STEM$prevCalc <- relevel(STEM$prevCalc,ref="1")
STEM$newMJ <- factor(STEM$newMJ)
STEM$newMJ <- relevel(STEM$newMJ,ref="1")

#MODEL
#logit(y=1)== logit(switchers)
# = beta0 + beta1 nST + beta2 new.teach + beta3 new.stu + gender_i + prevCalc_j + newMJ_k 

# we use the _i to show that there are just a couple effects

out.stem <- glm(y~nST+new.teach+new.stu+gender+prevCalc+newMJ,data=STEM,family="binomial")
summary(out.stem)
#positive gender2 estimate means that women are more likely to leave holding all else constant.
exp(coef(out.stem)[-1]) 

#interpretation for non-statistician
#We estimate that women are 1.428 times more likely to switch out of the calculus sequence than
# men all else held constant.

#For our picture we need a model for men and a model for women. they'll be curved. nST on x axis. 
# all else held at the "best." Most normal. y axis is p(switch)
#men
par(mfrow=c(1,1))
x.star <- data.frame(gender="1",nST=seq(2,99,length.out = 101),
                     new.teach=6,new.stu= 6,prevCalc="1",newMJ="1")
plot(x.star$nST,predict(out.stem,newdata=x.star,type="response"),type="l",
     xlab="Standardized Test Percentile",
     ylab="P(Switch from Calc Seq)",ylim=c(0,.25))
#women
x.star <- data.frame(gender="2",nST=seq(2,99,length.out = 101),
                     new.teach=6,new.stu= 6,prevCalc="1",newMJ="1")
lines(x.star$nST,predict(out.stem,newdata=x.star,type="response"),col="indianred")
legend("topright",legend=c("Men","Women"),col=c("black","indianred"),lty=c(1,1))
# do confidence intervals here. You gotta spell them out. create in logit space then untransform

# is it statistically significant difference between genders holding all else constant?
#95% CI or z-test or ChiSq Test
exp(confint(out.stem)[-1,])
# as the confidence interval does not contain 1, we conclude that there is a significant difference between
#genders

# z-test
summary(out.stem)
# With a p-value of .00332 we conclude that there is a statistically significant difference between 
#genders. zstat=2.936


#chisq
reduced1.stem <- glm(y~nST+new.teach+new.stu+prevCalc+newMJ,data=STEM,family="binomial")
anova(reduced1.stem,out.stem,test="Chisq")
# With a p-value of .00323 we conclude that there is a statistically significant difference between 
#genders. Chisq stat=8.67


#is there a calc prep effect?
# don't just look at the coefficients. That's pairwise comparisons
reduced2.stem <- glm(y~nST+new.teach+new.stu+gender+newMJ,data=STEM,family="binomial")
anova(reduced2.stem,out.stem,test="Chisq")
# there is insufficient evidence to suggest that previous calculus experience has an effect on 
# persistence in the calc seq. pval=.446, stat=1.62

# What are the consequences of the classificiation?

#ROC Curve
library(ROCR)
stem.pred <- prediction(predict(out.stem,type="response"),STEM$y)
stem.perf <- performance(stem.pred,measure="tpr",x.measure="fpr")
plot(stem.perf,xlab="1-specificity",ylab="sensitivity",main="ROC Curve") #sensitivity= switchers switched
abline(0,1,col="gray")

#compute auc
performance(stem.pred,measure="auc")
# .760. Our model is fair. A bad model would have a value of .5, while exceptional values are close to 1.
# Our model would improve with more specific data, say just one university

# Our research task was to see if women were in fact more likely to leave the calculus sequence than men
# after having adjusting for other variables. We were able to accomplish this task with the data
# we had access to. We had a lot of variables to adjust for that had an effect on whether students
# switched.

#weakness. Our performance was not very good according to the AUC value computed.

#Challenge: Determine whether or not there was a significant effect of gender on the odds of surviving on
# the titanic after having ajusting for other variables such as age and where their cabin was on the ship.
# data: http://biostat.mc.vanderbilt.edu/wiki/pub/Main/DataSets/titanic3.xls 
