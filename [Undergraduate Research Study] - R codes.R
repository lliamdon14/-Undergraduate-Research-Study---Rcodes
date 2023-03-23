#libraries
library(readr)
library(car)
library(ltm)
library(lsr)
library(dplyr)
library(sf)
library(DMwR)
library(stats)
library(ROCR)
library(pscl)
library(ResourceSelection)
library(MASS)
library(caret)


#import dataset
library(readxl)
data <- read_excel("D:/UPLB (ogranized files)/UPLB 5th year 22-23/Data Analysis/Survey Form (Responses).xlsx", 
                   sheet = "non_coded")
View(data)
#non-coded data was used

#-----------------------------------------------TRY (WITHOUT SMOTE)-----------------
full_model <- glm(D1.A~factor(A2)+A3+A4.1+A5.1+A6.1+sum_B1B3+sum_B4B6+sum_B7B9+sum_B10B12+D.2.+D.3.+D.4.+factor(E3.)+sum_E4E6+sum_E7E9,family=binomial(logit), data=data)
summary(full_model)
pR2(full_model)
vif(full_model)

#variable sum_B4B6 was removed since it is highly correlated to other predictors
full_model2 <- glm(D1.A~factor(A2)+A3+A4.1+A5.1+A6.1+sum_B1B3+sum_B7B9+sum_B10B12+D.2.+D.3.+D.4.+factor(E3.)+sum_E4E6+sum_E7E9,family=binomial(logit), data=data)
summary(full_model2)
pR2(full_model2)
vif(full_model2)

#Stepwise 
#The goal is to reduce the number of predictors
library(MASS)
library(caret)
step.model <- full_model2 %>% stepAIC(trace = FALSE)
formula(step.model)
summary(step.model)
exp(coef(step.model))

#final model
final_model <- glm(D1.A~factor(E3.)+D.2.,data=data)
summary(final_model)
pR2(final_model)
exp(coef(final_model))


#model overall fit
with(final_model, null.deviance - deviance)
with(final_model, df.null - df.residual)
logLik(final_model)
with(final_model,pchisq(null.deviance-deviance,df.null-df.residual,lower.tail=FALSE)) #p-value
exp(coef(final_model))


#Hosmer-Lemeshow test
modeldata <- model.frame(final_model)
hoslem.test(modeldata$D1.A,fitted(final_model),g=10)


#-------------------------------------------------------------------------

#Importing the coded data for SMOTE
library(readxl)
data_coded <- read_excel("D:/UPLB (ogranized files)/UPLB 5th year 22-23/Data Analysis/Survey Form (Responses).xlsx", 
                         sheet = "coded")
View(data_coded)


table(data_coded$D1.A)

#SMOTE
train_smote<-as.data.frame(data_coded)
train_smote$D1.A <- as.factor(train_smote$D1.A)
trainSmote <- SMOTE(D1.A ~ ., train_smote, perc.over = 100, perc.under = 200)
trainSmote$D1.A <- as.numeric(trainSmote$D1.A)
trainSmote <- floor(trainSmote)
table(trainSmote$D1.A)
View(trainSmote)
training <- trainSmote


length(trainSmote$D1.A)
typeof(trainSmote$D1.A)

#write.csv(training,file="training.csv",row.names = FALSE)


#import the recoded smote dataset
library(readr)
training <- read_csv("training.csv")
View(training)

training$D1.A[training$D1.A==1] <- 0
training$D1.A[training$D1.A==2] <- 1
View(training)

#FIRST VIF TESTING
full_model <- glm(D1.A~A2+A3+A4.1+A5.1+A6.1+factor(`B1-B3`)+factor(`B4-B6`)+factor(`B7-B9`)+factor(`B10-12`)+factor(`D.2.`)+factor(`D.3.`)+factor(`D.4.`),family=binomial(logit), data=training)
summary(full_model)
pR2(full_model)
vif(full_model)

#Stepwise 
#The goal is to reduce the number of predictors
library(MASS)
library(caret)
step.model <- full_model %>% stepAIC(trace = FALSE)
formula(step.model)
summary(step.model)
exp(coef(step.model))

#Multicollinearity testing
vif(step.model)
pR2(step.model)

#Dropping the highest VIF --> variable D.3.
reduced_model1 <- glm(D1.A~A4.1+A5.1+A6.1+factor(`B4-B6`)+factor(`B7-B9`)+factor(D.2.),family=binomial(logit), data=training)
formula(reduced_model1)
vif(reduced_model1)
summary(reduced_model1)

#checking the significance of multilevel predictors
summary(anova(reduced_model1,test="Chisq"))
lr.anova <- anova(reduced_model1,test="Chisq")
lr.anova

#stepwise of the reduced model
step.model2 <- reduced_model1 %>% stepAIC(trace = FALSE)
formula(step.model2)
summary(step.model2)
exp(coef(step.model2))
vif(step.model2)
pR2(step.model2)

glm.1 <- step.model2

#Checking the significance of predictors 
summary(aov(glm.1))



#model overall fit
with(glm.1, null.deviance - deviance)
with(glm.1, df.null - df.residual)
logLik(glm.1)
with(glm.1,pchisq(null.deviance-deviance,df.null-df.residual,lower.tail=FALSE)) #p-value
exp(coef(glm.1))


#Hosmer-Lemeshow test
modeldata <- model.frame(glm.1)
hoslem.test(modeldata$D1.A,fitted(glm.1),g=10)




try_model <- glm(D1.A~factor(`B7-B9`),family=binomial(logit), data=training)
summary(try_model)



#Dropping the highest VIF --> variable D.3.
try_model1 <- glm(D1.A~factor(`B4-B6`)+factor(`B7-B9`)+factor(D.2.),family=binomial(logit), data=training)
formula(try_model1 )
vif(try_model1 )
pR2(try_model1 )
summary(try_model1 )


  


table(training$`B4-B6`)
table(training$`B7-B9`)

table(training$D.2.)



