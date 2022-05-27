#1. Load the data
#2. Split data into test and train
#3. Build formula
#4. Build Model based on derived formula
#5. Prediction according to the model



setwd("/home/faculty/Documents/Krupa/Subjects/DataScience-I/data")
load("psub.RData")
summary(psub$INTP)
dtrain <- subset(psub,ORIGRANDGROUP >= 500)
dtest <- subset(psub,ORIGRANDGROUP < 500)

formulaForModel<-log(PINCP,base=10) ~ AGEP + SEX + COW + SCHL

model <- lm(formulaForModel,data=dtrain)


dtest$predLogPINCP <- predict(model,newdata=dtest)
dtrain$predLogPINCP <- predict(model,newdata=dtrain)

#Plotting residuals income as a function of predicted log income
library(ggplot2)
ggplot(data=dtest,aes(x=predLogPINCP,
                      y=predLogPINCP-log(PINCP,base=10))) +
  geom_point(alpha=0.2,color="black") +
  geom_smooth(aes(x=predLogPINCP,
                  y=predLogPINCP-log(PINCP,base=10)),
              color="black")

#Computing R-squared
rsq <- function(y,f) { 1 - sum((y-f)^2)/sum((y-mean(y))^2) }
rsq(log(dtrain$PINCP,base=10),predict(model,newdata=dtrain))
rsq(log(dtest$PINCP,base=10),predict(model,newdata=dtest))

#Calculating root mean square error
rmse <- function(y, f) { sqrt(mean( (y-f)^2 )) }
rmse(log(dtrain$PINCP,base=10),predict(model,newdata=dtrain))
rmse(log(dtest$PINCP,base=10),predict(model,newdata=dtest))

#Summary of model
#First part: model call
#Second part: residual summary
#Third part: coefficients
#Fourth part: model quality summary
summary(model)


#LOGISTIC REGRESSION

load("NatalRiskData.rData")
train <- sdata[sdata$ORIGRANDGROUP<=5,]
test <- sdata[sdata$ORIGRANDGROUP>5,]

complications <- c("ULD_MECO","ULD_PRECIP","ULD_BREECH")
riskfactors <- c("URF_DIAB", "URF_CHYPER", "URF_PHYPER",
                 "URF_ECLAM")
y <- "atRisk"
x <- c("PWGT",
       "UPREVIS",
       "CIG_REC",
       "GESTREC3",
       "DPLURAL",
       complications,
       riskfactors)



fmla <- paste(y, paste(x, collapse="+"), sep="~")




model <- glm(fmla, data=train, family=binomial(link="logit"))



train$pred <- predict(model, newdata=train, type="response")
test$pred <- predict(model, newdata=test, type="response")



library(ggplot2)
ggplot(train, aes(x=pred, color=atRisk, linetype=atRisk)) +
  geom_density()
ctab.test <- table(pred=test$pred>0.02, atRisk=test$atRisk)
precision <- ctab.test[2,2]/sum(ctab.test[2,])
recall <- ctab.test[2,2]/sum(ctab.test[,2])
enrich <- precision/mean(as.numeric(test$atRisk))
coefficients(model)
summary(model)
