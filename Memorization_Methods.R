setwd("/home/faculty/Documents/Krupa/Subjects/DataScience-I/data")

read.table("/home/faculty/Documents/Krupa/Subjects/DataScience-I/data/orange_small_train.data.gz",hearder=F)
getwd()
## Preparing the KDD data for analysis 

#   Read the file of independent variables. All data from 
#   https://github.com/WinVector/zmPDSwR/tree/master/KDD2009. 
#   Treat both NA and the empty string as missing  data. 
d <- read.table('orange_small_train.data.gz',  	
                header=T,
                sep='\t',
                na.strings=c('NA','')) 	 

#   Read churn dependent variable. 
churn <- read.table('orange_small_train_churn.labels.txt',
                    header=F,sep='\t') 	 

#   Add churn as a new column. 
d$churn <- churn$V1 
appetency <- read.table('orange_small_train_appetency.labels.txt',
                        header=F,sep='\t')

#   Add appetency as a new column. 
d$appetency <- appetency$V1 	 
upselling <- read.table('orange_small_train_upselling.labels.txt',
                        header=F,sep='\t')

#   Add upselling as a new column. 
d$upselling <- upselling$V1 	 

#   By setting the seed to the pseudo-random number generator, we make our work reproducible: 
#   someone redoing it will see the exact same results.
set.seed(729375) 	 
d$rgroup <- runif(dim(d)[[1]])

#   Split data into train and test subsets.     
dTrainAll <- subset(d,rgroup<=0.9)
dTest <- subset(d,rgroup>0.9) 	 
outcomes=c('churn','appetency','upselling')
vars <- setdiff(colnames(dTrainAll),
                c(outcomes,'rgroup'))

#   Identify which features are categorical variables. 
catVars <- vars[sapply(dTrainAll[,vars],class) %in%
                  c('factor','character')] 	 

#   Identify which features are numeric variables. 
numericVars <- vars[sapply(dTrainAll[,vars],class) %in%
                      c('numeric','integer')] 	

#   Remove unneeded objects from workspace. 
rm(list=c('d','churn','appetency','upselling')) 	 

#   Choose which outcome to model (churn). 
outcome <- 'churn' 	 

#   Choose which outcome is considered positive. 
pos <- '1' 	 

#   Further split training data into training and calibration. 
useForCal <- rbinom(n=dim(dTrainAll)[[1]],size=1,prob=0.1)>0 	 
dCal <- subset(dTrainAll,useForCal)
dTrain <- subset(dTrainAll,!useForCal)


## Plotting churn grouped by variable 218 levels 

#   Tabulate levels of Var218. 
#   Tabulate levels of churn outcome.    
#   Include NA values in tabulation. 
table218 <- table(
  Var218=dTrain[,'Var218'], 	
  churn=dTrain[,outcome], 	
  useNA='ifany') 

print(table218)

## Churn rates grouped by variable 218 codes 
print(table218[,2]/(table218[,1]+table218[,2]))

## Function to build single-variable models for categorical variables 


#   Given a vector of training outcomes (outCol), 
#   a categorical training variable (varCol), and a 
#   prediction variable (appCol), use outCol and 
#   varCol to build a single-variable model and then 
#   apply the model to appCol to get new 
#   predictions.
  mkPredC <- function(outCol,varCol,appCol) {
    
  #   Get stats on how often outcome is positive during training. 	 
  pPos <- sum(outCol==pos)/length(outCol) 
  naTab <- table(as.factor(outCol[is.na(varCol)]))
  
  #   Get stats on how often outcome is positive for NA values of variable during training. 
  pPosWna <- (naTab/sum(naTab))[as.character(pos)] 	
  vTab <- table(as.factor(outCol),varCol)
  
  ##   Get stats on how often outcome is positive, conditioned on levels of 
  #training variable(varCol). 
  pPosWv <- (vTab[as.character(pos),]+1.0e-3*pPos)/(colSums(vTab)+1.0e-3) 
  
  #   Make predictions by looking up levels of appCol. 
  pred <- pPosWv[appCol] 
  
  # Add in predictions for NA levels of appCol. 
  pred[is.na(appCol)] <- pPosWna
  
  #   Add in predictions for levels of appCol that weren’t known during training. 
  pred[is.na(pred)] <- pPos  	
  pred 
}

## Title: Applying single-categorical variable models to all of our datasets 

for(v in catVars) {
  
  pi <- paste('pred',v,sep='')
  dTrain[,pi] <- mkPredC(dTrain[,outcome],dTrain[,v],dTrain[,v])
  dCal[,pi] <- mkPredC(dTrain[,outcome],dTrain[,v],dCal[,v])
  dTest[,pi] <- mkPredC(dTrain[,outcome],dTrain[,v],dTest[,v])
}

  ##Scoring categorical variables by AUC 
  library('ROCR')
  
  calcAUC <- function(predcol,outcol) {
    perf <- performance(prediction(predcol,outcol==pos),'auc')
    as.numeric(perf@y.values)
  }
  
  for(v in catVars) {
    pi <- paste('pred',v,sep='')
    aucTrain <- calcAUC(dTrain[,pi],dTrain[,outcome])
    if(aucTrain>=0.8) {
      aucCal <- calcAUC(dCal[,pi],dCal[,outcome])
      print(sprintf("%s, trainAUC: %4.3f calibrationAUC: %4.3f",
                    pi,aucTrain,aucCal))
    }
  }
  
  ## Scoring numeric variables by AUC 
  
  mkPredN <- function(outCol,varCol,appCol) {
    cuts <- unique(as.numeric(quantile(varCol,
                                       probs=seq(0, 1, 0.1),na.rm=T)))
    varC <- cut(varCol,cuts)
    appC <- cut(appCol,cuts)
    mkPredC(outCol,varC,appC)
  }
  
  for(v in numericVars) {
    pi <- paste('pred',v,sep='')
    dTrain[,pi] <- mkPredN(dTrain[,outcome],dTrain[,v],dTrain[,v])
    dTest[,pi] <- mkPredN(dTrain[,outcome],dTrain[,v],dTest[,v])
    dCal[,pi] <- mkPredN(dTrain[,outcome],dTrain[,v],dCal[,v])
    aucTrain <- calcAUC(dTrain[,pi],dTrain[,outcome])
    if(aucTrain>=0.55) {
      aucCal <- calcAUC(dCal[,pi],dCal[,outcome])
      print(sprintf("%s, trainAUC: %4.3f calibrationAUC: %4.3f",
                    pi,aucTrain,aucCal))
    }
  }
  
  ## Plotting variable performance 
  
  library('ggplot2')
  ggplot(data=dCal) +
    geom_density(aes(x=predVar126,color=as.factor(churn)))
  
  ##Running a repeated cross-validation experiment 
  
  var <- 'Var217'
  aucs <- rep(0,100)
  for(rep in 1:length(aucs)) {
    useForCalRep <- rbinom(n=dim(dTrainAll)[[1]],size=1,prob=0.1)>0
    predRep <- mkPredC(dTrainAll[!useForCalRep,outcome],
                       dTrainAll[!useForCalRep,var],
                       dTrainAll[useForCalRep,var])
    aucs[rep] <- calcAUC(predRep,dTrainAll[useForCalRep,outcome])
  }
  mean(aucs)

  sd(aucs)

  ## Empirically cross-validating performance

  fCross <- function() {
    useForCalRep <- rbinom(n=dim(dTrainAll)[[1]],size=1,prob=0.1)>0
    predRep <- mkPredC(dTrainAll[!useForCalRep,outcome],
                       dTrainAll[!useForCalRep,var],
                       dTrainAll[useForCalRep,var])
    calcAUC(predRep,dTrainAll[useForCalRep,outcome])
  }
  aucs <- replicate(100,fCross())


  # ### Basic variable selection 
  # #   Define a convenience function to compute log likelihood.   
  logLikelyhood <- function(outCol,predCol) {
    sum(ifelse(outCol==pos,log(predCol),log(1-predCol)))
  }

  selVars <- c()
  minStep <- 5
  baseRateCheck <- logLikelyhood(dCal[,outcome],
                                 sum(dCal[,outcome]==pos)/length(dCal[,outcome]))

  #   Run through categorical variables and pick based on a deviance improvement
  #   (related to difference in log likelihoods; see chapter 3).
  for(v in catVars) {  	# Note: 2
    pi <- paste('pred',v,sep='')
    liCheck <- 2*((logLikelyhood(dCal[,outcome],dCal[,pi]) -
                     baseRateCheck))
    if(liCheck>minStep) {
      print(sprintf("%s, calibrationScore: %g",
                    pi,liCheck))
      selVars <- c(selVars,pi)
    }
  }


  # #   Run through numeric variables and pick based on a deviance improvement. 
  for(v in numericVars) { 	# Note: 3
    pi <- paste('pred',v,sep='')
    liCheck <- 2*((logLikelyhood(dCal[,outcome],dCal[,pi]) -
                     baseRateCheck))
    if(liCheck>=minStep) {
      print(sprintf("%s, calibrationScore: %g",
                    pi,liCheck))
      selVars <- c(selVars,pi)
    }
  }
  # 
  ## Building a bad decision tree

  library('rpart')

  fV <- paste(outcome,'>0 ~ ',
              paste(c(catVars,numericVars),collapse=' + '),sep='')

  tmodel <- rpart(fV,data=dTrain)

  print(calcAUC(predict(tmodel,newdata=dTrain),dTrain[,outcome]))
  print(calcAUC(predict(tmodel,newdata=dTest),dTest[,outcome]))

  print(calcAUC(predict(tmodel,newdata=dCal),dCal[,outcome]))

  ## Building another bad decision tree

  tVars <- paste('pred',c(catVars,numericVars),sep='')

  fV2 <- paste(outcome,'>0 ~ ',paste(tVars,collapse=' + '),sep='')

  tmodel <- rpart(fV2,data=dTrain)

  print(calcAUC(predict(tmodel,newdata=dTrain),dTrain[,outcome]))

  print(calcAUC(predict(tmodel,newdata=dTest),dTest[,outcome]))

  print(calcAUC(predict(tmodel,newdata=dCal),dCal[,outcome]))


  ## Building yet another bad decision tree

  tmodel <- rpart(fV2,data=dTrain,
                  control=rpart.control(cp=0.001,minsplit=1000,
                                        minbucket=1000,maxdepth=5)
  )
  print(calcAUC(predict(tmodel,newdata=dTrain),dTrain[,outcome]))

  print(calcAUC(predict(tmodel,newdata=dTest),dTest[,outcome]))

  print(calcAUC(predict(tmodel,newdata=dCal),dCal[,outcome]))
  
  
  ## Building a better decision tree 
  
  f <- paste(outcome,'>0 ~ ',paste(selVars,collapse=' + '),sep='')
  tmodel <- rpart(f,data=dTrain,
                  control=rpart.control(cp=0.001,minsplit=1000,
                                        minbucket=1000,maxdepth=5)
  )
  
  print(calcAUC(predict(tmodel,newdata=dTrain),dTrain[,outcome]))
  
  print(calcAUC(predict(tmodel,newdata=dTest),dTest[,outcome]))
  
  print(calcAUC(predict(tmodel,newdata=dCal),dCal[,outcome]))
  
  ## Printing the decision tree 
  
  print(tmodel)
  
  ## Plotting the decision tree 
  
  par(cex=0.7)
  plot(tmodel)
  text(tmodel)
  
  ## Title: Running k-nearest neighbors 
  
  library('class')
  
  nK <- 200
  
  #   Build a data frame with only the variables we wish to use for classification. 
  knnTrain <- dTrain[,selVars]  	
  
  #   Build a vector with the known training outcomes.  
  # 'churn' ->1 'apptency'->2 'upselling'->3
  knnCl <- dTrain[,outcome]==pos 	
 
  
  #   Bind the knn() training function with our data in a new function. 
  #   Convert knn’s unfortunate convention of calculating probability as “proportion of the 
  #   votes for the winning class” into the more useful “calculated probability of being a positive 
  #   example.” 
  knnPred <- function(df) { 
    knnDecision <- knn(knnTrain,df,knnCl,k=nK,prob=T)
    ifelse(knnDecision==TRUE, 	# Note: 4 
           attributes(knnDecision)$prob,
           1-(attributes(knnDecision)$prob))
  }
  
  knnPredCall<-knnPred(dTrain[,selVars])
  calcAUC(knnPredCall)
  
  
  
  print(calcAUC(knnPred(dTrain[,selVars]),dTrain[,outcome]))
  
  print(calcAUC(knnPred(dCal[,selVars]),dCal[,outcome]))
  
  print(calcAUC(knnPred(dTest[,selVars]),dTest[,outcome]))
  
  
  ## Platting 200-nearest neighbor performance 
  
  dCal$kpred <- knnPred(dCal[,selVars])
  ggplot(data=dCal) +   geom_density(aes(x=kpred, color=as.factor(churn),linetype=as.factor(churn)))
  
  ## Title: Plotting the receiver operating characteristic curve 
  
  plotROC <- function(predcol,outcol) {
    perf <- performance(prediction(predcol,outcol==pos),'tpr','fpr')
    pf <- data.frame(
      FalsePositiveRate=perf@x.values[[1]],
      TruePositiveRate=perf@y.values[[1]])
    ggplot() +
      geom_line(data=pf,aes(x=FalsePositiveRate,y=TruePositiveRate)) +
      geom_line(aes(x=c(0,1),y=c(0,1)))
  }                                                                      
  print(plotROC(knnPred(dTest[,selVars]),dTest[,outcome]))
  
  
  ## Plotting the performance of a logistic regression model 
  
  gmodel <- glm(as.formula(f),data=dTrain,family=binomial(link='logit'))
  
  print(calcAUC(predict(gmodel,newdata=dTrain),dTrain[,outcome]))
  
  print(calcAUC(predict(gmodel,newdata=dTest),dTest[,outcome]))
  
  print(calcAUC(predict(gmodel,newdata=dCal),dCal[,outcome]))
  
  
  
  ## Building, applying, and evaluating a Naive Bayes model 
  
  pPos <- sum(dTrain[,outcome]==pos)/length(dTrain[,outcome])
  
  nBayes <- function(pPos,pf) { 	# Note: 1 
    pNeg <- 1 - pPos
    smoothingEpsilon <- 1.0e-5
    scorePos <- log(pPos + smoothingEpsilon) + 
      rowSums(log(pf/pPos + smoothingEpsilon)) 	# Note: 2 
    scoreNeg <- log(pNeg + smoothingEpsilon) +
      rowSums(log((1-pf)/(1-pPos) + smoothingEpsilon)) 	# Note: 3 
    m <- pmax(scorePos,scoreNeg)
    expScorePos <- exp(scorePos-m)
    expScoreNeg <- exp(scoreNeg-m) 	# Note: 4 
    expScorePos/(expScorePos+expScoreNeg) 	# Note: 5 
  }

  
  
    
  pVars <- paste('pred',c(numericVars,catVars),sep='')
  
  dTrain$nbpredl <- nBayes(pPos,dTrain[,pVars])
  
  dCal$nbpredl <- nBayes(pPos,dCal[,pVars])
  
  dTest$nbpredl <- nBayes(pPos,dTest[,pVars]) 	# Note: 6 
  
  print(calcAUC(dTrain$nbpredl,dTrain[,outcome]))
  
  print(calcAUC(dCal$nbpredl,dCal[,outcome]))
  
  print(calcAUC(dTest$nbpredl,dTest[,outcome]))
  
  
  # Using a Naive Bayes package
  
  library('e1071')
  
  lVars <- c(catVars,numericVars)
  
  ff <- paste('as.factor(',outcome,'>0) ~ ', paste(lVars,collapse=' + '), sep='')
  
  nbmodel <- naiveBayes(as.formula(ff),data=dTrain)
  
  dTrain$nbpred <- predict(nbmodel,newdata=dTrain,type='raw')[,'TRUE']
  
  dCal$nbpred <- predict(nbmodel,newdata=dCal,type='raw')[,'TRUE']
  
  dTest$nbpred <- predict(nbmodel,newdata=dTest,type='raw')[,'TRUE']
  
  calcAUC(dTrain$nbpred,dTrain[,outcome])
  
  calcAUC(dCal$nbpred,dCal[,outcome])
  
  calcAUC(dTest$nbpred,dTest[,outcome])
  