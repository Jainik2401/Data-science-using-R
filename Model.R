#Logistic Regression
setwd("/home/faculty/Documents/Krupa/Subjects/DataScience-I/data")
spamD <- read.table('spamD.tsv',header=T,sep='\t')
spamTrain <- subset(spamD,spamD$rgroup>=10)
spamTest <- subset(spamD,spamD$rgroup<10)

#test<-setdiff(list('A','B','C','D'),list('A'))
spamVars <- setdiff(colnames(spamD),list('rgroup','spam'))
spamFormula <- as.formula(paste('spam=="spam"', paste(spamVars,collapse=' + '),sep=' ~ '))

#formula<-as.formula(spam~word.freq.make + word.freq.address)

#glm() <- logistic regression{answers 1/0}
spamModel <- glm(spamFormula,family=binomial(link='logit'), data=spamTrain)

#formula->model->prediction

spamTrain$pred <- predict(spamModel,newdata=spamTrain,type='response')
spamTest$pred <- predict(spamModel,newdata=spamTest, type='response')
print(table(spamTest$pred))


with(spamTest,table(y=spam,PredictionForMail=pred>0.7))
print(with(spamTest,table(y=spam,glmPred=pred>0.5)))

#Classification

sample <- spamTest[c(7,35,224,327),c('spam','pred')]
print(sample)

#Confusion Matrix
cM <- table(category=spamTest$spam,prediction=spamTest$pred>0.5)
print(cM)

#Inserting new data
t <- as.table(matrix(data=c(288-1,17,1,13882-17),nrow=2,ncol=2))
rownames(t) <- rownames(cM)
colnames(t) <- colnames(cM)
print(t)

#Evaluating scoring model
d <- data.frame(y=(1:10)^2,x=1:10)
model <- lm(y~x,data=d)
d$prediction <- predict(model,newdata=d)
library('ggplot2')
ggplot(data=d) + geom_point(aes(x=x,y=y)) +
  geom_line(aes(x=x,y=prediction),color='blue') +
  geom_segment(aes(x=x,y=prediction,yend=y,xend=x)) +
  scale_y_continuous('')

#Evaluating probability model
ggplot(data=spamTest) +
  geom_density(aes(x=pred,color=spam,linetype=spam))

#ROC Curve
library('ROCR')
eval <- prediction(spamTest$pred,spamTest$spam)

plot(performance(eval,"tpr","fpr"))
print(attributes(performance(eval,'auc'))$y.values[[1]])

#Log likelihood
sum(ifelse(spamTest$spam=='spam',log(spamTest$pred),log(1-spamTest$pred)))

sum(ifelse(spamTest$spam=='spam',log(spamTest$pred),log(1-spamTest$pred)))/dim(spamTest)[[1]]

#Cluster

set.seed(32297)
d <- data.frame(x=runif(100),y=runif(100))
clus <- kmeans(d,centers=5)
d$cluster <- clus$cluster

library('ggplot2'); 
library('grDevices')
h <- do.call(rbind,
             lapply(unique(clus$cluster),
                    function(c) { f <- subset(d,cluster==c); f[chull(f),]}))
ggplot() +
  geom_text(data=d,aes(label=cluster,x=x,y=y,
                       color=cluster),size=3)  +
  geom_polygon(data=h,aes(x=x,y=y,group=cluster,fill=as.factor(cluster)),
               alpha=0.4,linetype=0) +
  theme(legend.position = "none")

table(d$cluster)

library('reshape2')

n <- nrow(d)

pairs <- merge(1:n, 1:n, all=TRUE)
names(pairs) <- c("a",  "b")
pairs$ca <- d[pairs$a, 'cluster'][[1]]
pairs$cb <- d[pairs$b, 'cluster'][[1]]
pairs$dist <- sqrt((d[pairs$a, 'x']-d[pairs$b, 'x'])^2 + (d[pairs$a, 'y']-d[pairs$b, 'y'])^2)[[1]]

dcast(pairs, ca~cb, value.var='dist', mean)
