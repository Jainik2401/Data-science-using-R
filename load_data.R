uciCar <- read.table(
  'https://archive.ics.uci.edu/ml/machine-learning-databases/car/car.data',
  sep=',',
  header=F
)
print(uciCar)

colnames(uciCar)<-c('Buying', 'Maintenance', 'Doors', 'Persons', 'lug_boot', 'Safety', 'Rating') 
print(uciCar)
summary(uciCar)

class(uciCar)
help(class(uciCar))
summary(uciCar)
dim(uciCar)


#Using R on less-structured data



mapping <- list(
  'A11'='... <    0 DM',
  'A12'='0 <= ... <  200 DM',
  'A13'='... >= 200 DM /salary assignments for at least 1 year',
  'A14' = 'no checking account',
  'A30'='no credits taken/ all credits paid back duly',
  'A31'='all credits at this bank paid back duly',
  'A32'='existing credits paid back duly till now',
  'A33'='delay in paying off in the past',
  'A34'='critical account',
  'A61'='... <  100 DM',
  'A62'='100 <= ... <  500 DM',
  'A63'='500 <= ... < 1000 DM',
  'A64'='.. >= 1000 DM',
  'A65'='unknown/ no savings account',
  'A71'='unemployed',
  'A73'='1  <= ... < 4 years',
  'A74'='4  <= ... < 7 years',
  'A75'='.. >= 7 years',
  'A91'='male: divorced/separated',
  'A92'='female : divorced/separated/married',
  'A93'='male   : single',
  'A94'='male   : married/widowed',
  'A95'='female : single',
  'A101'='none',
  'A102'='co-applicant',
  'A103'='guarantor',
  'A121'='real estate',
  'A122'='if not A121:building society savings agreement/life insurance',
  'A123' ='if not A121/A122 : car or other, not in attribute 6',
  'A124'='unknown / no property',
  'A141'='bank',
  'A142'='stores',
  'A143'='none',
  'A151'='rent',
  'A152'='own',
  'A153'='for free',
  'A171'='unemployed/ unskilled  - non-resident',
  'A172'='unskilled - resident',
  'A173'='skilled employee / official',
  'A174'='management/ self-employed/highly qualified employee/ officer',
  'A191'='none',
  'A192'='yes, registered under the customers name',
  'A201'='yes',
  'A202'=' no',
  'A40'='car (new)',
  'A41'='car (used)',
  'A42'='furniture/equipment',
  'A43'='radio/television',
  'A44'='domestic appliances',
  'A45'='repairs',
  'A46=education',
  'A47'='vacation',
  'A48'='retraining',
  'A49'='business',
  'A410'='others'
)


for(i in 1:(dim(d))[2]) {
  if(class(d[,i])=='character') {
    d[,i] <- as.factor(as.character(mapping[d[,i]]))
  }
}


d$Good.Loan <- as.factor(ifelse(d$Good.Loan==1,'GoodLoan','BadLoan'))
print(d$Credit.amount)
library(ggplot2)     	 
library(scales)
ggplot(d)
#Histogram
ggplot(d) + geom_histogram(aes(x=credit.amount),binwidth=10) 

table(d$Purpose,d$Good.Loan)
summary(dpus$ORIGRANDGROUP)
write.csv(d,"newGermanData.csv")
getwd()



setwd('/home/faculty/Documents/Krupa/Subjects/DataScience-I/data')
load('phsample.RData')

dpus$SEX = as.factor(ifelse(dpus$SEX==1,'M','F'))
dpus$SEX = relevel(dpus$SEX,'M')

cowmap <- c("Employee of a private for-profit",
             "Private not-for-profit employee",
             "Local government employee",
             "State government employee",
             "Federal government employee",
             "Self-employed not incorporated",
             "Self-employed incorporated")

print(dpus$COW)
dpus$COW = as.factor(cowmap[dpus$COW])
dpus$COW = relevel(dpus$COW,cowmap[1])
 schlmap = c(
   rep("no high school diploma",15),
   "Regular high school diploma",
   "GED or alternative credential",
   "some college credit, no degree",
   "some college credit, no degree",
   "Associate's degree",
   "Bachelor's degree",
   "Master's degree",
   "Professional degree",
   "Doctorate degree")
 dpus$SCHL = as.factor(schlmap[dpus$SCHL])
 dpus$SCHL = relevel(dpus$SCHL,schlmap[1])
 dtrain = subset(dpus,ORIGRANDGROUP >= 500)
 dtest = subset(dpus,ORIGRANDGROUP < 500)
 
