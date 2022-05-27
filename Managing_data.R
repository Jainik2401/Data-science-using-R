#reading data
setwd("/home/faculty/Documents/Krupa/Subjects/DataScience-I/data")
custdata <- read.table('custdata.tsv',header=TRUE,sep='\t')

#Checking NAs
summary(custdata[is.na(custdata$housing.type), 	
                 c("recent.move","num.vehicles")])

#Remapping NA to a level
custdata$is.employed.fix <- ifelse(is.na(custdata$is.employed),  	 
                                   "missing",                    	 
                                   ifelse(custdata$is.employed==T, 	 
                                          "employed",
                                          "not employed"))  	 

summary(as.factor(custdata$is.employed.fix))

custdata$is.employed.fix <- ifelse(is.na(custdata$is.employed),
                                   "not in active workforce",
                                   ifelse(custdata$is.employed==T,
                                          "employed",
                                          "not employed"))
#checking for missing values
summary(custdata$Income)

#randomly missing
meanIncome <- mean(custdata$income, na.rm=T) 

Income.fix <- ifelse(custdata$income<=0,
                     meanIncome,
                     custdata$income)

Income.fix <- ifelse(is.na(custdata$income),
                     meanIncome,
                     custdata$income)

print(summary(Income.fix))

#numeric data breaks
breaks1 <-c(0, 20000, 50000, 100000, 250000, 1000000)           

Income.groups <- cut(custdata$income,
                     breaks=breaks1, include.lowest=T)  	 
cut()
summary(Income.groups)

Income.groups <- as.character(Income.groups)                   	 

Income.groups <- ifelse(is.na(Income.groups),                  	
                        "no income", Income.groups)

summary(as.factor(Income.groups))

#replacing missing values
missingIncome <- is.na(custdata$Income)  	
Income.fix <- ifelse(is.na(custdata$Income), 0, custdata$Income) 


#Normalizing income by state
medianincome <- aggregate(income~state.of.res,custdata,FUN=median)
colnames(medianincome) <- c('State','Median.Income')
summary(medianincome)

custdata <- merge(custdata, medianincome, by.x="state.of.res", by.y="State")  	

summary(custdata[,c("state.of.res", "income", "Median.Income")])

custdata$income.norm <- with(custdata, income/Median.Income) 	
summary(custdata$income.norm)

custdata$income.lt.20K <- custdata$income < 20000
summary(custdata$income.lt.20K)

#ranges
brks <- c(0, 25, 65, Inf)  	
custdata$age.range <- cut(custdata$age,
                          breaks=brks, include.lowest=T) 	
summary(custdata$age.range) 
#mean age
summary(custdata$age)
meanage <- mean(custdata$age)
custdata$age.normalized <- custdata$age/meanage
summary(custdata$age.normalized)

#undersatnding age data
summary(custdata$age)
meanage <- mean(custdata$age)  	
stdage <- sd(custdata$age)     	
meanage
stdage
custdata$age.normalized <- (custdata$age-meanage)/stdage 	
summary(custdata$age.normalized)

#test and train-random
custdata$gp <- runif(dim(custdata)[1])  	
testSet <- subset(custdata, custdata$gp <= 0.1) 	
trainingSet <- subset(custdata, custdata$gp > 0.1) 	
dim(testSet)[1]
dim(trainingSet)[1]

#ensuring test and train splits
hh <- unique(hhdata$household_id) 
households <- data.frame(household_id = hh, gp = runif(length(hh))) 
hhdata <- merge(hhdata, households, by="household_id") 	