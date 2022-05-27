setwd("/home/faculty/Documents/Krupa/Subjects/DataScience-I/data")
custdata <- read.table('custdata.tsv', header=TRUE,sep='\t')
summary(custdata)

summary(custdata$is.employed)
dim(custdata)

install.packages("ggplot2")
installed.packages()
library(ggplot2)     	 
library(scales)
ggplot(custdata)
#Histogram
ggplot(custdata) + geom_histogram(aes(x=age),binwidth=15, fill="black") 

#Density plot
ggplot(custdata) + geom_density(aes(x=income)) + scale_x_continuous(labels=dollar)

#Log-scaled density plot
ggplot(custdata) + geom_density(aes(x=income)) + scale_x_log10(breaks=c(100,1000,10000,100000), labels=dollar) + annotation_logticks(sides="bt")

#Bar chart
ggplot(custdata) +  geom_bar(aes(x=marital.stat), fill="gray")

#Horizontal bar chart 
ggplot(custdata) +  geom_bar(aes(x=state.of.res), fill="gray") +  coord_flip() + 	theme(axis.text.y=element_text(size=rel(0.8)))

#Sorted bar chart (state of residencec: Alabama=11, Florida=20)
statesums <- table(custdata$state.of.res) 
statef <- as.data.frame(statesums) 	 
colnames(statef)<-c("state.of.res", "count")
statef <- transform(statef,state.of.res=reorder(state.of.res, count))

ggplot(statef)+ geom_bar(aes(x=state.of.res,y=count),stat="identity", fill="gray") + coord_flip() +                                       	
  theme(axis.text.y=element_text(size=rel(0.9)))

#bar chart styles
ggplot(custdata) + geom_bar(aes(x=marital.stat,
                                fill=housing.type)) 	

ggplot(custdata) + geom_bar(aes(x=marital.stat,
                                fill=housing.type),
                            position="dodge")      	 

ggplot(custdata) + geom_bar(aes(x=marital.stat,
                                fill=health.ins),
                            position="fill")


ggplot(custdata, aes(x=marital.stat)) +
  geom_bar(aes(fill=health.ins), position="fill") +
  geom_point(aes(y=-0.05), size=0.75, alpha=0.3, 	 
             position=position_jitter(h=0.01))

ggplot(custdata) +                                          	 
  geom_bar(aes(x=marital.stat), 
           fill="darkgray") +
  facet_wrap(~health.ins, scales="free_y") +               	 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(custdata2) +                                     	 
  geom_bar(aes(x=housing.type, fill=marital.stat ),
           position="dodge") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))   	 



#Line plots
x <- runif(100)   
y <- x^2 + 0.2*x   	
ggplot(data.frame(x=x,y=y), aes(x=x,y=y)) + geom_line()

#Correlation between age and income
custdata2 <- subset(custdata,
                    (custdata$age > 0 & custdata$age < 100 
                     & custdata$income > 0))                  	

cor(custdata2$age, custdata2$income)

#Scatter plot
ggplot(custdata2, aes(x=age, y=income)) + geom_point() + ylim(0, 200000)

ggplot(custdata2, aes(x=age, y=income)) + geom_point() +
  stat_smooth(method="lm") +
  ylim(0, 200000)

ggplot(custdata2, aes(x=age, y=income)) +
  geom_point() + geom_smooth() +
  ylim(0, 200000)


ggplot(custdata2, aes(x=age, y=as.numeric(health.ins))) + 	 
  geom_point(position=position_jitter(w=0.05, h=0.05)) +  	 
  geom_smooth()

#Hexbin Plot
library(hexbin) 	

ggplot(custdata2, aes(x=age, y=income)) +
  geom_hex(binwidth=c(5, 10000)) +   	
  geom_smooth(color="white", se=F) +  	
  ylim(0,200000)

 


