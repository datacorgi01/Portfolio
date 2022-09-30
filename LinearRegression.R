#Read in the csv file
Stores = read.csv("/Users/allisonking/Downloads/Stores.csv")
str(Stores)

#Subset the data randomly
library(caTools)
set.seed(48)
split = sample.split(Stores, SplitRatio = 0.67)
Train = subset(Stores, split==TRUE)
Test = subset(Stores, split==FALSE)

#Correlation matrix
cor(Train)

#EARN is most positively correlated with P35

#Initial linear regression model
MyMod = lm(EARN ~ STOR + SIZE + EMPL + total + P15 + P25 + P35 + P45 + P55 + INC + COMP + NCOMP + NREST + PRICE + CLI, data = Train)
summary(MyMod)

#R^2 is 0.9007, statistically significant coefficients are SIZE, Intercept, NREST, INC, & PRICE at the 99% level 
#P15 at a 90% level

#Start by refining w/o total
MyMod = lm(EARN ~ STOR + SIZE + EMPL + P15 + P25 + P35 + P45 + P55 + INC + COMP + NCOMP + NREST + PRICE + CLI, data = Train)
summary(MyMod)

#Refine w/o EMPL
MyMod = lm(EARN ~ STOR + SIZE + P15 + P25 + P35 + P45 + P55 + INC + COMP + NCOMP + NREST + PRICE + CLI, data = Train)
summary(MyMod)

#Refine w/o P55
MyMod = lm(EARN ~ STOR + SIZE + P15 + P25 + P35 + P45 + INC + COMP + NCOMP + NREST + PRICE + CLI, data = Train)
summary(MyMod)

#Refine w/o NCOMP
MyMod = lm(EARN ~ STOR + SIZE + P15 + P25 + P35 + P45 + INC + COMP + NREST + PRICE + CLI, data = Train)
summary(MyMod)

#Refine w/o STOR
MyMod = lm(EARN ~ SIZE + P15 + P25 + P35 + P45 + INC + COMP + NREST + PRICE + CLI, data = Train)
summary(MyMod)

#Refine w/o P35
MyMod = lm(EARN ~ SIZE + P15 + P25 + P45 + INC + COMP + NREST + PRICE + CLI, data = Train)
summary(MyMod)

#Refine w/o P45
MyMod = lm(EARN ~ SIZE + P15 + P25 + INC + COMP + NREST + PRICE + CLI, data = Train)
summary(MyMod)

#Refine w/o CLI
MyMod = lm(EARN ~ SIZE + P15 + P25 + INC + COMP + NREST + PRICE, data = Train)
summary(MyMod)

#Refine w/o COMP
MyMod = lm(EARN ~ SIZE + P15 + P25 + INC + NREST + PRICE, data = Train)
summary(MyMod)

#Refine w/o P25
MyMod = lm(EARN ~ SIZE + P15 + INC + NREST + PRICE, data = Train)
summary(MyMod)

#New R^2 is 0.86
#Linear equation is EARN = 1.28e-06*SIZE + 3.03e-10*P15 + 0.000443*INC + 5.29e-05*NREST + 0.001850*PRICE + 1.03e-05
#Each sign, except the intercept, is positive which makes sense

#Evaluating quality of predictions
EarnPredictions = predict(MyMod, newdata=Test)
EarnPredictions

#Computing R^2 on Test Set 
SSE = sum((Test$EARN - EarnPredictions)^2)
SST = sum((Test$EARN - mean(Train$EARN))^2)
1 - SSE/SST

#R^2 for test set is 0.8331948

