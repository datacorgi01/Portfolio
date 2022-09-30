#Read in the data set
Loan = read.csv("/Users/allisonking/Downloads/SampleData.csv")
str(Loan)
summary(Loan)

#Load libraries
library(lattice)
library(mice)
library(VIM)
library(dplyr)
library(ggplot2)
library(caTools)
library(ROCR)

#Multiple imputation on the data
Simple = Loan[c("Age", "ParticipatesInCommunity", "SocialFabric", "MonthlyPayment")]
summary(Simple)
set.seed(144)

#Visualize missing data
md.pattern(Simple)
simple_aggr = aggr(Simple, col=mdc(1:2), numbers=TRUE, sortVars=TRUE, labels=names(Simple), cex.axis=.7, gap=3, ylab=c("Proportion of missingness","Missingness Pattern"))

#Remove variable with highest count of missing values, which is MonthlyPayment
Loan1 = subset(Loan, select = -c(MonthlyPayment))

#Scatterplot showing LoanSizeAvg as a function of MonthlyIncome, color-coded depending on whether customers paid back their loan or not
ggplot(Loan1, aes(x = LoanSizeAvg, y = MonthlyIncome, color = NotPayBackLoan)) + geom_point()

#Remove the instances where there is missing data using na.omit
LoanNew = na.omit(Loan1)
str(LoanNew)
summary(LoanNew)

#Split data between training and testing sets, prepping for logistic regression model
set.seed(48)
split = sample.split(LoanNew, SplitRatio = 0.65)
Train = subset(LoanNew, split==TRUE)
Test = subset(LoanNew, split==FALSE)
str(Train)
str(Test)

#Initial logistic regression model on training data
TrainModel = glm(NotPayBackLoan ~ Sex + Age + FamilySize + YearsAtThisHome + MainHousehold + MonthlyIncome + MonthlyExpenses + ParticipatesInCommunity + SocialFabric + LoanOpinion + LoanSizeAvg + UpfrontPaymentAvg + LoanPeriodMonths, data = Train, family = binomial)
summary(TrainModel)

#Refine model by removing variable with lowest significance (the intercept, in this case)
TrainModel = glm(NotPayBackLoan ~ Sex + Age + FamilySize + YearsAtThisHome + MainHousehold + MonthlyIncome + MonthlyExpenses + ParticipatesInCommunity + SocialFabric + LoanOpinion + LoanSizeAvg + UpfrontPaymentAvg + LoanPeriodMonths - 1, data = Train, family = binomial)
summary(TrainModel)

#Refine model
TrainModel = glm(NotPayBackLoan ~ Age + FamilySize + YearsAtThisHome + MainHousehold + MonthlyIncome + ParticipatesInCommunity + SocialFabric + LoanSizeAvg + UpfrontPaymentAvg + LoanPeriodMonths - 1, data = Train, family = binomial)
summary(TrainModel)

#Refine model
TrainModel = glm(NotPayBackLoan ~ Age + FamilySize + YearsAtThisHome + MainHousehold + MonthlyIncome + ParticipatesInCommunity + LoanSizeAvg + UpfrontPaymentAvg + LoanPeriodMonths - 1, data = Train, family = binomial)
summary(TrainModel)

#Refine model
TrainModel = glm(NotPayBackLoan ~ Age + FamilySize + YearsAtThisHome + MonthlyIncome + ParticipatesInCommunity + LoanSizeAvg + UpfrontPaymentAvg + LoanPeriodMonths - 1, data = Train, family = binomial)
summary(TrainModel)

#Refine model
TrainModel = glm(NotPayBackLoan ~ Age + FamilySize + YearsAtThisHome + ParticipatesInCommunity + LoanSizeAvg + UpfrontPaymentAvg + LoanPeriodMonths - 1, data = Train, family = binomial)
summary(TrainModel)

#Refine model
TrainModel = glm(NotPayBackLoan ~ Age + FamilySize + ParticipatesInCommunity + LoanSizeAvg + UpfrontPaymentAvg + LoanPeriodMonths - 1, data = Train, family = binomial)
summary(TrainModel)

#Refine model
TrainModel = glm(NotPayBackLoan ~ Age + ParticipatesInCommunity + LoanSizeAvg + UpfrontPaymentAvg + LoanPeriodMonths - 1, data = Train, family = binomial)
summary(TrainModel)

#Refine model - now it's good!
TrainModel = glm(NotPayBackLoan ~ ParticipatesInCommunity + LoanSizeAvg + UpfrontPaymentAvg + LoanPeriodMonths - 1, data = Train, family = binomial)
summary(TrainModel)

#Compare predictions
PredictTrain = predict(TrainModel, type="response") 
#Type = response - gives probabilities as predictions
PredictTrain
table(Train$NotPayBackLoan, PredictTrain > 0.5)
table(Train$NotPayBackLoan, PredictTrain > 0.7)
table(Train$NotPayBackLoan, PredictTrain > 0.2)

#AUC and ROC for training set
ROCRpred = prediction(PredictTrain, Train$NotPayBackLoan)
ROCCurve = performance(ROCRpred, "tpr", "fpr")
plot(ROCCurve)
plot(ROCCurve, colorize=TRUE, print.cutoffs.at=seq(0,1,0.1), text.adj=c(-0.2,0.7))
as.numeric(performance(ROCRpred, "auc")@y.values)

#AUC and ROC for testing set
PredictTest = predict(TrainModel, type="response", newdata=Test)
table(Test$NotPayBackLoan, PredictTest > 0.2)
ROCRpred2 = prediction(PredictTest, Test$NotPayBackLoan)
ROCCurve2 = performance(ROCRpred2, "tpr", "fpr")
plot(ROCCurve2)
plot(ROCCurve2, colorize=TRUE, print.cutoffs.at=seq(0,1,0.1), text.adj=c(-0.2,0.7))
as.numeric(performance(ROCRpred2, "auc")@y.values)

#Now try a logistic regression model with imputed data as opposed to removing blank data
#Fill in the missing data using the “mice” package 
imp = mice(Simple, m=5, printFlag=FALSE, maxit = 30, seed=2525)

imputed = complete(imp)
summary(imputed)
Loan1$Age = imputed$Age
Loan1$ParticipatesInCommunity= imputed$ParticipatesInCommunity
Loan1$SocialFabric = imputed$SocialFabric
summary(Loan1)
str(Loan1)

#Subset data into training set and test set
set.seed(48)
split = sample.split(Loan1, SplitRatio = 0.65)
Train1 = subset(Loan1, split==TRUE)
Test1 = subset(Loan1, split==FALSE)
str(Train1)
str(Test1)

#Initial logistic regression model on training data
TrainMiceModel = glm(NotPayBackLoan ~ Sex + Age + FamilySize + YearsAtThisHome + MainHousehold + MonthlyIncome + MonthlyExpenses + ParticipatesInCommunity + SocialFabric + LoanOpinion + LoanSizeAvg + UpfrontPaymentAvg + LoanPeriodMonths, data = Train1, family = binomial)
summary(TrainMiceModel)

#Refine
TrainMiceModel = glm(NotPayBackLoan ~ Sex + Age + YearsAtThisHome + MainHousehold + MonthlyIncome + MonthlyExpenses + ParticipatesInCommunity + SocialFabric + LoanOpinion + LoanSizeAvg + UpfrontPaymentAvg + LoanPeriodMonths, data = Train1, family = binomial)
summary(TrainMiceModel)

#Refine
TrainMiceModel = glm(NotPayBackLoan ~ Sex + Age + YearsAtThisHome + MonthlyIncome + MonthlyExpenses + ParticipatesInCommunity + SocialFabric + LoanOpinion + LoanSizeAvg + UpfrontPaymentAvg + LoanPeriodMonths, data = Train1, family = binomial)
summary(TrainMiceModel)

#Refine
TrainMiceModel = glm(NotPayBackLoan ~ Sex + Age + YearsAtThisHome + MonthlyIncome + MonthlyExpenses + ParticipatesInCommunity + SocialFabric + LoanOpinion + LoanSizeAvg + UpfrontPaymentAvg, data = Train1, family = binomial)
summary(TrainMiceModel)

#Refine
TrainMiceModel = glm(NotPayBackLoan ~ Sex + Age + YearsAtThisHome + MonthlyIncome + MonthlyExpenses + ParticipatesInCommunity + LoanOpinion + LoanSizeAvg + UpfrontPaymentAvg, data = Train1, family = binomial)
summary(TrainMiceModel)

#Refine
TrainMiceModel = glm(NotPayBackLoan ~ Sex + Age + YearsAtThisHome + MonthlyIncome + ParticipatesInCommunity + LoanOpinion + LoanSizeAvg + UpfrontPaymentAvg, data = Train1, family = binomial)
summary(TrainMiceModel)

#Refine
TrainMiceModel = glm(NotPayBackLoan ~ Sex + Age + YearsAtThisHome + ParticipatesInCommunity + LoanOpinion + LoanSizeAvg + UpfrontPaymentAvg, data = Train1, family = binomial)
summary(TrainMiceModel)

#Refine
TrainMiceModel = glm(NotPayBackLoan ~ Sex + YearsAtThisHome + ParticipatesInCommunity + LoanOpinion + LoanSizeAvg + UpfrontPaymentAvg, data = Train1, family = binomial)
summary(TrainMiceModel)

#Refine
TrainMiceModel = glm(NotPayBackLoan ~ Sex + YearsAtThisHome + ParticipatesInCommunity + LoanOpinion + LoanSizeAvg + UpfrontPaymentAvg - 1, data = Train1, family = binomial)
summary(TrainMiceModel)

#Refine
TrainMiceModel = glm(NotPayBackLoan ~ YearsAtThisHome + ParticipatesInCommunity + LoanOpinion + LoanSizeAvg + UpfrontPaymentAvg - 1, data = Train1, family = binomial)
summary(TrainMiceModel)

#Refine
TrainMiceModel = glm(NotPayBackLoan ~ YearsAtThisHome + ParticipatesInCommunity + LoanSizeAvg + UpfrontPaymentAvg - 1, data = Train1, family = binomial)
summary(TrainMiceModel)

#Refine
TrainMiceModel = glm(NotPayBackLoan ~ YearsAtThisHome + LoanSizeAvg + UpfrontPaymentAvg - 1, data = Train1, family = binomial)
summary(TrainMiceModel)

#AUC and ROC for training set, recomputed with imputed data
PredictTrain1 = predict(TrainMiceModel, type="response")
ROCRpred = prediction(PredictTrain1, Train1$NotPayBackLoan)
ROCCurve = performance(ROCRpred, "tpr", "fpr")
plot(ROCCurve)
plot(ROCCurve, colorize=TRUE, print.cutoffs.at=seq(0,1,0.1), text.adj=c(-0.2,0.7))
as.numeric(performance(ROCRpred, "auc")@y.values)

table(Train1$NotPayBackLoan, PredictTrain1 > 0.5)
table(Train1$NotPayBackLoan, PredictTrain1 > 0.7)
table(Train1$NotPayBackLoan, PredictTrain1 > 0.2)

#AUC and ROC for testing set, recomputed with imputed data
PredictTest1 = predict(TrainMiceModel, type="response", newdata=Test1)
table(Test1$NotPayBackLoan, PredictTest1 > 0.2)
ROCRpred3 = prediction(PredictTest1, Test1$NotPayBackLoan)
ROCCurve3 = performance(ROCRpred3, "tpr", "fpr")
plot(ROCCurve3)
plot(ROCCurve3, colorize=TRUE, print.cutoffs.at=seq(0,1,0.1), text.adj=c(-0.2,0.7))
as.numeric(performance(ROCRpred3, "auc")@y.values)
