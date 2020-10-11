###Predictive Modeling-Telecom Churn

# Install Packages & Load Libraries ---------------------------------------
pacman::p_load(tidyverse, summarytools, MASS, rsample, caret,corrplot,ggpubr,naniar)



##Load Data in R
library(readxl)
setwd("C:/Users/CHARUMATHY NARAYANAN/Documents/R-worked files/Batch-apr/PredictiveModeling/Project/")
Churned <- read_excel(file.choose())
Churned_n <- read_excel(file.choose())
glimpse(Churned)
attach(Churned)
head(Churned)
str(Churned)

##Conversion to factors
Churned$Churn = as.factor(Churned$Churn)
Churned$ContractRenewal = as.factor(Churned$ContractRenewal)
Churned$DataPlan = as.factor(Churned$DataPlan)

###Finding missing vales (NA)
sapply(Churned, function(x) sum(is.na(x)))
vis_miss(Churned)


# using  geom_miss_point()
ggplot(Churned,
       aes(x = ContractRenewal,
           y = DataPlan)) +
  geom_miss_point()


###Summary
summarytools::view(dfSummary(Churned))



##Univariate Analysis

ggplot(Churned, aes(Churn, fill = Churn)) +
  geom_bar() +
  theme(legend.position = 'right')

table(Churned$Churn)

round(prop.table(table(Churned$Churn)),3)


Churned %>%
  keep(is.numeric) %>%
  gather() %>%
  ggplot() +
  geom_histogram(mapping = aes(x=value,fill=key), color="black") +
  facet_wrap(~ key, scales = "free") +
  theme_minimal() +
  theme(legend.position = 'none')

Churned %>%
  keep(is.numeric) %>%
  gather() %>%
  ggplot() +
  geom_boxplot(mapping = aes(x=value,fill=key), color="black") +
  facet_wrap(~ key, scales = "free") +
  theme_minimal() +
  theme(legend.position = 'none')

ggplot(Churned, aes(DataPlan, fill = DataPlan)) +
  geom_bar() +
  theme(legend.position = 'none')

ggplot(Churned, aes(ContractRenewal, fill = ContractRenewal)) +
  geom_bar() +
  theme(legend.position = 'none')

###Bivariate analysis

Accountweeks1 <- cut(Churned$AccountWeeks, breaks = seq(0, 300, by = 20))
ggplot(Churned, aes(Accountweeks1, ..count.., fill = Churn)) + 
  geom_bar(position = "dodge",color = 'red')

table(Churned$Churn, Churned$ContractRenewal)

ggplot(Churned, aes(ContractRenewal, ..count.., fill = Churn)) + 
          geom_bar(position="dodge")

ggplot(Churned, aes(DataPlan, ..count.., fill = Churn)) + 
  geom_bar(position="dodge")

dataUsage <- cut(Churned$DataUsage, include.lowest = TRUE, breaks = seq(0, 5.5, by = 0.5))
ggplot(Churned, aes(dataUsage, ..count.., fill = Churn)) + 
  geom_bar(position="dodge")

Custsercalls = cut(Churned$CustServCalls, include.lowest = TRUE, breaks = seq(0, 5.5, by = 0.5))
ggplot(Churned, aes(Custsercalls, ..count.., fill = Churn)) + 
  geom_bar(position="dodge")

daymins <- cut(Churned$DayMins, include.lowest = TRUE, breaks = seq(0, 385, by = 35))
ggplot(Churned, aes(daymins, ..count.., fill = Churn)) + 
  geom_bar(position="dodge")



dayCalls <- cut(Churned$DayCalls, include.lowest = TRUE, breaks = seq(0, 165, by = 16.5))
ggplot(Churned, aes(dayCalls, ..count.., fill = Churn)) + geom_bar(position="dodge")

monthlyCharge <- cut(Churned$MonthlyCharge, include.lowest = TRUE, breaks = seq(14, 114, by = 10))
ggplot(Churned, aes(monthlyCharge, ..count.., fill = Churn)) + geom_bar(position="dodge")

overageFee <- cut(Churned$OverageFee, include.lowest = TRUE, breaks = seq(0, 19, by = 1.9))
ggplot(Churned, aes(overageFee, ..count.., fill = Churn)) + geom_bar(position="dodge")

roamMins <- cut(Churned$RoamMins, include.lowest = TRUE, breaks = seq(0, 20, by = 2))
ggplot(Churned, aes(roamMins, ..count.., fill = Churn)) + geom_bar(position="dodge")


##Outliner treatment

quantile(Churned$AccountWeeks,c(0.05,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.95,0.96,0.99,1))
Churned$AccountWeeks[which(Churned$AccountWeeks > 195)] =195
summary(Churned$AccountWeeks)

quantile(Churned$DataUsage,c(0.05,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.95,0.96,0.99,1))
Churned$DataUsage[which(Churned$DataUsage > 4.10)] = 4.10
summary(Churned$DataUsage)

quantile(Churned$CustServCalls,c(0.05,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.95,0.96,0.99,1))
Churned$CustServCalls[which(Churned$CustServCalls > 6)] = 6
summary(Churned$CustServCalls)


###Collinearity

library(corrplot)
corrplot(cor(Churned_n))


##Splitting as 70:30

set.seed(233)
Split1 <- initial_split(Churned, prop = 0.7, strata = "Churn", seed = 123)
Train <- training(Split1)
Test <- testing(Split1)

##checking dimensions of train and test splits of dataset
dim(Train)
dim(Test)

##Logistic Regression
model1 <- glm(Churn ~ ., data= Churned, family=binomial)
summary(model1)

#VIF
library(car)
vif(model1)

#Chi Square test
anova(model1, test = "Chisq")


#Interpretation of LR

logitR.pred = predict(model1, newdata = Test, type = "response")

logitR.predicted = ifelse(logitR.pred > 0.5 , 1, 0)
logitR.predF = factor(logitR.predicted, levels = c(0,1))

mean(logitR.predF == Test$Churn)




### Confusion Matrix for logitR model 
logitR.CM = confusionMatrix(logitR.predF, Test$Churn, positive = "1")
logitR.CM

##ROC for Logistic Regression model
library(ROCR)
ROCRpred = prediction(logitR.pred, Test$Churn)
AUC=as.numeric(performance(ROCRpred, "auc")@y.values)

## Area under the curve for LR model
AUC

perf = performance(ROCRpred, "tpr","fpr")
plot(perf,col="black",lty=2, lwd=2,colorize=T, main="ROC curve", xlab="Specificity", 
     ylab="Sensitivity")

abline(0,1)



###KNN Model:


## Matrix for check of split of Response var in Train and Test datasets
table(Train$Churn)
table(Test$Churn)

set.seed(123)
KNN <- train(Churn~., data = Train,
             method = 'knn',
             preProcess = c("center","scale"))
print(KNN)
plot(KNN)
KNN$bestTune

KNN_Predict <- KNN %>% predict(Test)
head(KNN_Predict)
confusionMatrix(KNN)
caret::confusionMatrix(KNN_Predict, Test$Churn)

# Naive Bayes Model 
set.seed(123)
NB <- train(Churn~., data = Train,
            method = 'nb')
print(NB)
plot(NB)


NB_Predict <- NB %>% predict(Test)
head(NB_Predict)
confusionMatrix(NB)
caret::confusionMatrix(NB_Predict, Test$Churn)


##Comparing Models 
results <- resamples(list(
                     K_Nearest_Neighbour = KNN,
                            Naive_Bayes = NB))
results$values
summary(results)
bwplot(results)

