#Load required packages
library(foreign) #for reading .dta files
library(dplyr) #for the filter function
library(questionr) #for the weight function
library(tidyverse)
library(haven)
library(openxlsx)
library(readr)
library(tidyr)
library(rio)
library(expss)
library(labelled)
library(weights)
library(anesrake)
library(stringr)
library(zoo)
library(sjmisc)
library(caret)
library(mlbench)
library(performance)
library(rcompanion)#for accuracy metrics
library(MLeval)
library(MLmetrics)
library("Metrics")# Install & load Metrics package
library(mixlm)
set.seed(7)
#Load in data
mcf_data<-read_dta("data/mcf_parfait.dta")
mcf_data<-mcf_data%>%
            select(where(is.numeric))
#sum(!is.na(mcf_data$new_improv_self_employment))
mcf_data<-mcf_data%>%filter(type_employ==2)%>%
  select(-c(uniqueid:note_sec_a))

#LOGISTIC REGRESSION####
# Data Preprocessing

pgp <- preProcess(select(mcf_data,-new_improv_self_employment),method = c("center", "scale", "YeoJohnson", "nzv"))

transformed <- predict(pgp, newdata = select(mcf_data,-new_improv_self_employment))
#head(transformed)
#view(cor(transformed,select(mcf_data,new_improv_self_employment)))
mcf_trans <- transformed
mcf_trans$new_improv_self_employment <- mcf_data$new_improv_self_employment
trainIndex <- createDataPartition(mcf_trans$new_improv_self_employment, p = .8, 
                                  list = FALSE, 
                                  times = 1)
mcf_train <- mcf_trans[trainIndex,]
mcf_test <-mcf_trans[-trainIndex,]
glmy <- glm(new_improv_self_employment~fami_need + indi_need + resp_workplace + 
              fully_ambition + wor_condi + inc_impro + chang_earning + chang_condition,
            data=mcf_train,
            family = binomial(link="logit"))

summary(glmy)

# New model considering the significance level of previous model components
glmy2 <- glm(new_improv_self_employment~resp_workplace + wor_condi + inc_impro,
            data=mcf_train,
            family = binomial(link="logit"))

summary(glmy2)

#Predictions on the test set
predictions <- predict(glmy, select(mcf_test,-new_improv_self_employment), type = "response")
table(mcf_test$new_improv_self_employment, predictions >= 0.5)
performance_accuracy(glmy)
# accuracy(glmy)

#Performance of the 2nd model
predictions2 <- predict(glmy2, select(mcf_test,-new_improv_self_employment), type = "response")
table(mcf_test$new_improv_self_employment, predictions2 >= 0.5)
performance_accuracy(glmy2)
# accuracy(glmy2)
pred2 <- round(predictions2)
# comparing the two models
tab_ful <- table(pred2,mcf_test$new_improv_self_employment)
recall(tab_ful)
Recall(mcf_test$new_improv_self_employment,pred2)

sink("logisticm3.txt")
print(summary(glmy2))
cat("###################################################\n")
print(performance_accuracy(glmy2))
cat("###################################################\n")
cat("The confusion matrix of the predicted versus the test true values\n")
print(confusion(mcf_test$new_improv_self_employment,pred2))
sink()

#LINEAR REGRESSION
#we are going to create a regression model on quality of life index using prod_quality_life as our dependent variable
mcf_data<-read_dta("data/mcf_clean_parfait.dta")
mcf_data<-mcf_data%>%
  select(where(is.numeric))
#sum(!is.na(mcf_data$new_improv_self_employment))
mcf_data<-mcf_data%>%
            select(-c(uniqueid:note_sec_a))

pgp2 <- preProcess(select(mcf_data,-prod_quality_life),method = c("center", "scale", "YeoJohnson","nzv"))

transformed <- predict(pgp2, newdata = select(mcf_data,-prod_quality_life))
#head(transformed)
#view(cor(transformed,select(mcf_data,prod_quality_life)))
mcf_trans <- transformed
mcf_trans$new_improv_wage_employment <- NULL
mcf_trans$prod_quality_life <- mcf_data$prod_quality_life
trainIndex <- createDataPartition(mcf_trans$prod_quality_life, p = .8, 
                                  list = FALSE, 
                                  times = 1)
mcf_train <- mcf_trans[trainIndex,]
mcf_test <-mcf_trans[-trainIndex,]
glmy3 <-
  lm(
    prod_quality_life ~ internet + sanitation + food + transport + telephone
    + bank_account + electricity + roads + transport_comp + nature +
      internet_comp
    + clean_water + bank_account_comp + sanitation_comp + electricity_comp
    + nature_comp + telephone_comp + healthcare + roads_comp + healthcare_comp
    + food_comp + clean_water_comp + loans + loans_comp + equiment_5 +
      education + equiment_2 + language_1 + computer_ownership + language_0 +
      floor + phone_ownership + trainings_0 + online_course,
    data = mcf_train
  )
summary(glmy3)
performance_accuracy(glmy3)

glmy4 <-
  lm(
    prod_quality_life ~ internet + sanitation + food + transport + telephone
    + bank_account + electricity + roads + transport_comp + nature +
      internet_comp
    + clean_water + bank_account_comp + sanitation_comp + electricity_comp
    + nature_comp + telephone_comp + healthcare + roads_comp + healthcare_comp
    + food_comp + clean_water_comp + loans + loans_comp,
    data = mcf_train
  )
summary(glmy4)
performance_accuracy(glmy4)

prediction4<- predict.lm(glmy4, select(mcf_test,-prod_quality_life), type = "response")
mean((mcf_test$prod_quality_life-prediction4)^2)
MSE(mcf_test$prod_quality_life,prediction4)
RMSE(mcf_test$prod_quality_life,prediction4)
MAPE(mcf_test$prod_quality_life,prediction4)
MAE(mcf_test$prod_quality_life,prediction4)
#RMSLE(mcf_test$prod_quality_life,prediction4)
R2_Score(mcf_test$prod_quality_life,prediction4)
sink("lm.txt")
print(summary(glmy4))
sink()
# so rep is for repeat and it can help create an empty dataframe with a size you want eg: tibble(pred=rep(NA,324))