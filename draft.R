mcf_train_sel <- select(mcf_train,new_improv_self_employment,resp_workplace,wor_condi,inc_impro)
fitControl <- trainControl(method = "cv",summaryFunction=prSummary,
                           classProbs=T,savePredictions = T,verboseIter = F)
im_fit <- train(new_improv_self_employment ~ ., data = mcf_train_sel,method = "glm",metric = "AUC",
                trControl = fitControl)
im_fit2 <- train(new_improv_self_employment ~ ., data = mcf_train_sel,method = "xgbTree",metric = "AUC",
                 trControl = fitControl)
im_fit2 <- train(new_improv_self_employment ~ ., data = mcf_train_sel,method = "rf",metric = "AUC",
                 trControl = fitControl)

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
set.seed(7)
#Load in data
mcf_data<-read_dta("data/mcf_parfait.dta")
mcf_data<-mcf_data%>%
  select(where(is.numeric))
#sum(!is.na(mcf_data$new_improv_self_employment))
mcf_data<-mcf_data%>%filter(type_employ==2)%>%
  select(-c(uniqueid:note_sec_a))

#LOGISTIC REGRESSION####
# create a correlation matrix
cormatrix <- cor(select(mcf_data,-new_improv_self_employment))
cormatrix[is.na(cormatrix)] <-0
colnames(cormatrix[rowSums(cormatrix)==1,])
view(cormatrix[rowSums(cormatrix)==1,])
# check highly correlated variables with a cutoff of 0.5

highlycorrelated <- findCorrelation(cormatrix,cutoff = 0.5)
view(highlycorrelated)

pgp <- preProcess(select(mcf_data,-new_improv_self_employment),method = c("center", "scale", "YeoJohnson", "nzv"))

transformed <- predict(pgp, newdata = select(mcf_data,-new_improv_self_employment))
head(transformed)
view(cor(transformed,select(mcf_data,new_improv_self_employment)))
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
#LINEAR REGRESSION
#we are going to create a regression model on quality of life index using prod_quality_life as our dependent variable
mcf_data<-read_dta("data/mcf_clean_parfait.dta")
mcf_data<-mcf_data%>%
  select(where(is.numeric))
#sum(!is.na(mcf_data$new_improv_self_employment))
mcf_data<-mcf_data%>%
  select(-c(uniqueid:note_sec_a))

pgp2 <- preProcess(select(mcf_data,-prod_quality_life),method = c("center", "scale", "YeoJohnson"))

transformed <- predict(pgp, newdata = select(mcf_data,-prod_quality_life))
head(transformed)
view(cor(transformed,select(mcf_data,prod_quality_life)))
mcf_trans <- transformed
mcf_trans$new_improv_wage_employment <- NULL
mcf_trans$new_improv_self_employment <- mcf_data$new_improv_self_employment
trainIndex <- createDataPartition(mcf_trans$new_improv_self_employment, p = .8, 
                                  list = FALSE, 
                                  times = 1)
mcf_train <- mcf_trans[trainIndex,]
mcf_test <-mcf_trans[-trainIndex,]