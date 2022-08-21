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
set.seed(7)
#Load in data
mcf_data<-read_dta("data/mcf_clean_parfait.dta")
mcf_data<-mcf_data%>%
            select(where(is.numeric))
#sum(!is.na(mcf_data$new_improv_self_employment))
mcf_data<-mcf_data%>%filter(type_employ==2)%>%
  select(-c(uniqueid:note_sec_a))

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
mcf_trans$new_improv_wage_employment <- NULL
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
#predict(glmy,type="response")

#Predictions on the test set
predictTest = predict(model_glm, newdata = test, type = "response")
predictions <- predict(glmy, select(mcf_test,-new_improv_self_employment), type = "response")
view(predictions)
table(mcf_test$new_improv_self_employment, predictions >= 0.5)
performance_accuracy(glmy)
# summarize results
# confusionMatrix(predictions$class, select(mcf_test,new_improv_self_employment))
