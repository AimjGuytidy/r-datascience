library(glmnet)
library(dplyr)
library(haven)
library(psych)
library(tibble)

mcf_data_df_shock<-read_dta("data/mcf_data_qual_regression.dta")
#regression with df against quality of life

X <- as.data.frame(mcf_data_df_shock$DFWIA1)
X <- X%>%
  mutate(dignified=ifelse(mcf_data_df_shock$DFWIA1==1,1,0),
         undignified=ifelse(mcf_data_df_shock$DFWIA1==0,1,0))
x <-data.matrix(X[,c("dignified","undignified")])
y <- mcf_data_df_shock$perc_quality_life


cv_model <- cv.glmnet(x,y,alpha = 1)
best_lambda <- cv_model$lambda.min
plot(cv_model)
best_model <- glmnet(x,y,alpha=1,lambda = best_lambda)
sink("coefficients.txt")
coef(best_model)
sink()

#Regression with df , adaptive and absorptive against quality of life index


(adapt_qual <- ggplot(data = mcf_data_df_shock,aes(perc_quality_life,
                                    adaptative_index))+
  geom_point()+
  geom_smooth())

(absorp_qual <- ggplot(data = mcf_data_df_shock,aes(perc_quality_life,
                                                   absorptive_index))+
  geom_point()+
  geom_smooth())

data_use <- mcf_data_df_shock[,c("uniqueid",
                                "adaptative_index","absorptive_index",
                                "perc_quality_life")]
data_use <- drop_na(data_use)

x <-data.matrix(data_use[,c("adaptative_index","absorptive_index")])
y <- data_use$perc_quality_life


cv_model <- cv.glmnet(x,y,alpha = 1)
best_lambda <- cv_model$lambda.min
plot(cv_model)
best_model <- glmnet(x,y,alpha=1,lambda = best_lambda)
sink("coefficients2.txt")
coef(best_model)
sink()


# Regression with 

#Regression with df against adaptive and absorptive


(adapt_qual <- ggplot(data = mcf_data_df_shock,aes(perc_quality_life,
                                                   adaptative_index))+
    geom_point()+
    geom_smooth())

(absorp_qual <- ggplot(data = mcf_data_df_shock,aes(perc_quality_life,
                                                    absorptive_index))+
    geom_point()+
    geom_smooth())

data_use <- mcf_data_df_shock[,c("uniqueid",
                                 "adaptative_index","absorptive_index",
                                 "perc_quality_life")]
data_use <- drop_na(data_use)

x <-data.matrix(data_use[,c("adaptative_index")])
y <- data_use$perc_quality_life


cv_model <- cv.glmnet(x,y,alpha = 1)
best_lambda <- cv_model$lambda.min
plot(cv_model)
best_model <- glmnet(x,y,alpha=1,lambda = best_lambda)
sink("coefficients2.txt")
coef(best_model)
sink()