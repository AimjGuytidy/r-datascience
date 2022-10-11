library(glmnet)
library(dplyr)
library(haven)
library(psych)
library(tibble)
library(caTools)

mcf_data_df_shock<-read_dta("data/mcf_data_qual_regression.dta")
#regression with df against quality of life-----

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

#Regression with df , adaptive and absorptive against quality of life index-----


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

#Regression with df against adaptive and absorptive----

data_use <- mcf_data_df_shock[,c("absorptive_index",
                                 "adaptative_index","DFWIA1")]
data_use <- drop_na(data_use)

x <-data.matrix(data_use[,c("absorptive_index","adaptative_index")])
y <- data_use$DFWIA1


cv_model <- cv.glmnet(x,y,alpha = 1)
best_lambda <- cv_model$lambda.min
plot(cv_model)
best_model <- glmnet(x,y,alpha=1,lambda = best_lambda)
sink("coefficients3.txt")
coef(best_model)
sink()

#simple Regression with df against adaptive, absorptive and quality of life----

lm_r = lm(formula=absorptive_index~DFWIA1,
          data = mcf_data_df_shock)
sink("coefficients_df_absorptive.txt")
coef(summary(lm_r))
sink()

lm_r1 = lm(formula=adaptative_index~DFWIA1,
          data = mcf_data_df_shock)
sink("coefficients_df_adaptative.txt")
coef(summary(lm_r1))
sink()

lm_r2 = lm(formula=perc_quality_life~DFWIA1,
           data = mcf_data_df_shock)
sink("coefficients_df_quallife.txt")
coef(summary(lm_r2))
sink()

#Simple linear regression of age education level geo-entity and gender against quality of life index
