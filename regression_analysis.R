library(glmnet)
library(dplyr)
library(haven)
library(psych)
library(tibble)
library(caTools)
library(dplyr)
library(vtable)
mcf_data_df_shock<-read_dta("data/mcf_data_qual_regression.dta")
data_resilience <- mcf_data_df_shock%>%
  mutate(resilience = rowSums(select(.,all_of(c("absorptive_index","adaptative_index"))),
                              na.rm = TRUE))%>%
  filter(stratum!=3)


keyword_label_c<-c("J1.",	"J2.",	"J3.",	"J4.")
variables_for_l512_c <- mcf_data_df_shock%>%look_for(keyword_label_c)
variables_for_l512_c <-variables_for_l512_c[,"variable"]
var_df_c <- as.data.frame(variables_for_l512_c)
var_df_c <- var_df_c[1:4,]

#filtering out unemployed and students
data_expectation <- mcf_data_df_shock%>%
  filter(stratum==1|stratum==2)%>%
  mutate(expectation_score = rowMeans(select(.,all_of(var_df_c)),na.rm = TRUE),
         exp_score_prop = ifelse(round(expectation_score)>=4,1,0))

#simple Regression with df against adaptive, absorptive ----

lm_r = lm(formula=resilience~DFWIA1,
          data = data_resilience)
sink("linear regression on resilience.txt")
print(summary(lm_r))
sink()
closeAllConnections()


data_resilience <- data_resilience%>%
  mutate(gender_male=ifelse(gender==1,1,0),
         gender_female=ifelse(gender==2,1,0),
         age_24=ifelse(age_group=="18-24",1,0),
         age_35=ifelse(age_group=="25-35",1,0),
         geo_rural=ifelse(geo_entity==2,1,0),
         geo_urban=ifelse(geo_entity==1,1,0))

lm_r2 = lm(formula=resilience~gender_male+gender_female+education+
             geo_rural+geo_urban+age_24+age_35+DFWIA1,
           data = data_resilience)
sink("linear regression on resilience using control variables.txt")
print(summary(lm_r2))
sink()
closeAllConnections()

#Simple linear regression of age education level geo-entity and gender against expectation
data_expectation <- data_expectation%>%
  mutate(gender_male=ifelse(gender==1,1,0),
         gender_female=ifelse(gender==2,1,0),
         age_24=ifelse(age_group=="18-24",1,0),
         age_35=ifelse(age_group=="25-35",1,0),
         geo_rural=ifelse(geo_entity==2,1,0),
         geo_urban=ifelse(geo_entity==1,1,0))

lm_r3 = lm(formula=expectation_score~DFWIA1,
          data = data_expectation)
sink("linear regression on expectation.txt")
print(summary(lm_r3))
sink()
closeAllConnections()

lm_r4 = lm(formula=expectation_score~gender_male+education+
             geo_urban+age_24+DFWIA1,
           data = data_expectation)
sink("linear regression on expectation using control variables.txt")
print(summary(lm_r4))
sink()
closeAllConnections()

#Simple linear regression of age education level geo-entity and gender against 
#quality of life index
data_qualitylife <- mcf_data_df_shock%>%
  filter(stratum!=3)%>% #filter out students
  mutate(gender_male=ifelse(gender==1,1,0),
         gender_female=ifelse(gender==2,1,0),
         age_24=ifelse(age_group=="18-24",1,0),
         age_35=ifelse(age_group=="25-35",1,0),
         geo_rural=ifelse(geo_entity==2,1,0),
         geo_urban=ifelse(geo_entity==1,1,0))

lm_r5 = lm(formula=perc_quality_life~DFWIA1,
           data = data_qualitylife)
sink("linear regression on quality of life.txt")
print(summary(lm_r5))
sink()
closeAllConnections()

lm_r6 = lm(formula=perc_quality_life~gender_male+education+
             geo_urban+age_24+DFWIA1,
           data = data_qualitylife)
sink("linear regression on quality of life using control variables.txt")
print(summary(lm_r6))
sink()
closeAllConnections()
