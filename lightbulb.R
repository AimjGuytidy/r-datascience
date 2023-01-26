







%>%
  mutate(across(matches("lose_livestocks_[0-9]+$"),~ifelse(.x==1,"Yes","No")))
mcf_lolivestock$lose_livestocks_0 <- set_variable_labels(mcf_lolivestock$lose_livestocks_0,.labels=c(x1="None"))
mcf_lolivestock$lose_livestocks_1 <- set_variable_labels(mcf_lolivestock$lose_livestocks_1,.labels=c(x1="Chickens"))
mcf_lolivestock$lose_livestocks_2 <- set_variable_labels(mcf_lolivestock$lose_livestocks_2,.labels=c(x1="Goats"))
mcf_lolivestock$lose_livestocks_3 <- set_variable_labels(mcf_lolivestock$lose_livestocks_3,.labels=c(x1="Cows"))
mcf_lolivestock$lose_livestocks_4 <- set_variable_labels(mcf_lolivestock$lose_livestocks_4,.labels=c(x1="Pigs"))
mcf_lolivestock$lose_livestocks_5 <- set_variable_labels(mcf_lolivestock$lose_livestocks_5,.labels=c(x1="Rabbits"))
mcf_lolivestock$lose_livestocks_6 <- set_variable_labels(mcf_lolivestock$lose_livestocks_6,.labels=c(x1="Sheep"))
mcf_lolivestock$lose_livestocks_7 <- set_variable_labels(mcf_lolivestock$lose_livestocks_7,.labels=c(x1="Beehives"))

%>%
  mutate(across(matches("equiment_[0-9]+$"),~ifelse(.x==1,"Yes","No")))
mcf_equiment$equiment_0 <- set_variable_labels(mcf_equiment$equiment_0,.labels=c(x1="None"))
mcf_equiment$equiment_1 <- set_variable_labels(mcf_equiment$equiment_1,.labels=c(x1="Television"))
mcf_equiment$equiment_2 <- set_variable_labels(mcf_equiment$equiment_2,.labels=c(x1="Radio"))
mcf_equiment$equiment_3 <- set_variable_labels(mcf_equiment$equiment_3,.labels=c(x1="Watch"))
mcf_equiment$equiment_4 <- set_variable_labels(mcf_equiment$equiment_4,.labels=c(x1="Feature phone"))
mcf_equiment$equiment_5 <- set_variable_labels(mcf_equiment$equiment_5,.labels=c(x1="Smartphone"))
mcf_equiment$equiment_6 <- set_variable_labels(mcf_equiment$equiment_6,.labels=c(x1="A computer"))
mcf_equiment$equiment_7 <- set_variable_labels(mcf_equiment$equiment_7,.labels=c(x1="Car"))
mcf_equiment$equiment_8 <- set_variable_labels(mcf_equiment$equiment_8,.labels=c(x1="Bicycle"))

%>%
  mutate(across(matches("lose_equiment_[0-9]+$"),~ifelse(.x==1,"Yes","No")))
mcf_loequiment$lose_equiment_0 <- set_variable_labels(mcf_loequiment$lose_equiment_0,.labels=c(x1="None"))
mcf_loequiment$lose_equiment_1 <- set_variable_labels(mcf_loequiment$lose_equiment_1,.labels=c(x1="Television"))
mcf_loequiment$lose_equiment_2 <- set_variable_labels(mcf_loequiment$lose_equiment_2,.labels=c(x1="Radio"))
mcf_loequiment$lose_equiment_3 <- set_variable_labels(mcf_loequiment$lose_equiment_3,.labels=c(x1="Watch"))
mcf_loequiment$lose_equiment_4 <- set_variable_labels(mcf_loequiment$lose_equiment_4,.labels=c(x1="Feature phone"))
mcf_loequiment$lose_equiment_5 <- set_variable_labels(mcf_loequiment$lose_equiment_5,.labels=c(x1="Smartphone"))
mcf_loequiment$lose_equiment_6 <- set_variable_labels(mcf_loequiment$lose_equiment_6,.labels=c(x1="A computer"))
mcf_loequiment$lose_equiment_7 <- set_variable_labels(mcf_loequiment$lose_equiment_7,.labels=c(x1="Car"))
mcf_loequiment$lose_equiment_8 <- set_variable_labels(mcf_loequiment$lose_equiment_8,.labels=c(x1="Bicycle"))

%>%
  mutate(across(matches("kindsup_[0-9]+$"),~ifelse(.x==1,"Yes","No")))
mcf_kindsup$kindsup_1 <- set_variable_labels(mcf_kindsup$kindsup_1,.labels=c(x1="Financial"))
mcf_kindsup$kindsup_2 <- set_variable_labels(mcf_kindsup$kindsup_2,.labels=c(x1="Business development training"))
mcf_kindsup$kindsup_3 <- set_variable_labels(mcf_kindsup$kindsup_3,.labels=c(x1="Technical training"))
mcf_kindsup$kindsup_4 <- set_variable_labels(mcf_kindsup$kindsup_4,.labels=c(x1="Farming Support"))
mcf_kindsup$kindsup_5 <- set_variable_labels(mcf_kindsup$kindsup_5,.labels=c(x1="Food Support"))
mcf_kindsup$kindsup_6 <- set_variable_labels(mcf_kindsup$kindsup_6,.labels=c(x1="Health Insurance"))
mcf_kindsup$kindsup_98 <- set_variable_labels(mcf_kindsup$kindsup_98,.labels=c(x1="Other (explain)"))
