# COMPLETE RANDOMIZATION
set.seed(1)
tr_cod <- c("Fertilizer 1","Fertilizer 2", "Fertilizer 3", "Control")
tr_cod_rep <- rep(tr_cod,6)
tr_cod_data <- sample(tr_cod_rep)
data.frame(tr_cod_data)
