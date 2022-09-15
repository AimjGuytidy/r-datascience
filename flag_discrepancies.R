# Backcheckers 
#join is required
#adding a prefix to columns
#m2 <- cbind(1,1:4)
#colnames(m2) <- c("x","Y")
#colnames(m2) <- paste("Sub", colnames(m2), sep = "_")

library(ggplot2)
library(dplyr)
set.seed(42)
data_enum <- mpg
View(data_enum)
data_enum<-data_enum%>%
  mutate(prim_key = sample(1090:4000,nrow(data_enum)))
data_bc <-data_enum
colnames(data_bc) <- paste("bc",colnames(data_bc),sep = "_")
View(data_bc)
data_bc<-rename(data_bc,prim_key=bc_prim_key)

data_join <- data_bc %>% 
  inner_join(data_enum,by="prim_key")
