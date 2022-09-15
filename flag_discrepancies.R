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
data_bc
data_join <- data_bc %>% 
  inner_join(data_enum,by="prim_key")

data_join <- select(data_join,-prim_key)
for(i in unique(gsub("^[b][c](_)","",names(data_join)))){
  data_join[,paste0("equal_", i)] <- ifelse(
    data_join[,i] == data_join[,paste0("bc_", i)], 
    0, 1
  )
}


for(i in unique(gsub("^[b][c](_)","",colnames(data_join)[colnames(data_join)!="prim_key"]))){
  data_join[,paste0("equal_", i)] <- ifelse(
    data_join[,i] == data_join[,paste0("bc_", i)], 
    0, 1
  )
}