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
#View(data_bc)
data_bc<-rename(data_bc,prim_key=bc_prim_key)
data_bc$bc_displ[32] <- 8.7
data_bc$bc_displ[11] <- 11.7
data_bc$bc_cty[11] <- 471
data_join <- data_bc %>% 
  inner_join(data_enum,by="prim_key")

for(i in unique(gsub("^(bc)(_)","",colnames(data_join)[colnames(data_join)!="prim_key"]))){
  data_join[,paste0("equal_", i)] <- ifelse(
    data_join[,i] == data_join[,paste0("bc_", i)], 
    0, 1
  )
}

#filter(data_join,if_any(colnames(select(data_join,matches("^equal_+"))),~.x==1))
for (i in colnames(select(data_join,matches("^equal_+")))){
  #print(i)
  for (j in as.vector(data_join[,i] == 1)){
  #print(select(data_join[which(as.vector(data_join[,i] == 1)),],prim_key,matches(paste0("^",gsub("^equal_","bc_",i))),
                 #matches(paste0("^",gsub("^equal_","",i)))))
     if (j == TRUE) {
         print(select(data_join[which(as.vector(data_join[,i] == 1)),],prim_key,matches(paste0("^",gsub("^equal_","bc_",i))),
  matches(paste0("^",gsub("^equal_","",i)))))
     }
    assign(paste0("df_",i),select(data_join[which(as.vector(data_join[,i] == 1)),],prim_key,matches(paste0("^",gsub("^equal_","bc_",i))),
                                  matches(paste0("^",gsub("^equal_","",i)))))
    #View(select(data_join,-matches("^equal")))
  }
}
