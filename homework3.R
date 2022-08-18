install.packages("tictoc")
library(tictoc)
dist_vec <- c("kigali","muhanga","rwamaganat","nyamasheke")

stringr::str_length(dist_vec)

for (i in dist_vec){
  if (nchar(i) == max(nchar(dist_vec))){
    print(i)
  }
}

stringr::str_detect(dist_vec,"^r",ignore.case=TRUE)
