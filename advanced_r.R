
library(lobstr)
set.seed(42)
df <- data.frame(runif(3), runif(3))
names(df) <- c(1,2)
#df$3 <- df$1 + df$2
df$`3` <- df$`1` + df$`2`
x <- runif(1e6)
y <- list(x,x,x)
object.size(y) # 24 mb, this is inaccurate!!!
lobstr::obj_size(y) # 8 mb, now this is accurate!!

a <- c(1, 5, 3, 2)
b <- a
b[[1]]
b[1]
b[[1]] <- 10
a
b
# a got copied on the 2 line (wrong, it got copied on the 3 line!!!)

# 2.2 BINDING BASICS #
######################

x <- c(1,2,4) # this code is binding the vector c(1,2,4) to a name x
y <- x # this code is binding y to the vector c(1,2,4)
lobstr::obj_addr(x) # object's identifier "0x1f89a39f7c8"

# 2.2.1 Non-syntactic names####
# syntactic names (allowed) vs non-syntactic names (not allowed)
#_abx <- 3 this gives an error!
# using backticks overrides the problem!
`_abx` <- 3

## exercises
############
# 1
# a,b,c,d are all names binded on the object 1:10
# 2
obj_addr(mean)
obj_addr(base::mean)
obj_addr(get("mean"))
obj_addr(evalq(mean))
obj_addr(match.fun("mean"))
# all these fuct point to the same underlying funct
# 3
?read.csv
# make check.names param FALSE
# 4
?make.names
