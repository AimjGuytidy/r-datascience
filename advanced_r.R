
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

# The character "X" is prepended if necessary. 
# All invalid characters are translated to ".". 
# A missing value is translated to "NA". 
# Names which match R keywords have a dot appended to them. 
# Duplicated values are altered by make.unique.


# 2.3 COPY-ON-MODIFY#
#####################

X <- c(1,3,4)
obj_addr(X)
y <- X  
obj_addr(y)
y[3] <- 2
obj_addr(y) # copy-on-modify in action: now y has been appended to a new object with 
            # this new address, this means that R objects are immutable!!!

# 2.3.1 tracemem() #
####################

cat(tracemem(X),"\n")
y <- X
y[[3]] <- 2
untracemem(y)

# 2.3.2 FUNCTION CALLS #
########################

f <- function(a) {
  a
}

x <- c(1,3,4)
cat(tracemem(x),"\n")
z <- f(x) # there is no copy!!!

untracemem(x)

# 2.3.3 Lists #
###############

l1 <- list(1,2,3)
cat(tracemem(l1),"\n")
l2 <- l1
l2[[3]] <- 8

ref(l2,l1)
untracemem(l1)

# 2.3.4 Data frames #
#####################

# a data frame is a list of vectors!!! so when you change an element in one column
# that specific column get copied but if you change a row then all columns get 
# copied!!

# 2.3.5 Character vectors #
###########################

