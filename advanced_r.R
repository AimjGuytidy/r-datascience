# install.packages("bench")
library(bench)
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

# for character vectors there comes a new concept of global string pool, which is 
# close to self explanatory.

c1 <- c("a","a","jf","k")
ref(c1,character = TRUE)
n1<-c(1,1,3,4)
ref(n1)
obj_size(c1)
obj_size(n1)

# 2.3.6 Exercises #

# 1
# tracemem(1:10) is not useful because the object is not bound to a name hence 
# there won't be consequences when the vector is modified!

# 2
x <- c(1L,2L,3L)
tracemem(x)
x[[3]] <- 4 # here we have directly modified the object that was bound to the name
            # x, hence we get a different object bound to x

# 3
a <- 1:10
b <- list(a,a)
c <- list(b,a,1:10)
ref(b,a)
ref(a,b)
ref(a,c)
# the relationship will follow the logic that we have 2 different objects: lists 
# and vectors, hence we should be mindfull of that!

# 4
x <- list(1:10)
x[[2]] <- x
ref(x,x)

# 2.4 Object size #
###################

obj_size(letters)
obj_size((ggplot2::diamonds))

x <- runif(1e6)
obj_size(x)
y <- list(x,x,x)
obj_size(y)
obj_size(list(NULL,NULL,NULL))

banana <- "bananas bananas bananas"
obj_size(banana)
obj_size(rep(banana,1000))

obj_size(x,y)

# every sequence no matter how large is the same size since r only store the first 
# and the last numbers of the sequences, this is known as alternative representation

obj_size(1:3)
obj_size(1:1e6)

# 2.4.1 Exercises #
###################

# 1 
?object.size
?obj_size
y<-rep(list(runif(1e4)),100)
object.size(y) # this works well with atomic vectors!!!
obj_size(y)

# 2
funs <- list(mean,sd,var)
obj_size(funs)
obj_size(mean,sd,var)
obj_size(mean)
obj_size(sd)
obj_size(var)
# the size for each individual fun when added up together it doesn't match with
# the size provided by obj_size, this size understate the real size.

# 3
a <- runif(1e6)
obj_size(a)

b <- list(a,a)
obj_size(b)
obj_size(a,b)

b[[1]][[1]] <- 10
obj_size(b)
obj_size(a,b)

b[[2]][[1]] <- 10
obj_size(b)
obj_size(a,b)

# 2.5 Modify-in-place #
#######################

v <- c(1,2,3)
tracemem(v)
v[[3]] <- 4
# here the modification happens in place because we are modifying an object with one 
# single name bound to it, so instead of creating a copy, it bind the name to a 
# new object and discard the old object!

x <- data.frame(matrix(runif(5*1e4),ncol=4))
medians <- vapply(x,median,numeric(1))
for (i in seq(ncol(x))) {
  x[[i]]<-x[[i]] - medians[[i]]
}

# for (i in seq_along(medians)) {
#   x[[i]]<-x[[i]] - medians[[i]]
# }
# 
# for (i in seq_along(medians)) {
#   print(i)
# }
# for (i in seq(ncol(x))) {
#   print(i)
# }

cat(tracemem(x),"\n")
for (i in seq_along(medians)) {
  x[[i]] <- x[[i]] - medians[[i]]
}
untracemem(x)

y <- as.list(x)
cat(tracemem(y),"\n")
for (i in seq_along(medians)) {
  y[[i]] <- y[[i]] - medians[[i]]
}

# 2.5.2 Environments #
######################
# environments are always modified in place (reference semantics)

e1 <- rlang::env(a = 1, b = 2, c = 3)
#cat(tracemem(e1),"\n") this is not useful for promise and environment objects
e2 <- e1

e2$b <- 1
e1$b

# environments can contain themselves!

e <- rlang::env()
e$self <- e
ref(e)

# 2.5.3 Exercises#

# 1
x <- list()
cat(tracemem(x),"\n")
x[[1]] <- x # this is not circular because we initially had an empty list object
            # now we have added an element that was bounded to a list object

# 2
fun1 <- function(a) {
  for (i in seq_along(medians)) {
    a[[i]] <- a[[i]] - medians[[i]]
  }
}

fun2 <- function(a) {
  a1 <- as.list(a)
  for (i in seq_along(medians)) {
    a1[[i]] <- a1[[i]] - medians[[i]]
  }
}

bench::bench_time(fun1(x))
bench_time(fun1(y))

# 3 
# using tracemem() on environment yields an error specifying that it can not be 
# applyed on environment and promise

# 2.6 Unbinding and the garbage collector #
###########################################
x <- 1:4
cat(tracemem(x),"\n")
x <- 3:6
tracemem(x)
rm(x)
gc()
lobstr::mem_used()
# use gcinfo() if you want R to print info everytime garbage collector is in action

