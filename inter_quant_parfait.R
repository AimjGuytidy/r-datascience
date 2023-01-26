# Basics of R----
print("Hello World")
print(5^8)
print(10000%%57)
print(10000000000)
print(100000)

# Scientific notation homework####
#when does R print in scientific notation
print(124314145232) # at 12 digits no scientific notation
#vs
print(1243141452324) # at 13 digits we see scientific notation

#stopping R from using scientific format

format(10000000000,scientific=FALSE) #using format fct

options(scipen = 999) #using options fct
print(10000000000)

#convert from scientific notation to standard notation

format(1.243141e+12,scientific=FALSE) #using format fct

options(scipen = 999) #using options fct
print(1.243141e+12)

# Data types homework####

a<- 7  # numeric or double
typeof(a)
class(a)
b<- 900.8 # numeric or double
typeof(b)
class(b)
c <- a+b # numeric or double
typeof(c)
class(c)
fruits <- "I like fruits a lot " # character
typeof(fruits)
class(fruits)
f<- TRUE # logical
typeof(f)
class(f)
vector_1<-c(1, 2, 3,5) #numeric or double
typeof(vector_1)
class(vector_1)
vector_2<-c("a", "b", "c", "d") #character
typeof(vector_2)
class(vector_2)
bc <- NA # logical
typeof(bc)
class(bc)

#Functions in R####

my_function <- function(x){
  return (sqrt(x)/log(x))
}
fun1 <- my_function
my_function(100)
fun1(100)
division_fun <- function(x,y){
  return(x/y)
}

funcy <- function(x,y){
  return((x-y)/(x+y))
}
funcy(54,65)
