# creating a random draw of 1000 numbers from a uniform distribution
set.seed(42)
u <- runif(1000)
max(u)*3 + 2 # converting the draw from 0 to 1 to 2 to 5
min(u)*3 + 2
