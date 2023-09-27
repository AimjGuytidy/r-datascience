stdin <- file('data/stdin.txt')
open(stdin)

n <- as.integer(trimws(readLines(stdin, n = 1, warn = FALSE), which = "both"))

close(stdin)
for (i in seq(10)) {
  cat(sprintf("%1$s x %2$s = %3$s \n",n,i,n*i))
  #i=1+i
}
