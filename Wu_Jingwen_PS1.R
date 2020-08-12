# 1a
value <- function(n){
  if (n<=2){
    return(1)
  }
  else{
    a = 1
    b = 1
    i = 1
    while (i<=(n-2)){
      c = a+b+2*i
      a = b
      b = c
      i = i+1
    }
    return(c)
  }
}

n <- 36
cat("The value of A",n,"is",value(n),".")

# 1b.1recursive function
co <- function(n,m){
  if (n==m || m==0){
    return(1)
  }
  else{
    return(co(n-1,m-1)+co(n-1,m))
  }
}

n <- 88
m <- 44
cat("The result is",co(n,m))   #The processing time was too long.

# 1b.2formular
co2 <- function(n,m){
  seq1 = ((n-m+1):n)
  a = 1
  for (i in seq1){
    a = a*i
    i = i+1
  }
  b = 1
  seq2 = (2:m)
  for (i in seq2){
    b = b*i
    i = i+1
  }
  return(a/b)
}

n <- 88
m <- 44
cat("The result is",co2(n,m))

# 1c
gcd <- function(x,y){
  seq1 = (2:min(x,y))
  for (i in seq1){
    if (x%%i==0 && y%%i==0){
      result1 = i
    }
    i = i+1
  }
  return(result1)
}

x <- 12306
y <- 32148
cat("The greatest common divisor of",x,"and",y,"is",gcd(x,y))
cat("The least common multiple of",x,"and",y,"is",x*y/gcd(x,y))

# 2a
library(readr)
WHO <- read_csv("C:/NYU SH/Machine Learning/Problem Set 1/WHO.csv")
View(WHO)
i = 1
while (i<=length(WHO)){
  if (sum(is.na(WHO[,i]))>=3){
    cat(names(WHO[,i])," ")
  }
  i = i+1
}

# 2b
WHO$Country[which.max(WHO$FertilityRate)]
WHO$Country[which.min(WHO$FertilityRate)]

# 2c
sdGNI = tapply(WHO$GNI, WHO$Region, sd, na.rm=TRUE)
cat(names(sdGNI[which.min(sdGNI)]),min(sdGNI))

# 2d
WHO$RichCountries = as.numeric(WHO$GNI > 20000, na.rm=TRUE)
tapply(WHO$ChildMortality, WHO$RichCountries, mean, na.rm=TRUE)

# 2e
plot(WHO$GNI, WHO$LifeExpectancy, xlab = "Income Level", ylab = "Life Expectancy", main = "The relationship between income level vs. life expectancy")
