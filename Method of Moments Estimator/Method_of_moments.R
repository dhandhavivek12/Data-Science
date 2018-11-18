#Project By Kushal Doshi, Malhar Khimasaria and Vivek Dhandha

#Normal Distribution
Normal <- function()
{
  cat("Example: 
      n <- 1000
      mean <- 10
      sd <- 3
      normlist <- rnorm(n,mean = mean,sd = sd)
      \n Considering the above example..")
  n <- as.integer(readline(prompt="Enter value of n: "))
  mean <- as.integer(readline(prompt="Enter Value of mean: "))
  sd <- as.integer(readline(prompt="Enter Value of sd: "))
  X <- rnorm(n,mean = mean,sd = sd)
  y <- sample(X,500)
  n <- length(y)
  mu <- sum(y)/n
  sd2 <- sum((y-mu)^2)/n
  sigma_sq <- sd2^0.5
  cat("mu = ",mu," and sigma sq = ",sigma_sq)
}

#Exponential Distribution
Exponential <- function()
{
  cat("Example: 
      n <- 10000
      rate <- 1
      explist <- rexp(n,rate)
      \n Considering the above example..\n")
  n <- as.integer(readline(prompt="Enter value of n: "))
  rate <- as.integer(readline(prompt="Enter Value of rate: "))
  X <- rexp(n,rate)
  y <- sample(X,100)
  beta = var(y)
  cat("beta = ",beta)
}


#Gamma Function
gamma <- function(X)
{
  cat("Example: 
      x <- 1000
      shape <- 2
      rate <- 1
      gammaList <- rgamma(1000,2,1)
      \n Considering the above example..\n")
  x <- as.integer(readline(prompt="Enter value of x: "))
  shape <- as.integer(readline(prompt="Enter Value of shape: "))
  rate <- as.integer(readline(prompt="Enter Value of rate: "))
  X <- rgamma(1000,2,1)
  y <- sample(X,1000)
  alpha <- (mean(y)^2)/var(y)
  beta <- (mean(y)/alpha)
  cat("a = ",alpha," & b = ",beta)
}

#Uniform Distribution
Uniform <- function()
{
  cat("Example: 
      n <- 1000
      min = 1
      max = 5
      uniList <- runif(1000,1,5)
      \n Considering the above example..\n")
  n <- as.integer(readline(prompt="Enter value of n: "))
  min <- as.integer(readline(prompt="Enter Value of min: "))
  max <- as.integer(readline(prompt="Enter Value of max: "))
  X <- runif(n,min,max)
  y = sample(X,1000)
  mu = mean(y)
  var1 = var(y)
  i = (12*var1)^0.5
  b = ((2*mu) + i)/2
  a = ((2*mu) - i)/2
  cat("a = ",a," & b = ",b)
}


#Binomial Distribution
Binomial <- function(){
  cat("Example: 
      n <- 1000
      size = 10
      prob = 0.6
      binomList = rbinom(1000,10,0.6)
      \n Considering the above example..\n")
  n <- as.integer(readline(prompt="Enter value of n: "))
  size <- as.integer(readline(prompt="Enter Value of size: "))
  prob <- as.double(readline(prompt="Enter Value of prob: "))
  X <- rbinom(n,size,prob)
  sam = sample(X,1000)
  x = mean(sam)
  y = var(sam)
  n = x^2/(x-y)
  p = (x-y)/x
  cat("n =  ",n," & p =",p)
}


#Beta Distribution
Beta <- function(X)
{
  cat("Example: 
      n <- 1000
      shape1 = 1
      shape2 = 2
      binomList = rbeta(1000,1,2)
      \n Considering the above example..\n")
  n <- as.integer(readline(prompt="Enter value of n: "))
  shape1 <- as.integer(readline(prompt="Enter Value of shape1: "))
  shape2 <- as.double(readline(prompt="Enter Value of shape2: "))
  X = rbeta(n,shape1,shape2)
  sam = sample(X,1000)
  mu = mean(sam)
  v = var(sam)
  a = ((mu^2) - (mu^3) - (mu*v))/v
  b = (mu - 2*(mu^2) + (mu^3) - v + (mu*v))/v
  cat("alpha = ",a," & beta = ",b)
}


# Geometric Disribution
Geometric <- function(X){
  cat("Example: 
      n <- 1000
      prob = 0.9
      geomList <- rgeom(1000,0.9)
      \n Considering the above example..\n")
  n <- as.integer(readline(prompt="Enter value of n: "))
  prob <- as.double(readline(prompt="Enter Value of prob: "))
  library(EnvStats)
  geomList <- rgeom(n,prob)
  p <- egeom(geomList,method="mle/mme")$parameter
  # sam = sample(X,1000)
  #  mu = mean(sam)
  # val = 1/mu
  cat("Mean = ",p)
}


#Bernoulli Distribution
Bernoulli <- function(X){
  cat("Example: 
      n <- 1000
      prob = 0.4
      x <- rbern(1000,0.4)
      \n Considering the above example..\n")
  n <- as.integer(readline(prompt="Enter value of n: "))
  prob <- as.double(readline(prompt="Enter Value of prob: "))
  library(Rlab)
  X <- rbern(n,prob)
  y = sample(X,1000)
  mu = mean(y)
  cat("p = ",mu)
}


#Poisson Function
poissonFunc <- function(X)
{
  cat("Example: 
      n <- 1000
      lambda = 1
      poissonList = rpois(1000,1)
      \n Considering the above example..\n")
  n <- as.integer(readline(prompt="Enter value of n: "))
  lambda <- as.double(readline(prompt="Enter Value of lambda: "))
  X <- rpois(n,lambda)
  y = sample(X,100)
  lambda = mean(y)
  cat("lambda = ",lambda)
}

chiSquareFunc <- function(X)
{
  cat("Example: 
      n <- 1000
      df = 10
      chiSquareList = rchisq(1000, 10, ncp = 0)
      \n Considering the above example..\n")
  n <- as.integer(readline(prompt="Enter value of n: "))
  df <- as.integer(readline(prompt="Enter Value of df: "))
  X <- rchisq(n,df,ncp = 0)
  y = sample(X,100)
  p = mean(y)
  cat("p = ",p)
}

pointmass <- function(x){
  cat('Taking Sample between 1 and 100 \n')
  x <- sample(1:100,1)
  p = mean(x)
  cat("p = ",p)
}


Tdist<-function(){
  cat("Example: 
      n <- 1000
      df = 7
      x<-rt(1000,7)
      \n Considering the above example..\n")
  n <- as.integer(readline(prompt="Enter value of n: "))
  df <- as.integer(readline(prompt="Enter Value of df: "))
  x<-rt(n,df)
  y<-var(x)
  df=2*y/(y-1)
  cat("For T Distribution the parameter 'degree of freedom' is",df)
}

multinomial<-function(){
  x<-rmultinom(1000,size=5,c(0.15,0.05,0.4,0.1,0.3))
  a=nrow(x)
  p<-c(0,0,0,0,0)
  for(i in 1:a)
    p[i]<-1-((var(x[i,]))/mean(x[i,]))
  n=sum(rowMeans(x))/sum(p[1:a])
  #n=mean(c(rowMeans(x)/p[1:a]))
  cat("For Multinomial Distribution the parameter 'n' is",n, "and the parameter 'p' is ",p)
  
}

multivariate <- function(){
  Sigma <- matrix(c(10,3,3,2),2,2)
  x<-mvrnorm(n = 1000, rep(0, 2), Sigma)
  mu<- colMeans(x)
  summ<- var(x)
  cat("For Multivariate Normal Distribution the parameters 'mu' is",mu," and Summation is",summ)
}


library(MASS)
library(Rlab)
library(EnvStats)
repeat{
  cat("\n")
  cat("\n")
  cat("1. Normal Distribution\n2. Exponential Distribution \n3. Gamma Distribution \n4. Uniform Distribution \n5. Binomial Distribution \n6. Bernoulli Distribution \n7. Geometric Distribution \n8. Beta Distribution \n9. Poisson Distribution \n10.ChiSquare Distribution \n11.Point Mass \n12.T Distribution \n13.Multinomial Distribution \n14.Multivariate Normal Distribution \n15.Exit\n")
  choice <- readline(prompt="Enter your distribution  ")
  
  switch(choice,
         "1"=Normal(),
         "2"=Exponential(),
         "3"=gamma(),
         "4"=Uniform(),
         "5"=Binomial(),
         "6"=Bernoulli(),
         "7"=Geometric(),
         "8"=Beta(),
         "9"=poissonFunc(),
         "10"=chiSquareFunc(),
         "11"=pointmass(),
         "12"=Tdist(),
         "13"=multinomial(),
         "14"=multivariate()
         
  )
  if(choice==15)
    break;
}
cat('Thank You!')
