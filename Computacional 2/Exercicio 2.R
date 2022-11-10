pop <- rbinom(100000,100,0.5)
pop <- runif(1000)
n <- 1000 
N <- 10000
x <-  replicate(N,mean(sample(n,pop)))
hist(x)
g.num <- function(n = 100,N = 10000, pop = rbinom(100000,100,0.5)){
  x <-  replicate(N,mean(sample(pop,n)))
  hist(x)
}

g.num()



x <- numeric(1000L)

for(i in 1:length(x)){
  y <- rbinom(100000,100,0.5)
  x[i] <- mean(y)
}

hist(x)



###############
###############
##############

Y <- rnorm(1000)
sam. <- replicate(10000,var(sample(Y,12)))
hist(sam.)

GNvar <- function(n = 10,N = 1000, pop = rnorm(1000)) {
  sam. <- replicate(N,var(sample(pop,n)))
  hist(sam., freq = FALSE)
  curve(dchisq(x,(n-1)), from = min(sam.), to = max(sam.), add = TRUE)
  
}

GNvar()

curve(dchisq(x,(n-1)), from = 0, to = 50, add = TRUE)

      