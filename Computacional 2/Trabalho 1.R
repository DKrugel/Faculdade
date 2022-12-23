f_x <- function(x, theta = 1){
  exp(  -((x^2)/(2* (theta^2) * x ))/theta^2 )
}

F_x <- function(x,theta = 1){
  1-exp((-x^2)/2*theta^2 )
}

curve(f_x(x),0,10)
curve(2*dexp(x,0.5),0,10, col = "red", add = T)

M <- 2
n <- 10000
x <- numeric(n)
i <- 1
k <- 1

while(i <= n){
  y <- rexp(1,rate = 0.5)
  u <- runif(1)
  ratio <- f_x(y)/(M * dexp(y,0.5))
if( u < ratio){
  x[i] <- y
  i <- i+1
}
  k <- k+1
}
k/n


hist(x)

plot(ecdf(x)) # Acumulada empírica.
plot(density(x, from = 0, to = 10)) # Dens. empírica.

ger_ex9 <- function(n = 10000){
  # Função de probabilidade
  f_x <- function(x, theta = 1){
    exp(  -((x^2)/(2* (theta^2) * x ))/theta^2 )
  }
  M <- 2
  x <- numeric(n)
  i <- 1
  k <- 1
  
  while(i <= n){
    y <- rexp(1,rate = 0.5)
    u <- runif(1)
    ratio <- f_x(y)/(M * dexp(y,0.5))
    if( u < ratio){
      x[i] <- y
      i <- i+1
    }}
  
  return(x)

}


#########################

Fx <- function(x,theta1 = 3, theta2 = 3, theta3 = 3){
  ( 1- ( x/theta1 )^-theta2)^-theta3
  
}

curve(Fx, from = 0, to = 10)





######### 14 ###############

Fx14 <- function(x,theta1,theta2){
  1 - exp(-theta1 * exp(theta2*x))
}

fx14 <- function(x,theta = 1,pi = 2){
  log(pi)*exp(((-exp(pi^(pi*x) ) * theta) )+ pi ^ (pi*x)) * (pi ^((pi*x)+1)) * theta
}

curve(Fx14(x,1,1),from = 0, to = 10)
curve(2*dexp(x, 10), col = 'red', add = T, from = 0, to = 4)

curve(fx14(x,1,2), from = 0, to = 4)
curve(2*dexp(x, 10), col = 'red', add = T, from = 0, to = 4)

n <- 1000
M <- 2
x <- numeric(n)
i <- 1
k <- 1

while(i <= n){
  y <- rexp(1,rate = 10)
  u <- runif(1)
  ratio <- fx14(y,1,2)/(M * dexp(y,10))
  if( u < ratio){
    x[i] <- y
    i <- i+1
  }}

ger_ex14 <- function(n = 10000){
  # Função de probabilidade
  fx14 <- function(x,theta = 1,pi = 2){
    log(pi)*exp(( (-exp(pi^(pi*x) ) * theta) )+ pi ^ (pi*x)) * (pi ^((pi*x)+1)) * theta
  }
  
  M <- 2
  x <- numeric(n)
  i <- 1
  k <- 1
  
  while(i <= n){
    y <- rexp(1,rate = 10)
    u <- runif(1)
    ratio <- fx14(y,1,2)/(M * dexp(y,10))
    if( u < ratio){
      x[i] <- y
      i <- i+1
    }
    k <- k + 1
    }
  return(x)
}
e14 <- ger_ex14()
hist(e14)
plot(ecdf(e14))



curve(fx14(x,1,2), col = "red")

x <- runif(100,0,1)
iF14 <- function(u, theta1, theta2){
  return(log(-log(1-u)/theta1)/theta2)
}
head(iF14(x,1,2))

plot(function(x)fx14(x,1,2), from = -2,2)
lines(density(iF14(seq(0,1,by = 0.001),1,2)),add = T)
