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
  y <- runif(1,min = 0, max = 10)
  u <- runif(1)
  ratio <- f_x(y)/(M * dexp(y,0.5))
if( u < ratio){
  x[i] <- y
  i <- i+1
}
  k <- k+1
}

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
    y <- runif(1,min = 0, max = 10)
    u <- runif(1)
    ratio <- f_x(y)/(M * dexp(y,0.5))
    if( u < ratio){
      x[i] <- y
      i <- i+1
    }}
  
  return(x)

}


