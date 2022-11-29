fx_gamma <- function(x, alpha = 5, beta = 5){
  ((beta^alpha)/gamma(alpha))*(x^alpha-1)*exp(-beta*x)
}

curve(fx_gamma, -5,5)

curve(dgamma(x,5,5),0,5)
curve(1.25*dweibull(x,2.1,1.1), add = TRUE, col = 'red')  

M <- 1.25
n <- 10000
x <- numeric(n)
i <- 1
k <- 1
while (i <= n) {
  y <- runif(1, min = 0, max = 5)
  u <- runif(1)
  ratio <- dgamma(y,5,5)/(M * dweibull(y,2.1,1.1))
  if(y>3){
    next
  }
  if (u < ratio) {
    x[i] <- y
    i <- i + 1
  }
  k <- k + 1
}

n/k # Proporção de aceitação.
plot(ecdf(x)) # Acumulada empírica.
plot(density(x, from = 0, to = 5)) # Dens. empírica.
curve(dgamma(x,5,5), add = TRUE, col = 2) # Dens. teórica.

