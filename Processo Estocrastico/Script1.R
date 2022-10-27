# Criando a função
rw.Metropolis <- function(x, sigma, x0, N) {
  f <- function(x) abs(cos(x))*exp(-abs(x))
  x <- numeric(N)
  x[1] <- x0
  k <- 0
  u <- runif(N)
  for(i in 2:N) {
    z <- rnorm(1, mean = 0, sd = sigma)
    y <- x[i - 1] + z
    alpha <- min(f(y)/f(x[i - 1]), 1)
    u <- runif(1)
    if(u <= alpha) {
      x[i] <- y
    } else {
      x[i] <- x[i - 1]
    }
  }
  return(x)
}

rw1 <- rw.Metropolis(0,0.1,0,100000)
rw2 <- rw.Metropolis(0,2,0,100000)
rw3 <- rw.Metropolis(0,5,0,100000)
rw4 <- rw.Metropolis(0,10,0,100000)

par(mfrow = c(2,2))
plot(rw1[0:1000], type = "l")
plot(rw2[0:1000], type = "l")
plot(rw3[0:1000], type = "l")
plot(rw4[0:1000], type = "l")

par(mfrow = c(2,2))
hist(rw1, breaks = 100, freq = FALSE)
curve(abs(cos(x))*exp(-abs(x)), add = TRUE)

hist(rw2, breaks = 100, freq = FALSE)
curve(abs(cos(x))*exp(-abs(x)), add = TRUE)

hist(rw3, breaks = 100, freq = FALSE)
curve(abs(cos(x))*exp(-abs(x)), add = TRUE)

hist(rw4, breaks = 100, freq = FALSE)
curve(abs(cos(x))*exp(-abs(x)), add = TRUE)


## Fora da função
x0 <- 0
N <- 100000
sigma <- 2
f <- function(x) abs(cos(x))*exp(-abs(x))
x <- numeric(N)
x[1] <- x0
k <- 0
u <- runif(N)
for(i in 2:N) {
  z <- rnorm(1, mean = 0, sd = sigma)
  y <- x[i - 1] + z
  alpha <- min(f(y)/f(x[i - 1]), 1)
  u <- runif(1)
  if(u <= alpha) {
    x[i] <- y
  } else {
    x[i] <- x[i - 1]
  }
}

par(mfrow = c(1,1))
plot(ecdf(x))
curve((1/2)*sign(x)+exp(-x*sign(x))*(sin(x)*(sign(cos(x))/(((-sign(x))^2)+1))-cos(x)*sign(x)*((sign(cos(x)))/((-sign(x))^2+1))), from = -10, to = 10)
