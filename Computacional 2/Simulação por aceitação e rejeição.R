#Simulação por aceitação e rejeição

# Densidade da X, distribuição alvo.
f_X <- function(x, r) {
  b <- r; a <- -r; c <- 0
  f <- 0 + 2 * (x - a)/((b - a) * (c - a)) * (x <= c) * (x > a) +
    2 * (b - x)/((b - a) * (b - c)) * (x > c) * (x < b)
  return(f)
}
# Densidade da Y, distribuição candidata.
f_Y <- function(y, r) {
  f <- 1/(2 * r) * (y > -r) * (y < r)
  return(f)
}
# Gráficos.
curve(f_X(x, r = 1), from = -1, to = 1)
curve(f_Y(y, r = 1), xname = "y", add = TRUE, col = 2, lty = 2)
curve(2 * f_Y(y, r = 1), xname = "y", add = TRUE, col = 2)

M <- 2
n <- 10000
x <- numeric(n)
i <- 1
k <- 1
while (i <= n) {
  y <- runif(1, min = -1, max = 1)
  u <- runif(1)
  ratio <- f_X(y, r = 1)/(M * f_Y(y, r = 1))
  if (u < ratio) {
    x[i] <- y
    i <- i + 1
  }
  k <- k + 1
}
n/k # Proporção de aceitação.
plot(ecdf(x)) # Acumulada empírica.
plot(density(x, from = -1, to = 1)) # Dens. empírica.
curve(f_X(x, r = 1), add = TRUE, col = 2) # Dens. teórica.

