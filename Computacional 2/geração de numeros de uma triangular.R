rtri <- function(n){
  replicate(n,{
    sum(runif(2))
  })
}
x <- rtri(100000)
hist(x, breaks = 100)

#-----------------------------------------------------------------------
# Triagular.

# Função densidade de probabilidade.
fx <- function(x) {
  ifelse(x < 0, 1 + x, 1 - x) * (-1 < x) * (x < 1)
}

# Gráfico da função.
curve(fx, -1, 1, add = TRUE)

#--------------------------------------------
# Gerando números.

# Usando a soma de duas uniformes, i.e. ~U(-1/2, 1/2).
t <- replicate(1000, sum(runif(2, -0.5, 0.5)))

# Poderia-se fazer em um laço também.
# t <- numeric(1000)
# for(i in 1:length(t)) {
#     t[i] <- sum(runif(2))
# }

plot(density(t, from = -1, to = 1))
curve(fx, add = TRUE, from = -1, to = 1, col = 2)

# O melhor é verificar na distribuição acumulada.
Fx <- function(x) {
  ifelse(x < 0,
         x^2/2 + x + 1/2,
         -x^2/2 + x + 1/2)
}

plot(ecdf(t))
curve(Fx, add = TRUE, col = 2)


iFx <- function(u) {
  ifelse(u < 0.5,
         sqrt(2 * u) - 1,
         1 - sqrt(2 - 2 * u))
}

curve(iFx, 0, 1)


library(latticeExtra)
pteo <- ppoints(n = length(t), a = 1/2)
qteo <- iFx(pteo)

qobs <- sort(t)
pobs <- Fx(x = qobs)

# pp-plot.
xyplot(pobs ~ pteo, col = 1, cex = 0.2) +
  layer(panel.abline(a = 0, b = 1, col = 2))
