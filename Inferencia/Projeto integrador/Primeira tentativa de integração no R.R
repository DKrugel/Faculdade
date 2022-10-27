#
library(extraDistr)
LogMu <-  function(mu, sigma){7/2*sum(((2*mu)-(2*x))/((x-mu)^2)+8*sigma^2)}
LogMu(8, 1)
x <- rlst(100, 8, 10, 1)
sapply(mu, LogMu(1,1))
mu.vals <- seq(11, 15, l = 100)
print(mu.vals)
-sum(x)+sigma 
#Função escore para mu dado x e sigma
ScoreMU <- function(x, sigma, mu){-7*sum((-x+mu)/((x-mu)^(2)+8*sigma^2))}
roots()
rootSolve::uniroot.all(ScoreMU(x, sigma = 1, mu = 0), interval = c(8,12))

ScoreMU(x, 1, 10)
