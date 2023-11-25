library(astsa)
# gerando os dados
set.seed(123); 
x <- numeric(100)
x[1] <- rnorm(1,0,2.78)

for(i in 2:100){
  x[i] <- x[i-1]*0.8 + rnorm(1)
}

v <- rnorm(100,0,1)
y <- x + v

num = 100

# filtro e alisamento (Ksmooth0 faz ambos)

ks = Ksmooth(y, A=1, mu0=0, Sigma0=1, Phi=1, sQ=1, sR=1)
  
par(mfrow=c(3,1), mar=c(3,3,1,1), mgp=c(1.6,.6,0), pch=19); Time = 1:num
plot(Time, x, main='Preditos', ylim=c(-5,10), ylab=expression(X[t]))
lines(ks$Xp); grid()
lines(ks$Xp+2*sqrt(ks$Pp), lty=2, col=4)
lines(ks$Xp-2*sqrt(ks$Pp), lty=2, col=4)
plot(Time, x, main='Filtro de Kalman', ylim=c(-5,10), ylab=expression(X[t]))
lines(ks$Xf); grid()
lines(ks$Xf+2*sqrt(ks$Pf), lty=2, col=4)
lines(ks$Xf-2*sqrt(ks$Pf), lty=2, col=4)
plot(Time, x, main='Alisamento de Kalman', ylim=c(-5,10), ylab=expression(X[t]))
lines(ks$Xs); grid()
lines(ks$Xs+2*sqrt(ks$Ps), lty=2, col=4)
lines(ks$Xs-2*sqrt(ks$Ps), lty=2, col=4)

df1 <- data.frame("x1" = x[1],
                  "X0n" = ks$X0n,
                  "P0n" = ks$P0n)

df2 <- data.frame("Y" = y[1:10],
                 "Xp" = ks$Xp[1:10],
                 "Xf" = ks$Xf[1:10],
                 "Xs" = ks$Xs[1:10],
                 "Pp" = ks$Pp[1:10],
                 "Pf" = ks$Pf[1:10],
                 "Ps" = ks$Ps[1:10])

# Questão 14

y = aggregate(unemp, nfrequency = 4, FUN = mean) # Fornecido pelo enunciado

# Questão 23

library(depmixS4)
library(gamlss.data)

data <- polio

set.seed(1) # Mudando a seed pois a seed 123 não achou convergencia no modelo
model <- depmix(data ~ 1, nstates=max(data), data= data.frame(polio), family=poisson())

fit01 <- fit(model)
summary(fit01)

