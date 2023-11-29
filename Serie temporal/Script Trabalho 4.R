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

num = length(y)
A = cbind(1,1,0,0)
# Função para calcular a verossimilhança
Linn =function(para){
  Phi = diag(0,4); Phi[1,1] = para[1]
  Phi[2,]=c(0,-1,-1,-1); Phi[3,]=c(0,1,0,0); Phi[4,]=c(0,0,1,0)
  cQ1 = para[2]; cQ2 = para[3] # raiz quadrada de q11 e q22
  cQ = diag(0,4); cQ[1,1]=cQ1; cQ[2,2]=cQ2
  cR = para[4] # raiz quadrada de r11
  kf = Kfilter(jj, A, mu0, Sigma0, Phi, cQ, cR)
  return(kf$like) }

# Parâmetros iniciais
mu0 = c(.7,0,0,0); Sigma0 = diag(.04,4)
init.par = c(1.03,.1,.1,.5) # Phi[1,1], 2 cQs e cR
# Estimação e resultados
est = optim(init.par, Linn,NULL, method='BFGS', hessian=TRUE, control=list(trace=1,REPORT=1))

SE = sqrt(diag(solve(est$hessian)))
u = cbind(estimate=est$par, SE)
rownames(u)=c('Phi11','sigw1','sigw2','sigv'); u

# Alisamento
Phi = diag(0,4); Phi[1,1] = est$par[1]
Phi[2,]=c(0,-1,-1,-1); Phi[3,]=c(0,1,0,0); Phi[4,]=c(0,0,1,0)
cQ1 = est$par[2]; cQ2 = est$par[3]
cQ = diag(1,4); cQ[1,1]=cQ1; cQ[2,2]=cQ2
cR = est$par[4]
ks = Ksmooth(y,A,mu0,Sigma0,Phi,cQ,cR)
# Gráficos
Tsm = ts(ks$Xs[1,,], start=1948, freq=4)
Ssm = ts(ks$Xs[2,,], start=1948, freq=4)
p1 = 3*sqrt(ks$Ps[1,1,]); p2 = 3*sqrt(ks$Ps[2,2,])
par(mfrow = c(2,1), mar=c(3,3,1,1), mgp=c(1.6,.6,0), cex=0.9, pch=19)
plot(Tsm, main='Componente de tend&ecirc;ncia', xlab="Tempo", ylab='Tend&ecirc;ncia')
xx = c(time(y), rev(time(y)))
yy = c(Tsm-p1, rev(Tsm+p1))
polygon(xx, yy, border=NA, col=gray(.5, alpha = .3))


grid()
plot(y, main='Dados & Tendência + Sazolidade', xlab="Tempo",ylab='Johnson & Johnson')
xx = c(time(y), rev(time(y)) )
yy = c((Tsm+Ssm)-(p1+p2), rev((Tsm+Ssm)+(p1+p2)) )
polygon(xx, yy, border=NA, col=gray(.5, alpha = .3))
grid()

polygon(xx,yy,  col=gray(.5, alpha = 2))

# Questão 23

library(depmixS4)
library(gamlss.data)

data <- polio

set.seed(1) # Mudando a seed pois a seed 123 não achou convergencia no modelo
model <- depmix(data ~ 1, nstates=max(data), data= data.frame(polio), family=poisson())

fit01 <- fit(model)
summary(fit01)

