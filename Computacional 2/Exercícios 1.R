# http://cursos.leg.ufpr.br/ce089/exercicios/exercicios01.html

#Exercicio 1
(32/2)+4

((3^2)*5)+2

(39-(4^2))/5

((exp(1)^-2)*(2^3))/factorial(3)

5+log(8, base =2)

#Exercício

a <- c(12,14,16,11,12,18,15,12,15)

b <- c(3,7,11,15,19,23)

c <- c(1,2,4,7,11,16,22,29,37,46,56,67)

#b <- ((0:5)*4) + 3

length(unique(a))

duplicated(a)

#Quais os valores em a) maiores que 13 ?
#Utilizar o wich me retorna a posição, usar o [] me indexa o vetor
a[which(a > 13)]

i <- 0
ab <- a
for(i in 0:length(b)){
  ab[i] <- ab[i]+b[i]
}
ab

a[which(a%%3 == 0)]

#Tabela de frequência dos valores do vetor a
table(a)

a <- c(15,12,14,16,11,12,18,15,12,15,15)

which.max(table(a))

altura <- function(lado){
  h <- (sqrt(3)*lado)/2
  return(h)
}
altura(2)

area <- function(lado){
  a <- (lado*altura(lado))/2
}
ger <- function(p,N){

u <- numeric(200)
u[1] <- 0

for(i in 2:N){
  if(runif(1)<p){
    u[i] <- u[i-1]
  }else{
    u[i] <- abs(1-u[i-1])
  } }
p.est <- 1 - mean(abs(diff(sim1)))
return(u)
}


p.est <- function(p,N){
  
  u <- numeric(200)
  u[1] <- 0
  
  for(i in 2:N){
    if(runif(1)<p){
      u[i] <- u[i-1]
    }else{
      u[i] <- abs(1-u[i-1])
    } }
  u.est <- 1 - mean(abs(diff(u)))
  return(u.est)
}

p.est(0.5,200)

sim1 <- ger(0.5, 200)

#A função diff faz cada elemento menos o anterior
#A função Abs pego o absoluto dos elementos

mean(abs(diff(sim1))) #Estimador de probabilidade de transição da função, sem nenhuma teoria estatística por trás

est <- replicate(10000, p.est(0.3,200))
head(est)
mean(est)
var(est)
sd(est)

#Gerando a consistencia do estimador
n <- seq(50,1000, by = 50)
variancia <- numeric(length(n))
pos <- 1
variancia <- var(replicate(1000, p.est(0.3,n)))
for(pos in 1:length(n)){
  print(pos)
  variancia[pos] <- var(replicate(10000, p.est(0.3,n[pos])))
  
}

plot(variancia ~ n, type = 'l', main = 'Plot do valor da variancia de 10000 replicações dado N amostras',
     xlab = 'Num de amostras em cada repetição', ylab = 'Variância das simulações')
