### Avaliação da qualidade do ajuste usando simulação (ilustração).
### Ao invés de utilizar a distribuição qui-quadrado assintótica como referência
### para testar a qualidade do ajuste com base na deviance, vamos obter 
### a distribuição de referência para o teste via simulação.

### Passo 1: Ajuste do modelo aos dados observados
dados1 <- read.csv2('Dados - Sinistros Poisson.csv')[,-1]
head(dados1)

ajuste1 <- glm(claims ~ ., family = poisson, data = dados1) 
### Ajuste do GLM do número de sinistros pelo resto.

print(ajuste1) 
### Resumo do modelo ajustado

fit <- fitted(ajuste1); fit 
### Valores ajustados pelo modelo para cada indivíduo na amostra.

fit1 <- fitted(ajuste1)[1]; fit1 
### Valor ajustado pelo modelo para o primeiro indivíduo na amostra

devobs <- ajuste1$deviance; devobs 
### devobs armazena o desvio avaliado para o ajuste com os dados observados.

#########################################################################

### Passo 2: Simulação de y com base no modelo ajustado.

yest1 <- rpois(1,fit1); yest1 
### Número de sinistros simulado para o 1º indivíduo com base no modelo ajustado. 

yest <- rpois(500,fit); yest 
### Números de sinistros simulados para os 500 indivíduo da amostra com 
### base no modelo ajustado.


#########################################################################

### Passo 3: Ajuste do GLM aos dados simulados.
ajuste1est <- glm(yest ~ ., family = poisson, data = dados1)

dev1 <- ajuste1est$deviance; dev1 
### dev1 armazena a deviance avaliada para o ajuste com os dados simulados

#########################################################################

### Passo 4: Simulação dos passos 2 e 3 por 1000 vezes

desviossim <- numeric() 
# desviossim vai armazenar os desvios gerados nas 1000 simulações.

desviossim[1] <- dev1 
# Armazenando a primeira simulação.

for(i in 2:1000){
  
  yest <- rpois(500,fit) 
  
  ajuste1est <- glm(yest ~ ., family = poisson, data = dados1)
  desviossim[i] <- ajuste1est$deviance 
}

desviossim

### Antes do passo 5, vamos sobrepor a distribuição simulada para os desvios e
### a distribuição qui-quadrado (n-p)

hist(desviossim, freq=F, xlim=c(300,700))
curve(dchisq(x,495), from=300, to=700, add = T)
### Claramente, a aproximação com a distribuição qui-quadrado é bem ruim

lines(c(devobs,devobs), c(0,1), lty=2, col='red') 
### Representando o valor observado para a deviance.

#########################################################################

### Passo 5: Obtendo o p-value

psimulado <- sum(desviossim > devobs)/1000; psimulado 
### Logo, não há evidências de que o modelo esteja mal ajustado.

### Já se usássemos a aproximação com a distribuição qui-quadrado:

paproximado <- pchisq(devobs,495,lower.tail=F); paproximado 
### Pela aproximação com a qui-quadrado, rejeitariamos o modelo.

#########################################################################

### Vamos ver um segundo exemplo, em que as contagens (e as médias) são maiores.

### Gerando valores para x e y
set.seed(4159)
x <- round(rnorm(500, 10, 2))
y <- rpois(500, exp(x))

ajuste1 <- glm(y ~ x, family = poisson)
fit1 <- fitted(ajuste1)
devobs <- ajuste1$deviance

desviossim <- numeric()

for(i in 1:1000){
  
  yest=rpois(500,fit1) 
  
  ajuste1est=glm(yest~x,family=poisson)
  desviossim[i]=ajuste1est$deviance 
}

desviossim

hist(desviossim, freq = F, xlim = c(300,700))
curve(dchisq(x,498), from=300, to=700, add=T)
lines(c(devobs, devobs), c(0,1),lty = 2,col = 'red') 
### Representando o valor observado para a deviance.

psimulado <- sum(desviossim>devobs)/1000; psimulado 
### Logo, não há evidências de que o modelo esteja mal ajustado.

### Já se usássemos a aproximação com a distribuição qui-quadrado:

paproximado <- pchisq(devobs, 495, lower.tail = F); paproximado 
### A conclusão usando a aproximação qui-quadrado é semelhante à obtida 
### via simulação.

