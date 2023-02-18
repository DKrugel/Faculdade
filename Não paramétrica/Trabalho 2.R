# Questão 1

n <- replicate(18, FALSE)
nsucess <- sum(n == TRUE)
p <- 0.25
p_hat <- nsucess/length(n)

sigma <- sqrt( (p*(1-p))/length(n)) 

Z <- (p_hat - p)/sigma

pnorm(-1.96)> pnorm(Z) # rejeita-se a hipótese Alternativa 

P_Y <- c(0.0056,0.0338,0.0958,0.1704,0.2130,0.1988,0.1436,0.0820,0.0376,
         0.0139,0.0042,0.0010,0.0002,replicate(6,0))
df <- data.frame(P_Y)



# Questâo 2
# Teste binomial para comparação de duas proporções

n <- c(replicate(8,FALSE), replicate(8, TRUE))
nsucess <- sum(n == TRUE)
p <- 0.40
p_hat <- nsucess/length(n)

sigma <- sqrt( (p*(1-p))/length(n)) 

Z <- (p_hat - p)/sigma

pnorm(-1.96)> pnorm(Z) # rejeita-se a hipótese nula 

P_Y <- c(0.0003,
         0.0030,
         0.0150,
         0.0468,
         0.1014,
         0.1623,
         0.1983,
         0.1889,
         0.1417,
         0.0840,
         0.0392,
         0.0142,
         0.0040,
         0.0008,
         0.0001,
         0.0000,
         0.0000)


# Questão 3
# Mesmo teste da questão 1 e 2

n <- c(replicate(12,FALSE), replicate(3, TRUE))
nsucess <- sum(n == TRUE)
p <- 0.40
p_hat <- nsucess/length(n)

sigma <- sqrt( (p*(1-p))/length(n)) 

Z <- (p_hat - p)/sigma

pnorm(-1.96) > pnorm(Z) # rejeita-se a hipótese alternativa 

# Questão 5
library(tidyverse)
dados <- c(20.5,40,60.5,22,42,62,28,43,62.8,29.5,43.5,63,32,44,66,34,48,72,35,52,83,38,54,83.8,39.8,57,90,40,60,110)

sumario <- summary(dados)
sd(dados)
t_tabelado <- 1.303
EPM <- t_tabelado*sd(dados)/sqrt(length(dados))

lower <- as.numeric(sumario[5]) - EPM
upper <- as.numeric(sumario[5]) + EPM
round(data.frame(lower, upper, row.names = "80% IC"),4)

# Interpretação: podemos afirmar com 80% de confiança que o terceiro quartil da distribuição de alturas
# das árvores daquela região está localizado entre 54,20 e 69,80 metros. Isso significa que 80%
#   das alturas das árvores daquela região estão nesse intervalo.

# Questão 6

p <- (120-10)/4
dados.6 <-c(replicate(7, '(10,37.5]'), replicate(17, '(37.5,65]'), replicate(5, '(65,92.5]'), replicate(1, '(92.5,120]'))
xx <- table(dados.6)

chisq.test(xx)

# Questão 7
sumario <- summary(dados)
Q1 <- as.numeric(sumario[2])
Q2 <- as.numeric(sumario[3])
Q3 <- as.numeric(sumario[5])

freq <- c(sum(dados < Q1), sum(dados < Q2 & dados > Q1), sum(dados < Q3 & dados > Q2), sum(dados > Q3))
df <- t(data.frame(freq))
colnames(df) <- c(1:4)  

ks.test(df, pnorm)

#Questão 8
ks.test(df, pexp)

#Questão 9

dados <- c(1,1.2,2.2,2.5,2.7,3,3.6,3.8,4.9,6.1,6.6,6.7,10.2,11.5,12.3,14.8,20.8,26.1,31.3,31.5)

esperança <- mean(dados)
lambda <- 1/esperança

Q1 <- -(1/lambda)*log(0.75)
Q2 <- -(1/lambda)*log(0.5)
Q3 <- -(1/lambda)*log(0.25)
freq <- c(sum(dados < Q1), sum(dados < Q2 & dados > Q1), sum(dados < Q3 & dados > Q2), sum(dados > Q3))
df <- t(data.frame(freq))
colnames(df) <- c(1:4)  


chisq.test(df)

# # Questão 11
# s.d <- sd(dados)
# n <- length(dados)
# media <- mean(dados)
# y <- seq(0:20)
# p <- numeric(length(y))
# 
# for(i in 1:length(y)){
#   p[i] <- sum(dados[dados == y[i]])/n
# }

P <- c(
0.0000,
0.0000,
0.0002,
0.0011,
0.0046,
0.0148,
0.0370,
0.0739,
0.1201,
0.1602,
0.1762,
0.1602,
0.1201,
0.0739,
0.0370,
0.0148,
0.0046,
0.0011,
0.0002,
0.0000,
0.0000)

df <- data.frame(P)
alpha_1 <- 0.0207
alpha_2 <- 0.0207
ic_confint <- 1-alpha_1-alpha_2

# Questão 12
library(BSDA)
SIGN.test(x,y)
x <- c(35,24,28,31,38,30,33,32,28,29,30,30,33,32,34,36,27,22,31,38)
y <- c(33,30,40,45,48,27,44,45,35,42,38,38,40,30,35,42,40,45,44,35)

# Questão 13
marcaA <- c(18, 4)
marcaB <- c(8, 0)
df <- data.frame(marcaA)
df$marcaB <- marcaB
gl <- 2

qchisq(0.95, 2) > (((18-22)^2)/22) 1 (((8-12)^2)/12) 1 (((4-4)^2)/4)
# Como o valor crítico para a qui quadrado é maior do que o valor da qui quadrado calculado,não rejeitamos a hipótese nula
# Portanto, não há evidência suficiente para afirmar que as preferências são significativamente diferentes entre as duas marcas.
# Assim, concluímos que, no nível de significância de 0,05, não há evidência de diferença significativa de preferência entre as duas marcas.

#Questão 14
contra <- c(15, 8)
favor <- c(22,5)

df <- data.frame(contra,favor)
row.names(df) <- c("contra","a favor")
df

qchisq(0.95, 1) > ((22 - 8)^2)/(22 + 8)
# Rejeita - se hipótese nula
p <- ((22 - 8)^2)/(22 + 8)

# % de eleitores favoráveis à mudança antes do debate
(815)/50

# % de eleitores favoráveis à mudança depois do debate:
(2215)/50

# P-valor
pchisq(p, 1, lower.tail = F)

#Questão 15









# Questão 16
Class <- c(1:18)
Treino <- c(0, 0, 1, 1, 0, 0,0,0,1,1,0,1,
            0,1,1,1,1,1)
# C = 0
# T = 1

df <- data.frame(Class, Treino)

m <- sum(df$Treino == 1)
n <-sum(df$Treino == 0)

R_xi <- 53

W_tabled <- 57
T_resp <- sum(R_xi)

W_tabled > T_resp

#Questão 17

Antes <- c(125,132,138,120,125,127,136,139,131,132,135,136,128,127,130)
Depois <- c(118,134,130,124,105,130,130,132,123,128,126,140,135,126,132)
D_i <- c(7,2,8,4,20,3,6,7,8,4,9,4,7,1,2)
Sinal <- c(-1,1,-1,1,-1,1,-1,-1,-1,-1,-1,1,1,-1,1)
Rank <- c(10,2.5,12.5,6,15,4,8,10,12.5,6,14,6,10,1,2.5)
R_i <- Sinal * Rank
df <- data.frame(Antes, Depois, D_i,Rank,R_i)

p <- sum(R_i)/sqrt(sum(R_i^2))
pnorm(p)

# Questão 18
Med1 <- c(7,8,7,6,9,8,6)
Rank_Med1 <- c(16,19.5,16,10.5,21,19.5,10.5)
Med2 <- c(5,4,6,7,4,5,7)
Rank_Med2 <- c(6,2.5,10.5,16,2.5,6,16)
Med3 <- c(6,6,4,6,5,4,7)
Rank_Med3 <- c(10.5,10.5,2.5,10.5,6,2.5,16)

df <- data.frame(Med1, Rank_Med1, Med2, Rank_Med2, Med3, Rank_Med3)

R_Xij <- (21*22*43)/6
s2 <- (1/20)*(R_Xij - (21*(22^2))/4)
T_resp <- (1/s2)*((113^2)/length(Med1)+(59.5^2)/length(Med2)+(58.5^2)/length(Med3)-(21*(22^2))/4)

#Teste de hipotese
qchisq(0.95,2) < T_resp
# Rejeita H0


# P valor
pchisq(T_resp, 2, lower.tail = FALSE)
