ger_ex14 <- function(param){
  # Função de probabilidade
  fx14 <- function(x,theta = 1,pi = 2){
    log(pi)*exp(( (-exp(pi^(pi*x) ) * theta) )+ pi ^ (pi*x)) * (pi ^((pi*x)+1)) * theta
  }
  
  M <- 2
  x <- numeric(param[1])
  i <- 1
  k <- 1
  
  while(i <= param[1]){
    y <- rexp(1,rate = 10)
    u <- runif(1)
    ratio <- fx14(y,param[2],param[3])/(M * dexp(y,10))
    if( u < ratio){
      x[i] <- y
      i <- i+1
    }
    k <- k + 1
  }
  return(x)
}

x <- ger_ex14(param = c(10, 1,2))

set.seed(123)

# optim(c(1,1,2), ger_ex14)


N <- 1000
g <- numeric(N)
for(i in 1:N){
  x <- ger_ex14(c(2,1,2))
  g[i] <- abs(x[2] - x[1])
}
mean(g)

# IC do meu erro padrão das estimativas
#theta1
1.133861 + (c(-1,1)*1.96 * mean(g))

#Theta2
1.801283 + (c(-1,1)*1.96 * mean(g))


Amostra <- ger_ex14(c(5,1,2))

for(i in seq(1,length(Amostra))){
  amostra_copy <- Amostra[-i]
  Est.test[i]  <- mean(amostra_copy)
}

sd(Est.test) * (length(Amostra)-1)/length(Amostra)


##########################################################################
dados <- ger_ex14(c(5,1,2))

# Função para obter o estimador de interesse
estimador <- function(dados) {
  return(mean(dados))
}

# Número de reamostragens bootstrap
B <- 1000

# Bootstrap
estimativas_boot <- numeric(B)
for (i in 1:B) {
  dados_boot <- sample(dados, replace = TRUE)
  estimativas_boot[i] <- estimador(dados_boot)
}

# Estimador corrigido
estimador_corr <- mean(estimativas_boot) - (mean(estimativas_boot) - estimador(dados))

# Erro padrão
erro_padrao <- sd(estimativas_boot)

# Intervalo de confiança de 95%
quantil_inf <- quantile(estimativas_boot, 0.025)
quantil_sup <- quantile(estimativas_boot, 0.975)
intervalo_conf <- c(quantil_inf, quantil_sup)

# Resultados
cat("Estimador corrigido:", estimador_corr, "\n")
cat("Erro padrão:", erro_padrao, "\n")
cat("Intervalo de confiança (95%):", intervalo_conf, "\n")



estimador_corr1000 <- numeric(500)
estimador_corr5 <- numeric(500)
estimador_corr10 <- numeric(500)
estimador_corr100 <- numeric(500)

erro_padrao1000 <- numeric(500)
erro_padrao5 <- numeric(500)
erro_padrao10 <- numeric(500)
erro_padrao100 <- numeric(500)

intervalo_conf5 <- data.frame()
intervalo_conf10 <- data.frame()
intervalo_conf100 <- data.frame()
intervalo_conf1000 <- data.frame()


estimativas_boot5 <- numeric(B)
estimativas_boot10 <- numeric(B)
estimativas_boot100 <- numeric(B)
estimativas_boot1000 <- numeric(B)
B <- 100



for(j in 1:500){
dados1000 <- ger_ex14(c(1000,1,2))
dados5 <- ger_ex14(c(5,1,2))
dados10 <- ger_ex14(c(10,1,2))
dados100 <- ger_ex14(c(100,1,2))

# Função para obter o estimador de interesse
estimador <- function(dados1000) {
  return(mean(dados1000))
}

# Número de reamostragens bootstrap
#B <- 10000

# Bootstrap

for (i in 1:B) {
  dados1000_boot <- sample(dados1000, replace = TRUE)
  estimativas_boot1000[i] <- estimador(dados1000_boot)
  dados5_boot <- sample(dados5, replace = TRUE)
  estimativas_boot5[i] <- estimador(dados5_boot)
  dados10_boot <- sample(dados10, replace = TRUE)
  estimativas_boot10[i] <- estimador(dados10_boot)
  dados100_boot <- sample(dados100, replace = TRUE)
  estimativas_boot100[i] <- estimador(dados100_boot)
  }

# Estimador corrigido
estimador_corr5[j] <- mean(estimativas_boot5) - (mean(estimativas_boot5) - estimador(dados5))
estimador_corr10[j] <- mean(estimativas_boot10) - (mean(estimativas_boot10) - estimador(dados10))
estimador_corr100[j] <- mean(estimativas_boot100) - (mean(estimativas_boot100) - estimador(dados100))
estimador_corr1000[j] <- mean(estimativas_boot1000) - (mean(estimativas_boot1000) - estimador(dados1000))

# Erro padrão
erro_padrao1000[j] <- sd(estimativas_boot1000)
erro_padrao5[j] <- sd(estimativas_boot5)
erro_padrao10[j] <- sd(estimativas_boot10)
erro_padrao100[j] <- sd(estimativas_boot100)




# Intervalo de confiança de 95%
intervalo_conf1000[j,1] <- as.numeric(quantile(estimativas_boot, 0.025))
intervalo_conf1000[j,2] <- quantile(estimativas_boot, 0.975)

intervalo_conf5[j,1] <- as.numeric(quantile(estimativas_boot5, 0.025))
intervalo_conf5[j,2] <- quantile(estimativas_boot5, 0.975)

intervalo_conf10[j,1] <- as.numeric(quantile(estimativas_boot10, 0.025))
intervalo_conf10[j,2] <- quantile(estimativas_boot10, 0.975)

intervalo_conf100[j,1] <- as.numeric(quantile(estimativas_boot100, 0.025))
intervalo_conf100[j,2] <- quantile(estimativas_boot100, 0.975)

}


###################################
#     GRÁFICOS PARA BOOTSTRAP 

library(tidyverse)
library(hrbrthemes)
par(mfrow = c(1,2))


comp.estimador <- t(data.frame("5" = mean(estimador_corr5), "10" = mean(estimador_corr10), "100" = mean(estimador_corr100), "1000" = mean(estimador_corr1000)))
colnames(comp.estimador) <- c("media")
comp.estimador <- as.data.frame(comp.estimador)
comp.estimador$tamanho <- c(5,10,100,1000)

ggplot(comp.estimador, aes(x = tamanho ,y = media)) +
  geom_line() +
  xlim(5,1000)
  


estimador_corr.df <- data.frame(amostra = seq(1, length(estimador_corr1000)), estimador_corr1000)
estimador_corr.df %>% 
  ggplot(aes(x = amostra, y = estimador_corr1000)) +
  geom_bar(stat = "identity") +
  theme_ipsum()

erro_padrao.df <- data.frame(amostra = seq(1,length(erro_padrao1000)), erro_padrao1000)
erro_padrao.df %>% 
  ggplot(aes(x = amostra, y = erro_padrao1000)) +
  geom_bar(stat = "identity") +
  theme_ipsum()

rownames(intervalo_conf1000) <- seq(1, nrow(intervalo_conf1000))

############# JK ####################
erro_padrao_jk <- numeric(100)
tam_amostra <- numeric(4)

for(k in 1:100){
  Amostra <- ger_ex14(c(5, 1, 2))
for(i in seq(1,length(Amostra))){
  amostra_copy <- Amostra[-i]
  Est.test[i]  <- mean(amostra_copy)
}

erro_padrao_jk[k]<- sd(Est.test) * (length(Amostra)-1)/length(Amostra)
}

erro_padrao_jk.df <- data.frame(tamanho = seq(1, length(erro_padrao_jk)), erro_padrao_jk)
