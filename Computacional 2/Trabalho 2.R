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

optim(c(1,1,2), ger_ex14)


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



estimador_corr <- numeric(4)
erro_padrao <- numeric(4)
intervalo_conf <- data.frame()
tamanho <- numeric(10)


for(j in 1:4){
dados <- ger_ex14(c(j*1000,1,2))
tamanho[j] <- j*1000 
# Função para obter o estimador de interesse
estimador <- function(dados) {
  return(mean(dados))
}

# Número de reamostragens bootstrap
B <- 100000

# Bootstrap
estimativas_boot <- numeric(B)
for (i in 1:B) {
  dados_boot <- sample(dados, replace = TRUE)
  estimativas_boot[i] <- estimador(dados_boot)
}

# Estimador corrigido
estimador_corr[j] <- mean(estimativas_boot) - (mean(estimativas_boot) - estimador(dados))

# Erro padrão
erro_padrao[j] <- sd(estimativas_boot)

# Intervalo de confiança de 95%
intervalo_conf[j,1] <- as.numeric(quantile(estimativas_boot, 0.025))

intervalo_conf[j,2] <- quantile(estimativas_boot, 0.975)

}


###################################
#     GRÁFICOS PARA BOOTSTRAP 

library(tidyverse)
library(hrbrthemes)
par(mfrow = c(1,2))

estimador_corr.df <- data.frame(tamanho, estimador_corr)
estimador_corr.df %>% 
  ggplot(aes(x = amostra, y = estimador_corr)) +
  geom_bar(stat = "identity") +
  theme_ipsum()

erro_padrao.df <- data.frame(tamanho, erro_padrao)
erro_padrao.df %>% 
  ggplot(aes(x = amostra, y = erro_padrao)) +
  geom_bar(stat = "identity") +
  theme_ipsum()

rownames(intervalo_conf) <- tamanho


############# JK ####################
erro_padrao_jk <- numeric(4)
tam_amostra <- numeric(4)

for(k in 1:4){
  Amostra <- ger_ex14(c(k*1000, 1, 2))
  tam_amostra[k] <- k*1000  
for(i in seq(1,length(Amostra))){
  amostra_copy <- Amostra[-i]
  Est.test[i]  <- mean(amostra_copy)
}

erro_padrao_jk[k]<- sd(Est.test) * (length(Amostra)-1)/length(Amostra)
}

erro_padrao_jk.df <- data.frame(tam_amostra, erro_padrao_jk)
