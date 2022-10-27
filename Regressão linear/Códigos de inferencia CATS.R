library(MASS) 
library(ggplot2)
data(cats) ### Carregando a base.
help(cats)

n <- nrow(cats)
ajuste <- lm(Hwt ~ Bwt, data = cats)
summary(ajuste)
fitted(ajuste)
residuals(ajuste)
beta_chap <- coef(ajuste)

### Estimação da variância do erro
res <- residuals(ajuste)
s2 <- sum(res^2)/(n-2) ### Variância estimada do erro
s <- sqrt(s2) ### Desvio padrão estimado do erro

### Obtenção dos erros padrões das estimativas

x <- cats$Bwt
xbar <- mean(x)

var_beta1 <- s2/sum((x - xbar)^2) 
### Variância estimada de beta_chapeu_1

ep_beta1 <- sqrt(var_beta1); ep_beta1
### Erro padrão de beta_chapeu_1

var_beta0 <- s2 * (1/n + xbar^2/sum((x - xbar)^2)) 
### Variância estimada de beta_chapeu_0

ep_beta0 <- sqrt(var_beta0); ep_beta0
### Erro padrão de beta_chapeu_1


### Vamos testar H0: beta1=0 vs H1: beta1!=0
t_value <- beta_chap[2]/ep_beta1
t_crit <- qt(0.025, df = n-2, lower.tail = FALSE) ### Considerando alpha=5%
abs(t_value) > t_crit 
### Rejeitamos H0, ou seja, o peso do coração está relacionado ao peso corporal.
p_value <- 2*pt(abs(t_value), df = n-2, lower.tail = FALSE)


### Intervalos de confiança (95%) para os parâmetros do modelo

beta_chap[1] + qt(c(0.025, 0.975), df = n-2) * ep_beta0
beta_chap[2] + qt(c(0.025, 0.975), df = n-2) * ep_beta1


################################################################################
### Estimação da resposta média e predição de novas observações

### Estimação do peso médio do coração para gatos com x=3kg
x0 <- 3
mu_chap <- beta_chap[1] + beta_chap[2] * x0; mu_chap
var_mu_chap <- s2*(1/n + ((x0-xbar)^2)/(sum((x - xbar)^2)))
ep_mu_chap <- sqrt(var_mu_chap); ep_mu_chap

mu_chap + qt(c(0.025, 0.975), df = n-2) * ep_mu_chap

### Predição do peso do coração para um novo gato com peso x=3kg

y_chap <- beta_chap[1] + beta_chap[2] * x0; y_chap
var_y_chap <- s2*(1 + 1/n + ((x0-xbar)^2)/(sum((x - xbar)^2)))
ep_y_chap <- sqrt(var_y_chap); ep_y_chap

y_chap + qt(c(0.025, 0.975), df = n-2) * ep_y_chap

################################################################################
### Estimação por máxima verossimilhança

### Função de (log)verossimilhança (assumindo normalidade)
lik_fun <- function(x, y, param){
  beta_0 <- param[1]
  beta_1 <- param[2]
  sigma <- param[3]
  -sum(dnorm(y, mean = beta_0 + beta_1 * x, sd = sigma, log = TRUE))
}

### Otimização (maximização) da log-verossimilhança
optim(par=c(0, 1, 2), fn = lik_fun, x = cats$Bwt, y = cats$Hwt)

################################################################################
### Análise de variância
y_chap <- fitted(ajuste)
y <- cats$Hwt
y_bar <- mean(y)
n <- nrow(cats)

### Somas de quadrados
SQ_Reg <- sum((y_chap - y_bar)^2)
SQ_Reg
SQ_Res <- sum((y - y_chap)^2)
SQ_Res

### Graus de liberdade
df_Reg <- 1
df_Res <- n-2

### Quadrados médios
QM_Reg <- SQ_Reg/df_Reg
QM_Reg
QM_Res <- SQ_Res/df_Res
QM_Res

### Estatística do teste
F_test <- QM_Reg/QM_Res
F_test

### Valor crítico do teste (5% de significância)
qf(0.95, df1 = df_Reg, df2 = df_Res)

### p-valor do teste
pf(F_test, df1 = df_Reg, df2 = df_Res, lower.tail = FALSE)

anova(ajuste)