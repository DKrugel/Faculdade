library(gcmr)

#Questão 1
dados <-  read.csv("/home/fernando/Dropbox/Sitio/CE090/20231S/DRS.dat",
                 header = TRUE, sep = ",")


#Questão 2
dados <- read.csv(file = "http://leg.ufpr.br/~lucambio/MSM/cigarettes.txt",
                  header = TRUE, sep = "")
library(nnet)
library(tidyverse)
library(glmnet)
# gcmr(y ~ Price + Gender + Brand, marginal = poisson.marg(), cormat = cluster.cormat(dados$Gender, type = "independence"))

#Transformar as variáveis para uma análise descritiva:
dados$Gender <- as.factor(dados$Gender)
dados$Brand <- as.factor(dados$Brand)


plot(as.factor(dados$Gender), dados$y)
plot(as.factor(dados$Brand), dados$y)

dados %>% 
  group_by(Brand) %>% 
  summarise(media = mean(y))

dados %>% 
  group_by(Gender) %>% 
  summarise(media = mean(y))


#Testar alguns modelos vistos anteriormente:
fit0 <- multinom(y ~ Price + Gender + Brand, data = dados)
summary(fit0)
car::Anova(fit0)

fit1 <- multinom(y ~ Price + Gender + Brand + Brand:Price, data = dados)
summary(fit1)
car::Anova(fit1) # Explica razoávelmente a relação Price com Y, mas ainda não é satisfatória

fitlm <- lm(y ~ Price + Gender + Brand, data = dados)
summary(fitlm)
car::Anova(fitlm) # As relações não são lineares, é possível ver tanto na descritiva como no pvalor de cada estimativa

dados$Gender <- ifelse(dados$Gender == "M", 1,0)
dados$Brand <- ifelse(dados$Brand == "A",0,1)

fitglm <- glm(y ~.,
              data = dados,
              family = poisson) # Como a nossa variável resposta é uma variável de contagem, utilizei metodo de regressão generalizada com resposta Poisson.
car::Anova(fitglm) 
summary(fitglm)

fitglm2 <- glm(y ~. + Brand:Price, 
               family = poisson,
               data = dados) 
summary(fitglm2)
car::Anova(fitglm2)

fitglm3 <- glm(y ~. + I(Price^2), family = poisson, data = dados)
summary(fitglm3)
car::Anova(fitglm3) # Perdeu um pouco do poder de explicação das variáveis dependentes

y <- dados$y
x <- as.matrix(dados[,-1])

regRidge <- glmnet(x,y, 
                   family = "poisson", 
                   alpha = 0)
LambdaCalculado <- cv.glmnet(x,y)

LambdaCalculado$lambda.min

regRidge <- glmnet(x,y, 
                   family = "poisson", 
                   alpha = 0,
                   lambda = LambdaCalculado$lambda.min)

x11()
plot(regRidge)

# calcular o erro médio quadrático
y.pred <- predict(regRidge, x)
mse <- mean((y - y.pred)^2)
print(paste0("MSE = ", round(mse, 3)))


regLasso <- glmnet(x,y, family = "poisson", alpha = 1)
plot(regLasso)
