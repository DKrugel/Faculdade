library(tidyverse)
library(hrbrthemes)
library(GGally)
library(glmnet)

dados <- read.csv2("divorce.csv")
i <- 1
for(i in 1:(ncol(dados)-1)){
  dados[,i] <- as.factor(dados[,i])
  levels(dados[,i]) <- c(levels(dados[,i]),"5")
  }
levels(dados$Class) <- c('0','1')


str(dados)
class(dados)
summary(dados)

# Distribuição das respostas
total <- count(dados,dados[,1])

for(i in 2:53){
  part <- count(dados,dados[,i])
  total[,2] <-total[,2] + part[,2]
}
colnames(total) <- c('respostas','n')

total %>% 
  ggplot(aes(x = respostas, y = n)) + 
  geom_bar(stat = 'identity')

# Balanceamento da variável resposta
ggplot(dados, aes(x = as.factor(Class))) + 
  geom_bar() +
  xlab('Divórcio') +
  ylab('Quantidade') + 
  theme_ipsum()

#Correlação entre as variáveis separadas pelo divórcio ou não
divorciados <- dados %>% filter(Class == 1)

casados <- dados %>% filter(Class == 0)

 ggcorr(divorciados, method = c("everything","pearson"))
 
 ggcorr(casados, method = c("everything","pearson"))
 
-# ajustando modelo (Família Gaussiana) Antes de transformar para fatores+
 
fit <- glm(formula = Class ~.,
           data = dados)
summary(fit)
fit.mod <- step(fit, direction = 'both')
summary(fit.mod)

anova(fit, fit.mod)
# Não há diferença significativa entre os dois modelos, portanto o mais simples deve de ser utilizado

plot(fit.mod)

# Ajuste utilizando gaussian
fit.gaus <- glm(formula = Class ~.,
           family = binomial(link = 'logit'),
           data = dados)
summary(fit.gaus)
fit.gaus.mod <- step(fit, direction = 'both')
summary(fit.gaus.mod)

anova(fit, fit.mod)

#Ajuste utilizando família binomial

#Lasso
fit.Bin.lasso <- glmnet(Class ~.,
               family = binomial(link = 'logit'),
               alpha = 1,
               data = dados)

fit.bin.lasso.mod <- step(fit.Bin, direction = "both")

fit.Bin.ridge <- glmnet(Class ~.,
                  family = binomial(link = 'logit'),
                  alpha = 0,
                  data = dados)

fit.bin.ridge.mod <- step(fit.Bin, direction = "both")

summary(fit.Bin)

car::Anova(fit.bin.ridge.mod, fit.bin.lasso.mod)

summary(fit.bin.mod)

#####################################
# Remover algumas perguntar para diminuir a correlação do modelo

dados2<- dados[,c(1,4,5,7,9,13,14,18,23,27,31,32,35,39,42,48,54,55)]

fit2 <- glm(formula = Class ~.,
            family = binomial(link = 'logit'),
           data = dados2)

summary(fit2)


dados3<- dados[,c(1,4,7,9,13,18,23,27,31,32,39,42,54,55)]


fit3 <- glm(formula = Class ~.,
            family = binomial(link = 'logit'),
            data = dados3)
