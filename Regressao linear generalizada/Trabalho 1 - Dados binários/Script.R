library(tidyverse)
library(hrbrthemes)
library(GGally)
library(glmnet)
library(highcharter)

dados <- read.csv2("divorce.csv")
correlacao <- read.csv2("divorce.csv")
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
divorciados <- correlacao %>% filter(Class == 1)

casados <- correlacao %>% filter(Class == 0)

 ggcorr(divorciados, method = c("everything","pearson"), name ='Divorciados')
 
 ggcorr(casados, method = c("everything","pearson", name = 'Casados'))
 
 ggcorr(correlacao[,-55], method = c('everything','pearson', name = "Ambos"))
 
-# ajustando modelo (Família Gaussiana) Antes de transformar para fatores+
 
fit <- glm(formula = Class ~.,
           family = binomial(link = 'logit'),
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
fit.gaus.mod$coefficients

summary(fit.gaus.mod)

anova(fit, fit.mod)

#Ajuste utilizando família binomial

#Lasso
fit.Bin.lasso <- glmnet(dados$Class,
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
plot(fit3)

princomp(dados, cor = T) %>% hchart()
pca <- princomp(dados, cor = T)
pca$loadings



dados <- dados[,-c(40,54,6,16,31)]
#Removed 40, 54

corr.fit <- glm(Class ~ Atr6 + Atr7 + Atr45,
                data = dados,
                family = binomial(link = 'logit'))

summary(corr.fit)

dados <- dados[,-c(40,54,6,16,31,38,37)];for(i in 5:length(dados[,-length(dados)])){
  fit.check <- glm(formula = Class ~ dados[,i] + dados[,i-1] + dados[,i-2] + dados[,i-3] + dados[,i-4],
                   data = dados,
                   family = binomial(link = 'logit'))
  print(i)
  if(fit.check$converged == TRUE){
    fit.final <- fit.check
  }
  if(fit.check$converged == FALSE){
    print("Parou no"); print()
    break
  }
}

x <- model.matrix(Class ~., data = dados)[,-1]; y <- dados$Class

fit.lasso <- glmnet(x, y, family = binomial(link = 'logit'), alpha = 1)

x11(width = 12, height = 12)
plot(fit.lasso,las = 1, lwd = 2, label=TRUE)

cvfit <- cv.glmnet(x, y, family = 'binomial', alpha = 1, nfolds = 20) 
plot(cvfit)
coef(fit.lasso, s=cvfit$lambda.min)


fit.lasso.final <- glmnet(x, y, family = binomial(link = 'logit'), alpha = 1, lambda = cvfit$lambda.min)
plot(fit.lasso,las = 1, lwd = 2, label=TRUE)




fit.ridge <- glmnet(x, y, family = binomial(link = 'logit'), alpha = 0)
plot(fit.ridge,las = 1, lwd = 2, label=TRUE)
### menor deviance, estimado via validação cruzada.
cvfit <- cv.glmnet(x, y, family = 'binomial', alpha = 0, nfolds = 20) 

coef(fit.ridge, s=cvfit$lambda.min)
