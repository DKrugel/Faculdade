library(tidyverse)
library(highcharter)
library(hrbrthemes)
library(car)
library(mcprofile)

starbucks = read.csv( file = "http://leg.ufpr.br/~lucambio/ADC/starbucks.csv" )
head(starbucks) 

starbucks$Day <- factor(starbucks$Day, levels = c('Monday',"Tuesday","Wednesday","Thursday","Friday"))

starbucks %>% 
  ggplot(aes(x = Day, y = Count)) +
  geom_bar(stat = "identity") +
  theme_ipsum() 

fit1 <- glm(Count ~ Day, 
            family = poisson(link='log'),
            data = starbucks)
  
summary(fit1)
Anova(fit1)

quart <- summary(starbucks$Count)

exp(fit1$coefficients[1] + quart[3]*fit1$coefficients[2:5])
exp(fit1$coefficients[2] + quart[3]*fit1$coefficients[which(fit1$coefficients!=fit1$coefficients[2])])

quart[3]*fit1$coefficients[1:5]

starbucks %>% 
  group_by(Day) %>% 
  reframe(total = sum(Count))

starbucks %>% 
  group_by(Day) %>% 
  reframe(total = mean(Count))

K <- matrix(data = c(1*quart[4], 0, 0, 0, 0,
                     0, 1*quart[4], 0, 0, 0,
                     0, 0, 1*quart[4], 0, 0,
                     0, 0, 0, 1*quart[4], 0,
                     0, 0, 0, 0, 1*quart[4]), nrow = 5, ncol = 5, byrow = TRUE)
linear.combo <- mcprofile(object = fit1, CM = K)
ci.beta <- confint(object = linear.combo, level = 0.95)
Estimativas <- ci.beta$confint
rownames(Estimativas) <- c("Segunda","Terça","Quarta","Quinta","Sexta")

# Ambas as formas de escrita do teste de hipóteses descrevem um modelo linear de variáveis, porém quando levado para modelos lineares generalizados precisamos levar em conta a função de ligação utilizada no modelo.
# Como utilizamos a função canônica de ligação para o GLM da família Poisson, sua link é a Função logarítmica, o que deve de ser levado a diante no teste de razões LRT
# Seria algo como H0: Log(Mu_mondau) = Log(Mu_Tuesday) ... e H1: Algum dos logarítmos da média diferem.
