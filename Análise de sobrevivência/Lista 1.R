dados <- c(7, 34, 40, 63, 64, 74, 83, 84, 91, 108, 110, 109, 133, 133, 139, 140, 140, 146, 149, 
154, 157, 160, 160, 165,173, 176, 185, 018, 005, 041, 048, 073, 077, 079, 097, 319, 
405, 417, 400, 440, 503, 503, 583, 594, 1101,1116, 1146, 1006, 1349, 1410, 1417)

cens <- c(1,1,1,1,1,0,
          1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,1,1,1,1,1,1,0,1,0,1,1,1,1,1,0,1,1,1,0,1,0,0,0,1)

df <- data.frame(dados, cens)
library(survey)
library(survminer)
library(tidyverse)
library(ggfortify)

Surv(dados, cens)
fit0 <- survfit(Surv(dados,cens) ~ 1, data = df)
autoplot(fit0)
summary(fit0)

print(fit0, print.rmean = T)

fit1 <- survfit(coxph(Surv(dados, cens)), data = dados, method = "breslow")

summary(fit0, times = c(42,100,300,1000))
summary(fit0)
0.157+0.126/0.183
a <- summary(fit0)
sum(a$surv)

Grande <- c(28, 89, 175, 195, 309, 377, 393, 421, 447, 462, 709, 744, 770, 1106, 1206)
censG <- c(1, 1, 1, 1, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0)

Peq <- c(4, 88, 137, 199, 280, 291, 299, 300, 309, 351, 358, 369, 369, 370, 375, 382, 392, 429,451, 1119)
CensP <- c(1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0,1, 0)

tip <- c(rep("Grande", length(Grande)), rep("Pequeno",length(Peq)))

G <- Surv(Grande,censG)
P <- Surv(Peq,CensP)

df <- data.frame("Dados" = c(G, P), "Tipo" = tip)

autoplot(survfit(Dados ~ Tipo, data = df))
