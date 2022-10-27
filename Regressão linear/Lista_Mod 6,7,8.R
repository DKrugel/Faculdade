library(faraway)
library(MASS)
library(leaps)

data <- prostate
str(data)

options(device = 'x11')

ajuste <- lm(lpsa ~., data = data)

ajuste_Back <- stepAIC(ajuste, direction = "backward", test = 'F')

ajuste_Fow_AIC <- stepAIC(ajuste, direction = "forward")

all_reg <- regsubsets(lpsa ~., method = "exhaustive",
                      nvmax = 11, data = data)

plot(all_reg)

s1 <- summary(all_reg, matrix.logical = TRUE)
s1$cp
s1$adjr2

dados <- seatpos
help(seatpos)
str(seatpos)

ajuste <- lm(hipcenter ~., data = dados)
summary(ajuste)

confint(ajuste)

AjusteSelecionado <- stepAIC(ajuste, direction = "both", k = 2)
sumary(AjusteSelecionado)

confint(AjusteSelecionado)
