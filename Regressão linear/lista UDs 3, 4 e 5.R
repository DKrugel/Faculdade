library(faraway)
require(corrplot)
help(sat); View(sat)
dado <- sat[-c(44,48,29),]
par(mfrow = c(2,2))

ajuste <- lm(total~expend+salary+ratio+takers, data = sat)
plot(ajuste)


help(prostate); View(prostate)
ajuste1 <- lm(lpsa ~ lcavol + lweight + age + lbph + svi + lcp + gleason + pgg45, data = prostate)
plot(ajuste1)

residuals(ajuste1)
boxplot(residuals(ajuste1))

summary(ajuste1)

help(happy); View(happy)
happy1 <- happy[-36,]
ajusteH <- lm(happy ~ money + sex + love + work, data = happy1)
par(mfrow = c(2,2))
plot(ajusteH)
boxplot(residuals(ajusteH))

#Observação 36 pode ser um outlier por erro de digitação, enfase no PODE

#Exercício 5

help(divusa); View(divusa)

AjusteD <- lm(divorce ~ unemployed + femlab + marriage + birth + military, data = divusa)
plot(AjusteD)
corrplot.mixed(cor(divusa[,-1]), upper = "ellipse", lower.col = 'black')
residuals(AjusteD)
cor.test(divusa$year, residuals(AjusteD))
