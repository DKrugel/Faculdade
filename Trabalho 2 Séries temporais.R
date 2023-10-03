library(astsa)
library(forecast)
data <- log(varve)


HW025 <- HoltWinters(data, alpha = 0.25, beta = FALSE, gamma = FALSE)
HW050 <- HoltWinters(data, alpha = 0.50, beta = FALSE, gamma = FALSE)
HW075 <- HoltWinters(data, alpha = 0.75, beta = FALSE, gamma = FALSE)

par(mfrow = c(3,1))
plot(HW025)
plot(HW050)
plot(HW075)
layout(1)
df <- data.frame("0.25" = HW025$coefficients,
                 "0.50" = HW050$coefficients,
                 "0.75" = HW075$coefficients)

# Apesar dos coeficientes gerados pelo algoritmo de HoltWinters serem extramente próximos, podemos ver o efeito pesado da suavisação duplicando e triplicando

###
### Questão 32 
###
data <- oil

#Ajustando um ARIMA (0,1,1)
plot(data)

ARIMA011<- arima(data, order = c(0,1,1))

RESID.011 <- checkresiduals(ARIMA011)

# Para um primeiro ajuste está rasoavel, os residuos são aproximadamente normais, os residuos está em torno do zero, porém o ACF está bem irregular

#Utilizando o algoritmo auto.arima, minimizando soma de quadrados condicionais
ARIMAAUTO <- auto.arima(data)

(RESID.AUTO <- checkresiduals(ARIMAAUTO))

(df <- data.frame(
RESID.011$p.value,
RESID.AUTO$p.value
))
# Curiosamente podemos ver a saída da função checkresiduals() já nos fornece o pvalor do teste Ljung-Box no qual podemos ver a maldição de adicionarmos mais parâmetros nos modelos autoregressivos, onde o maior grau de liberdade, do modelo, contribuiu para um pvalor maior que o modelo ARIMA(0,1,1), mais simples.

# Se eu fosse escolher um dos modelos utlizaria o gerado pelo auto.arima(), o ACF demonstrou um comportamento mais concentrado nas bandas de confiança e uma enfase maior nos meses mais próximas do evento, além disso os resíduos se aproximam mais de zero pelo que me aparenta.

