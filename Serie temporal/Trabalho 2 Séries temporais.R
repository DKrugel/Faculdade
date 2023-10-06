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

###
### Questão 34
###

dados <- so2

ARIMA011 <- arima(dados,order = c(0,1,1))
checkresiduals(ARIMA011)

ARIMA111 <- arima(dados,order = c(1,1,1))
checkresiduals(ARIMA111) # Aumento de um grau no processo auto regressivo fez o resultado do Ljung-Box indicar afastamento da normalidade dos residuos

ARIMA012 <- arima(dados,order = c(0,1,2))
checkresiduals(ARIMA012)

ARIMA021 <- arima(dados,order = c(0,2,1))
(RES021 <- checkresiduals(ARIMA021)) # Modelo escolhido
RES021$p.value # p valor adquirido no teste Ljung Box

# O modelo que não utiliza ordem de auto regressão, e usando o operador de segunda ordem de diferenciação nas médias móveis apresentou a melhor modelagem para os dados de So2. 

predict(ARIMA021, n.ahead = 4)

###
### Questão 36
###

# A)
DADOS <- cpg
plot(y = DADOS, x = c(1980:2008))

#Como mencionado no enunciado, vemos que o preço por GB de armazenamento decaiu extremamente rápido do inicio da decada de 80 até o ano de 95
plot(y = DADOS, x = c(1980:2008))
plot(ar.ols(log(DADOS), order = 1))

EQUATION <- -0.5498*exp(1.0217*c(1980:2008))
plot(EQUATION)
