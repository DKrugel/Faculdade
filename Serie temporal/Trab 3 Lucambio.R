library(astsa)
library(tidyverse)
library(seewave)

dados <- sunspotz

A.E <- mvspec(dados, log="no")

df <- A.E$details

df[which(df[,3] == max(df[,3])),]

freq <- 1/0.0917

A.E$df

# Intervalo de confiança
#Chi quadrado com 2 graus de liberdade, para truncar

qchisq(0.025, 2)
qchisq(0.975, 2)

c(2*(freq/qchisq(0.975, 2)) ,2*(freq/qchisq(0.025, 2))) #Pouca coisa de amplitude


##############
### Questão 20
##############

dados <- climhyd
head(dados)

PRECIPTRANS <- sqrt(dados$Precip)
fit01 <- sarima(PRECIPTRANS, 0,0,0, 0,1,1,12)

INFLUXTRANS <- log(dados$Inflow)
fit02 <- sarima(INFLUXTRANS, 0,0,0, 0,1,1,12, details = F)

mvspec(cbind(INFLUXTRANS, PRECIPTRANS), plot.type = "coh", spans = c(7,7))
mvspec(cbind(INFLUXTRANS, dados$Temp), plot.type = "coh", spans = c(7,7))
mvspec(cbind(INFLUXTRANS, dados$DewPt), plot.type = "coh", spans = c(7,7))
mvspec(cbind(INFLUXTRANS, dados$CldCvr), plot.type = "coh", spans = c(7,7)) #Alguma coerencia
mvspec(cbind(INFLUXTRANS, dados$WndSpd), plot.type = "coh", spans = c(7,7)) 


#c)

defasada <- LagReg(INFLUXTRANS, PRECIPTRANS) 

##############
### Questão 29
##############

mvspec(prodn, log = "n")
mvspec(prodn, log = "y")

A.E <- mvspec(prodn, log="no")

df <- A.E$details

df[which(df[,3] == max(df[,3])),]

1/0.032

#UNEMP
A.E2 <- mvspec(unemp, log="no")

df2 <- A.E2$details

df2[which(df2[,3] == max(df2[,3])),]

1/0.064 
