################################################################################
### Exrecício RLS 1

### Dados sobre erupções de um vulcão (Old Faithful geyser in Yellowstone National 
### Park, Wyoming, USA).

data(faithful)
help(faithful)

### O objetivo é ajustar um modelo de regressão para a duração da erupção
### (eruptions) em função do tempo decorrido desde a erupção mais recente
### (waiting), ambos em minutos.

plot(eruptions ~ waiting, data = faithful, pch = 20, col = 'grey60',
     xlab = 'Tempo desde a última erupção',
     ylab = 'Duração da erupção')
### Novamente, parece haver uma relação linear crescente entre as variáveis.

### Exercício: dar sequência à análise, conforme realizado para os dados sobre
### a anatomia dos gatos domésticos.
library(ggplot2)

grafico <- ggplot(data = faithful, aes(x = eruptions, y = waiting)) +
  geom_point(size = 1)+
  xlab("Duração da erupção") +
  ylab("Tempo para a próxima erupção")

ajuste <- lm(waiting ~ eruptions, 
             data = faithful)

grafico + geom_abline(intercept = ajuste[[1]][1], slope = ajuste[[1]][2], col = 'red', size = 1)

plot(faithful$waiting, fitted(ajuste),
     xlab = "Duração da erupção",
     ylab = "Tempo até a próxima erupção ajustado"
     )
cor(faithful$waiting, fitted(ajuste))^2
#Coeficiente de determinação igual a 0.8114, ou seja 81% da variação do tempo entre erupções é explicada pela duração da erupção anterior

plot(fitted(ajuste), residuals(ajuste), pch = 20, xlab = 'Valores ajustados', 
     ylab = 'Resíduos', col = 'grey60', las = 1, cex = 1.3)
#Não apresentou nenhum problema



