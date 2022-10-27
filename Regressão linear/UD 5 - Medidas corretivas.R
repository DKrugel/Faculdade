########################################################################
### Aula - YD5- medidas corretivas.

### Vamos carregar os pacotes a serem utilizados nesta aula.

require(faraway)
require(car)
require(MASS)
require(quantreg)
options(device = 'x11')

########################################################################
### Exemplo 1 - Tranformação na resposta - o método de Box-Cox.

### Nesta aplicação, vamos usar a base de dados ozone, do pacote faraway.
help(ozone)

### Vamos fazer uma análise de regressão linear considerando a concentração
### de ozônio como resposta e temperatura, unidade e ibh como preditoras.

ozone2 <- subset(ozone, select = c('O3', 'temp', 'humidity', 'ibh'))
plot(ozone2, pch = 20, cex = 1.4, col="navy")
### Podemos observar relações não lineares, evidente variância não constante,
### alguma evidência de assimetria. 

ajuste <- lm(O3 ~ temp + humidity + ibh, data = ozone2)
par(mfrow = c(2,2))
plot(ajuste) 
### Há fortes indícios de variância não constante para os erros, e alguma 
### evidência de não normalidade.

ncvTest(ajuste) ### A hipótese de variância constante é rejeitada.
shapiro.test(rstandard(ajuste)) 

### Vamos usar o método de Box-Cox para identificar uma transformação adequada.
par(mfrow = c(1,1))
b1 <- boxCox(ajuste, lambda = seq(0,1,0.1))
b1$x[which.max(b1$y)]

### Uma transformação do tipo raiz cúbica parece indicada.

ajuste2 <- lm(I(O3^(1/3)) ~ temp + humidity + ibh, data = ozone2)
par(mfrow = c(2,2))
plot(ajuste2)

ncvTest(ajuste2)
shapiro.test(rstandard(ajuste2))
### Observe que não há mais evidências contrárias às hipóteses de variância
### constante e normalidade. Os gráficos de resíduos têm um padrão bem mais
### adequado, indicando um bom ajuste.

fitted(ajuste2)
### Valores ajustados pelo modelo na escala transformada.

fitted(ajuste2)^3
### Valores ajustados pelo modelo na escala original da resposta.

### Vamos analisar os gráficos de efeitos na escala original da resposta.
p1 <- predictorEffects(ajuste2, ~ temp)
plot(p1, axes = list(y = list(transform = function(x) x^3, lab = 'O3')))

p2 <- predictorEffects(ajuste2, ~ humidity)
plot(p2, axes = list(y = list(transform = function(x) x^3, lab = 'O3')))

p3 <- predictorEffects(ajuste2, ~ ibh)
plot(p3, axes = list(y = list(transform = function(x) x^3, lab = 'O3')))


########################################################################
### Exemplo 2 - Mínimos quadrados ponderados. Vamos utilizar a base de 
### dados cars, disponível na base do R.

help(cars) 
head(cars,10) 
summary(cars) 

par(cex = 1.2, las = 1)
plot(cars,pch=20,xlab='Velocidade (mph)',ylab='Distância de frenagem (m)', col = 'navy') 
with(cars,lines(lowess(dist~speed),col='red',lwd=2)) 

### A variância de y (resposta) parece aumentar conforme a velocidade do carro.

### Ajuste 1: regressão linear via mínimos quadrados ordinários.

ajuste <- lm(dist~speed,data = cars) 
summary(ajuste) 

par(mfrow = c(2,2))
plot(ajuste, pch = 20, cex = 1.4)
### Os resíduos reforçam a evidência de variância não constante.

ncvTest(ajuste) 
### Como a hipótese nula é a de variância constante para os erros, temos evidência
### significativa de variância não constante ao nível de 5%.

### Ajuste 2: regressão linear via mínimos quadrados ponderados.
### vamos assumir que a variância aumenta linearmente conforme a velocidade (x).
### Assim, os pesos vão ser definidos por 1/x.

ajuste2 <- lm(dist ~ speed, weights = 1/speed, data = cars) 
summary(ajuste) 
compareCoefs(ajuste, ajuste2, zvals = TRUE, pvals = TRUE) 
### Comparação das estimativas e erros padrões fornecidos pelos dois modelos.

plot(ajuste2, pch = 20, cex = 1.4)
### Diafnóstico de resíduos para o modelo ajustado por mínimos quadrados ponderados.

par(mfrow = c(1,2))
plot(ajuste, pch = 20, cex = 1.4, which = 3, lwd = 3,
     main = 'Mínimos quadrados ordinários')
plot(ajuste2, pch = 20, cex = 1.4, which = 3, lwd = 3,
     main = 'Mínimos quadrados ponderados')
### Visualmente, o padrão de variância não constante é menos acentuado para o modelo
### ajustado com ponderação.

ncvTest(ajuste2) 
### A hipótese de variância constante já não é rejeitada.


########################################################################
### Exemplo 3 - Regressão robusta: vamos usar a base de dados teengamb,
### do pacote faraway.

help("teengamb")

ajuste <- lm(gamble ~ ., data = teengamb)
par(mfrow = c(2,2))
plot(ajuste)
shapiro.test(rstandard(ajuste))
### Aparentemente os erros não têm distribuição normal, havendo indicação
### de alguma distribuição com caudas mais pesadas.

### Vamos usar regressão robusta, obter os estimadores M baseado no método
### de Huber.

ajuste2 <- rlm(gamble ~ ., psi = psi.huber, data = teengamb)
summary(ajuste)
summary(ajuste2)

### As estimativas pontuais apresentaram alguma variação. Os erros padrões
### são todos menores para a regressão robusta. Embora o resumo do modelo
### não apresente os p-valores, os efeitos podem ser testados usando os 
### valores t com base na distribuição normal assintótica. No geral, exceto
### pelas variações numéricas, as conclusões produzidas pelos dois modelos 
### são, neste caso, semelhantes.

### Agora usando a função biweight no lugar da huber.

ajuste3 <- rlm(gamble ~ ., psi = psi.bisquare, data = teengamb)
summary(ajuste3)

### Os erros padrões são um pouco menores em relação ao ajuste anterior,
### novamente se verifica diferença nas estimativas e erros padrões em relação
### aos resultados do ajuste por mínimos quadrados.

### Vamos avaliar os pesos atribuídos às observações no ajuste final.
cbind(resid(ajuste), ajuste2$w)
cbind(resid(ajuste), ajuste2$w)[c(24,36,39),]
### As observações 24, 36 e 39 são aquelas mais penalizadas, recebendo
### menor peso no ajuste do modelo. Essas três observações são justamente 
### aquelas destacadas na análise dos resíduos do modelo original como 
### possíveis outliers.

### Para o ajuste por least absolute deviations podemos usar a função rq
### do pacote quantreg.

ajuste4 <- rq(gamble ~ ., method = 'br', data = teengamb)
summary(ajuste4)