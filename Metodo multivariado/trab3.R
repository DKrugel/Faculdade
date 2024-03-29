library(ggfortify)
library(ggplot2)
library(factoextra)
library(ggbiplot)
library(devtools)
library(stats)
library(mclust)
library(caret)
library(ggdendro)
library(sparcl)
library(dplyr)




pizza = read.csv(
  file = "http://leg.ufpr.br/~lucambio/CE090/20231S/pizza.csv",
  header = TRUE, sep = ",")

# auto valores da pizza
svd(var(pizza[,-c(1,2)]))$d

resultado <- prcomp(pizza[,-c(1,2)]) # calcula as Componentes Principais
summary(resultado)

resultado <- prcomp(pizza[,c(3,4,5)]) # calcula as Componentes Principais
summary(resultado)


#Incluiria até a terceira componente ja que ela explica em torno de 0.99 da variância dos dados

library(ggfortify)
pca.plot <- autoplot(resultado, data = pizza, colour = 'brand')
pca.plot

screeplot(resultado, type = "l", npcs = 7, main = "Gráfico das 7 PCs", pch = 19)
box()
grid()
abline(h = 1, col="red", lty=5)
legend("topright", legend=c("Autovalor = 1"), col=c("red"), lty=5, cex=0.6)




resultado_teste <- prcomp(pizza[,c(3,8,5)])


### 1 b ###
factoextra::fviz_pca_var(resultado_teste,
                         col.var = "contrib", # Cor por contribuições para o PC
                         gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                         repel = TRUE     #Evite sobreposição de texto
)
resultado$rotation[,1:3]

#O PC1 representa pizzas pobres em carboidratos e rica em umidade, proteínas e gorduras. Enquanto o PC2 representa pizzas secas mas ricas em gordura. Já o PC3 representa pizzas com poucas proteínas mas gordurosas e úmidas.

### 1 c ###

resultado_normalizado <- prcomp(pizza[,-c(1,2)], scale. = T, center = T) # calcula as Componentes Principais

ggbiplot::ggbiplot(resultado_normalizado,
         obs.scale = 1,
         var.scale = 1,
         groups = as.factor(pizza$brand),
         ellipse = TRUE,
         circle = TRUE,
         ellipse.prob = 0.95)


# Nível de crocância não é uma das variáveis avaliadas neste estudo, estarei avaliando a crocância com base na quantidade de água (umidade)

# Gordurosa e crocante: Marca A
# Macia e pouco gordurosa: Marca I e J
# Equilibrada: Marca B e J


### Questão 2 ###

exemplo = read.csv(
  file = "http://leg.ufpr.br/~lucambio/CE090/20231S/NTRforW.csv",
  sep = ",")

# transformando os dados

exemplo[,6:8] <- exemplo[,6:8] * 60


# 2 b #
Fatorial2 <- factanal(exemplo[,2:8], factors = 2)
Fatorial2


Fatorial3 <- factanal(exemplo[,2:8], factors = 3)
Fatorial3
# 
# Inicialmente, temos dois modelos interessantes para análise: um com dois fatores e outro com três fatores. O modelo com dois fatores apresenta um valor de p de 0.62, indicando que não há evidências suficientes para rejeitar a hipótese nula de que dois fatores são adequados. Por outro lado, o modelo com três fatores possui um carregamento SS loading maior que 1, o que, de acordo com a regra de Kaiser, sugere que vale a pena mantê-lo.
# 
# Considerando esses resultados, a escolha do modelo dependerá da análise realizada. Nesse caso, optaremos pelo modelo mais simples, com apenas dois fatores.
# 
# A interpretação dos fatores é a seguinte: o Fator 1 tem maior influência nas corridas de longa duração, enquanto o Fator 2 tem maior influência nas corridas de curta duração.

#######
# 2 c #
#######

Fatorial2_none <- factanal(exemplo[,2:8], factors = 2, rotation = "none")
Fatorial2_varimax <- factanal(exemplo[,2:8], factors = 2, rotation = "varimax")
Fatorial2_promax <- factanal(exemplo[,2:8], factors = 2, rotation = "promax")

par(mfrow = c(1,3))
plot(Fatorial2_none$loadings[,1], 
     Fatorial2_none$loadings[,2],
     xlab = "Fator 1", 
     ylab = "Fator 2", 
     ylim = c(-0.5,1),
     xlim = c(0,1),
     pch = 19,
     main = "Sem rotaçao")
abline(h = 0, v = 0)

plot(Fatorial2_varimax$loadings[,1], 
     Fatorial2_varimax$loadings[,2],
     xlab = "Fator 1", 
     ylab = "Fator 2", 
     ylim = c(-0.5,1),
     xlim = c(0,1),
     pch = 19,
     main = "Rotação Varimax")

text(Fatorial2_varimax$loadings[,1]-0.03, 
     Fatorial2_varimax$loadings[,2]+0.03,
     colnames(exemplo[,2:8]),
     col="darkgreen")
abline(h = 0, v = 0)

plot(Fatorial2_promax$loadings[,1], 
     Fatorial2_promax$loadings[,2],
     xlab = "Fator 1", 
     ylab = "Fator 2",
     ylim = c(-0.5,1),
     xlim = c(0,1),
     pch = 19,
     main = "Rotação Promax")
text(Fatorial2_promax$loadings[,1]-0.03, 
     Fatorial2_promax$loadings[,2]+0.03,
     colnames(exemplo[,2:8]),
     col="darkgreen")
abline(h = 0, v = 0)

#################
### Questão 3 ###
#################

layout(1)

penguins = read.csv(
  file = "http://leg.ufpr.br/~lucambio/CE090/20231S/penguins.csv",
  header = TRUE, sep = ",")

penguinsdata <- penguins[,4:6]


set.seed(2203)
amostra <- penguins[sample(nrow(penguins), 80, replace=FALSE),]
distancia <- dist(penguinsdata[amostra$X,], method = "euclidean")

hbloco  <- hclust(distancia)
plot(hbloco)
box()

y = cutree(hbloco, 3)
ColorDendrogram(hbloco, y = y, labels = names(y), main = "Espécies de pinguins", 
                branchlength = 80)
