library(tidyverse)
library(andrews)

url <-  "http://leg.ufpr.br/~lucambio/CE090/20231S/pottery"
dados <- read.table(url, header=TRUE)

head(dados)
str(dados)

boxplot(dados[,-10])

#Laço for para fazer todos os histogramas
par(mfrow = c(3,3))
for(i in 1:9){
nome <- paste0("Concentração de ", names(dados)[i])

hist(dados[,i], 
     main = names(dados)[i],
     xlab = nome,
     ylab = "Frequência")

}

dados %>%
  pivot_longer(!kiln, names_to = "Quimica", values_to = "Valores") %>% 
  ggplot(aes(x = fct_inorder(Quimica), y = Valores))+
    geom_boxplot()


#Curva de Andrews
andrews(dados,
        type = 1,
        clr = 10,
        step = 100)

# pivot_longer(dados, kiln)
# 
# dados %>% 
#   ggplot(aes(x = c(Al2O3, Fe2O3, MgO, CaO, Na2O, K2O, TiO2, MnO, BaO))) +
#   geom_boxplot()

#################
### QUESTÃO 2 ###
#################

# Taxas de desemprego em todos os estados federais
# da Alemanha em setembro de 1999.
local = "http://leg.ufpr.br/~lucambio/MSM/population.txt"
dados = read.table(local, header = TRUE, sep = " ")

#Boxplots
dados %>% 
  ggplot(aes(x = Inhabitants)) + 
  geom_boxplot()

dados %>% 
  ggplot(aes(x = Unemployed)) + 
  geom_boxplot()

#Curva de Andrews
andrews(dados,
        type = 1,
        clr = 1)

#Histogramas
hist(dados$Inhabitants)
hist(dados$Unemployed)

# Diagramas de dispersão
plot(dados[,-3]) #Inhabitants
plot(dados[,-2]) #Unemployed
plot(dados[,-1]) #Inhabitants x Unemployed

#################
### Questão 3 ###
#################
# Com um determinante igual a zero temos que pela propriedade de que o determinante é igual ao produtório dos autovalores, chegamos a conclusão de que pelo menos um dos autovalores é igual a zero, portanto não, todos os auto valores NÃO podem ser positivos.

#################
### Questão 4 ###
#################


# Sim, se todos os autovalores de uma matriz quadrada A são diferentes de zero, então a matriz é não-singular e, portanto, tem inversa. Isso pode ser mostrado usando a definição de inversa, que é uma matriz B tal que AB = BA = I, onde I é a matriz identidade.
# 
# Para uma matriz quadrada A com autovalores todos diferentes de zero, podemos encontrar sua decomposição em valores singulares (SVD), que é uma factorização matricial que permite escrever A como o produto de três matrizes: U, Σ e V. A matriz Σ é uma matriz diagonal com os autovalores de A na diagonal. Como todos os autovalores são diferentes de zero, Σ é uma matriz não-singular, o que significa que seus elementos diagonais são todos diferentes de zero.
# 
# Dessa forma, podemos definir a matriz inversa de A como A^-1 = V Σ^-1 U^T, onde Σ^-1 é a matriz diagonal com a inversa de cada autovalor de A na diagonal. Como Σ tem todos os seus elementos diagonais diferentes de zero, Σ^-1 também terá todos os seus elementos diagonais diferentes de zero, o que significa que A^-1 existe.

#################
### Questão 5 ###
#################

M <- matrix(c(0,1/3,2/3,0,0,
              0,0,0,1/4,3/4,
              0,0,0,1/4,3/4,
              1,0,0,0,0,
              1,0,0,0,0), ncol = 5,
            byrow = T)
M
svd(M)

