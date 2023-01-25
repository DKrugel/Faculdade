#Carregando dados
data <- read.csv(file = "http://leg.ufpr.br/~lucambio/ADC/healthcare_worker.csv")

#Criando tabela de contingência
c.table <- array(data = c(data[,2],(data[,3]-data[,2])),
                 dim = c(5,2),
                 dimnames = list(occupation = data[,1], Hepatite = c("+","-")))

c.table

chisq.test(c.table)

vcd::assocstats(c.table)

# Para este teste esperavamos ver um p-valor alto pois assim não teriamos dependencia entre o nível de exposição e a quantidade de 
# Casos de hepatite C, como foi demonstrado no exercício 19 do trabalho 2. No teste qui quadrado de pearson encontramos um p-valor de 0.342
# muito elevado para o padrão ouro de p-valor de 0.05, portanto podemos assumir que existe independencia entre o nível de exposição ao paciente e a presença de Hepatite C.

##################
### Questão 11 ###
##################

cereal = read.csv( file = "http://leg.ufpr.br/~lucambio/ADC/cereal_dillons.csv")
head(cereal)

stand01 <- function (x) { (x - min(x))/( max(x) - min(x)) }
cereal2 <- data.frame (Shelf = cereal$Shelf, sugar = stand01 (x = cereal$sugar_g / cereal$size_g ), 
                       fat = stand01 (x = cereal$fat_g / cereal$size_g ), 
                       sodium = stand01 (x = cereal$sodium_mg / cereal$size_g ))

VGAM::vglm(Shelf ~ sugar + fat + sodium, 
           family = 'multinomial',
           data = cereal2)


#b
#Açúcar
boxplot ( formula = sugar ~ Shelf , data = cereal2 , ylab = "Açúcar", xlab = "Prateleira", 
          pars = list ( outpch = NA)) 

boxplot ( formula = fat ~ Shelf , data = cereal2 , ylab = "Gordura", xlab = "Prateleira", 
          pars = list ( outpch = NA)) 

boxplot ( formula = sodium ~ Shelf , data = cereal2 , ylab = "Sódio", xlab = "Prateleira", 
          pars = list ( outpch = NA)) 


grid()

library(tidyverse)
library(hrbrthemes)

cerealb <- cereal2

cerealb$Shelf <- as.factor(cerealb$Shelf)

par(mfrow = c(1,3))

ggplot(cerealb, aes(x = Shelf, y = sugar)) + 
  geom_boxplot() + 
  geom_jitter(color = 'red', size = 0.4, alpha = 0.6) +
  theme_ipsum()

ggplot(cerealb, aes(x = Shelf, y = sodium)) + 
  geom_boxplot() + 
  geom_jitter(color = 'blue', size = 0.4, alpha = 0.6) +
  theme_ipsum()

ggplot(cerealb, aes(x = Shelf, y = fat)) + 
  geom_boxplot() + 
  geom_jitter(color = 'green', size = 0.4, alpha = 0.6) +
  theme_ipsum()

# c)

# Levariamos em conta a ordinalidade da variável Shelf no caso da posição de cada uma das prateleiras, assim colocando alimentos com mais açucar, sódio e gordura em prateleiras mais altas, para evitar que crianças as peguem com facilidade, por exemplo

# d)
fit0 <- nnet::multinom(formula = Shelf ~ sugar + fat + sodium,
               data = cereal2)

summary(fit0)

car::Anova(fit0)

# As variáveis Sódio e Açúcar são as variáveis mais influentes para a variável Shelf

fit1 <- nnet::multinom(formula = Shelf ~ sugar:fat:sodium,
                            data = cereal2)
summary(fit1)

car::Anova(fit1)

# e)

kelloggs <- array(c(0.28,0.12,0.5),
                  dim = c(1,3))
kelloggs <- as.data.frame(kelloggs)

colnames(kelloggs) <- c("sugar","fat","sodium")

pihat <- predict(object = fit0, 
                 newdata = kelloggs, 
                 type = "probs")
# A uma probabilidade de 0.51 do cereal estar na prateleira 3 e de 0.48 na prateleira 4

# G)

# h)

sd.cereal <- apply (X = cereal2, MARGIN = 2, FUN = sd)
c.value <- c(1,sd.cereal)

beta.hat <- coefficients(fit0)[1,2:4]
round(1/exp(c.value[c(-1,-2)] * beta.hat),2)


####################
### Exercício 20 ###
####################


data <- read.csv(file = "http://www.ncbi.nlm.nih.gov/pubmedhealth/PMH0001187")
