placekick <- read.csv(file = "http://leg.ufpr.br/~lucambio/CE073/20222S/Placekick.csv")


#Ajustando regressão logistica usando familia binomial com o link logito
mod.fit <- glm( formula = good ~ distance , family = binomial ( link = logit ), data = placekick )
mod.fit

#A fórmula da regressão fica Logit(p)=5.8121 -0.1150x1 com x1 sendo a distancia

summary(mod.fit) #Distancia é significante

methods(class=glm)

mod.fit2 <- glm( formula = good ~ change + distance , 
                 family = binomial ( link = logit ), data = placekick ) #Regressão logistica multipla

vcov(mod.fit) 
# Observe que a variancia desta matriz de variancia e covariancia na diagonal principal 
# demonstra a variância estimada do estimador Beta

pi.hat <- mod.fit$fitted.values
V <- diag (pi.hat *(1 - pi.hat))
X <- cbind (1, placekick$distance )
solve (t(X) %*% V %*% X) # Fazendo a matriz vcov com calculo matricial

logL <- function (beta , x, Y) {
  pi <- exp( beta [1] + beta [2]* x)/(1 + exp( beta [1] + beta [2]* x)) 
  sum (Y*log(pi) + (1-Y)*log (1- pi))
}

#criamos uma função ,strong>logL() para calcular a função de log-verossimilhança para quaisquer valores
# de parâmetros fornecidos, valores de variáveis explicativas para x1 e respostas binárias para Y

logL ( beta = mod.fit$coefficients , x = placekick$distance , Y = placekick$good )

logLik (mod.fit )

#Maximizando usando optim
reg.mod <- lm( formula = good ~ distance , data = placekick )

mod.fit.optim <- optim(par= reg.mod$coefficients, fn = logL,
                       hessian = TRUE, x = placekick$distance,
                       Y = placekick$good,
                       control = list (fnscale = -1),
                       method = 'BFGS') #Control = list(fnscale=-1) é o parâmetro utilizado pra inverter
#A função, então invez do optim minimizar ele maximiza a função

mod.fit.optim$par #Retorna os coeficientes do modelo
mod.fit.optim$value #Retorna oo valor da logLik
mod.fit.optim$convergence
-solve(mod.fit.optim$hessian) #Matriz de covariância do modelo

#Dentro da chamada para optim(), especificamos as estimativas de parâmetros iniciais usando 
# o argumento par e especificamos a função a ser maximizada usando o argumento fn. 
# Observe que o primeiro argumento na função nomeada em fn deve corresponder às estimativas dos 
# parâmetros iniciais; é por isso que beta foi dado como o primeiro argumento em logL().
# O valor TRUE para o argumento hessiano instrui R a obter uma estimativa numérica da matriz hessiana para os parâmetros

beta0.values<-seq(from = -5, to = 18, by = 0.1)
beta1.values<-seq(from = -0.65, to = 0.25, by = 0.01)
count<-1
save.logL<-numeric(length(beta0.values)*length(beta1.values))
for (beta0 in beta0.values) {
  for (beta1 in beta1.values) {
    save.logL[count]<-logL(beta = c(beta0, beta1), x = placekick$distance, Y = placekick$good)
    count<-count+1
  }
}
max(save.logL) #Método de minimização manual, invés de fazer optim utilizar uma ideia de grid

w <- aggregate ( formula = good ~ distance , data = placekick , FUN = sum )
n <- aggregate ( formula = good ~ distance , data = placekick , FUN = length )
# Aggregate tenta relacionar duas ou mais variáveis do dataset através da função especificada no
#   Argumento FUN, no caso a somatória e o comprimento da variável resposta

w.n <- data.frame ( distance = w$distance , success = w$good , 
                    trials = n$good , proportion = round ( w$good /n$good ,4) )
#Criando um novo dataset para aplicação de pesos

mod.fit.bin <- glm( formula = success / trials ~ distance , 
                    weights = trials , family = binomial ( link = logit ), data = w.n)
# Pesos atribuidos com o numero de tentativas realizadas em cada ensaio, ainda utilizando o link logito

summary (mod.fit.bin)
summary(mod.fit2)

library(car)
car::Anova (mod.fit2 , test = "LR") #Tabela Anova não ordenada

#Utilizando Anova para comprar se a adição da variável Change é significativa
mod.fit.H0 <- glm( formula = good ~ distance , family = binomial ( link = logit ), data = placekick )
anova (mod.fit.H0 , mod.fit2 , test = "Chisq") # Significatvio entre 1 e 5 por cento


