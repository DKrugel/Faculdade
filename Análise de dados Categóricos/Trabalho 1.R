# 2)
n <- 750
w <- 48
pi.hat <- w/n

# Y ~ binomial (0.064)

#Criação do intervalo Wald
alpha <- 0.05
var.wald <- pi.hat*(1 - pi.hat)/n
lower <- pi.hat - qnorm (p = 1- alpha /2) * sqrt ( var.wald )
upper <- pi.hat + qnorm (p = 1- alpha /2) * sqrt ( var.wald )
round ( data.frame (lower , upper ), 4)

#Criação do intervalo Agrest Coull
p.tilde <- (w + qnorm(p = 1- alpha/2)^2/2) / (n + qnorm(p = 1- alpha/2)^2)
var.ac <- p.tilde*(1 -p.tilde ) / (n + qnorm(p = 1- alpha/2)^2)
round(p.tilde + qnorm(p = c( alpha/2, 1- alpha/2) ) * sqrt(var.ac), 4)

# A uma probabilidade de 95% da média de permanencia de clamídia entre a população está entre 0.0840 e 0.0485

tb <- data.frame(temp = c(10,15,20),
                qtd = c(0,1,25))
barplot(tb$qtd, 
        names.arg = tb$temp,
        xlab = 'Temperatura',
        ylab = 'Quantidade chocada')

alpha <- 0.05
n <- 30
w <- tb$qtd
pi.hat <- w/n
p.tilde <- (w + qnorm(p = 1-alpha/2)^2/2)/(n+qnorm(1-alpha/2)^2)
# Wald
var.wald<-pi.hat*(1-pi.hat)/n
lower.wald <- pi.hat - qnorm(p = 1-alpha/2)*sqrt(var.wald)
upper.wald <- pi.hat + qnorm(p = 1-alpha/2)*sqrt(var.wald)

Wald.inter <- data.frame(temperatura = tb$temp,
                         lower = lower.wald,
                         upper = upper.wald)
# Agresti-Coull
lower.AC <- p.tilde - qnorm(p = 1-alpha/2) * sqrt(p.tilde*(1-p.tilde) / (n+qnorm(1-alpha/2)^2))
upper.AC <- p.tilde + qnorm(p = 1-alpha/2) * sqrt(p.tilde*(1-p.tilde) / (n+qnorm(1-alpha/2)^2))
AC.inter <- data.frame(temperatura = tb$temp,
                         lower = lower.AC,
                         upper = upper.AC)
# Wilson
lower.wilson <- p.tilde - qnorm(p = 1-alpha/2) * sqrt(n) / (n+qnorm(1-alpha/2)^2) * 
  sqrt(pi.hat*(1-pi.hat) + qnorm(1-alpha/2)^2/(4*n))
upper.wilson <- p.tilde + qnorm(p = 1-alpha/2) * sqrt(n) / (n+qnorm(1-alpha/2)^2) * 
  sqrt(pi.hat*(1-pi.hat) + qnorm(1-alpha/2)^2/(4*n))

wilson.inter <- data.frame(temperatura = tb$temp,
                         lower = lower.wilson,
                         upper = upper.wilson)

######## FALTOU UMA ALTERNATIVA ################


################
###Questão 13###
################


y <- c(135,434,569,15,9,24,150,443,539)
tb <- matrix(y, nrow = 3, ncol = 3, byrow = TRUE)

alpha <- 0.05
n <- c(tb[1,3],tb[2,3])
w <- c(tb[1,1],tb[2,1])
pi.hat <- w/n
p.tilde <- (w + qnorm(p = 1-alpha/2)^2/2)/(n+qnorm(1-alpha/2)^2)
# Wald para proporção estimada de cada padrão de uso de camisinha
var.wald<-pi.hat*(1-pi.hat)/n
lower.wald <- pi.hat - qnorm(p = 1-alpha/2)*sqrt(var.wald)
upper.wald <- pi.hat + qnorm(p = 1-alpha/2)*sqrt(var.wald)

Wald.inter <- data.frame(Padrão = c('Nunca','Sempre'),
                         lower = lower.wald,
                         upper = upper.wald)

# Criando função para criar intervalo wald para diferença
wald.dif <- function(pi1, pi2, n1 ,n2 , alpha = 0.05){
  var.wald <- (pi1*(1 - pi1)/n1) + (pi2*(1 - pi2)/n2)
  loweR <- pi1-pi2 - qnorm(p = 1-alpha/2) * sqrt(var.wald)
  uppeR <- pi1-pi2 + qnorm(p = 1-alpha/2) * sqrt(var.wald)

  Wald.inter <- data.frame(lower = loweR,
                           upper = uppeR)
  return(Wald.inter)
}

# Criando função para criar intervalo agresti-Caffo para diferença
AgrestCaffo.dif <- function(pi1, pi2, n1 ,n2 , alpha = 0.05){
  var.AC <- (pi1*(1 - pi1)/(n1+2)) + (pi2*(1 - pi2)/(n2+2))
  loweR <- pi1-pi2 - qnorm(p = 1-alpha/2) * sqrt(var.AC)
  uppeR <- pi1-pi2 + qnorm(p = 1-alpha/2) * sqrt(var.AC)
  
  AC.inter <- data.frame(lower = loweR,
                           upper = uppeR)
  return(AC.inter)
}

pi1 <- tb[1,1]/tb[1,3]
pi2 <- tb[2,1]/tb[2,3]
wald.dif(pi1 = pi2, pi2 = pi1, n2 = tb[1,3], n1 = tb[2,3])
AgrestCaffo.dif(pi1 = pi2, pi2 = pi1, n2 = tb[1,3], n1 = tb[2,3])

#Criação de tabela de contingência
c.table <- array ( data = c(135,15, 434, 9) , dim = c(2 ,2) , 
                   dimnames = list ( First = c("NUNCA", "SEMPRE") , Second = c("Positivo", "Negativo")))
c.table

prop.test(c.table, conf.level = 0.95, correct = FALSE)
# Rejeição de hipõtese nula, existe uma diferença nas médias entre casos positivos de HIV com e sem o uso de preservativo
# Valor da estatística Chi quadrado é de 18.322, p valor é de 1.866e-05, dentro da região de rejeição.

# Razão de chances

OR <- c.table[1,1] * c.table[2,2]/ (c.table[1,2] * c.table [2,1])

1/OR

# A Não utilização do preservativo incrementa em 5.35 vezes as chances de se testar HIV positivo 

alpha<-0.05
var.log.or<-1/c.table[1,1] + 1/c.table[1,2] + 1/c.table[2,1] + 1/c.table[2,2]
OR.CI<-exp(log(OR) + qnorm(p = c(alpha/2, 1-alpha/2)) * sqrt(var.log.or))
round(OR.CI, 2)



