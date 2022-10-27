#Gerando valores aleatórios usando rlst() com df = 8
set.seed(123)
x <- extraDistr::rlst(100, 8, mu = 10, sigma = 1)

#Adiciconando a log verossimilhança ao R
lstLL <- function(mu, sigma){
  -sum(extraDistr::dlst(x, 8, mu, sigma, log = TRUE)
  )
}

#Testando a função log
lstLL(8,1)

#maximizando a função log utilizando o método MLE incluso no R
stats4::mle(lstLL, start = list(mu = 11, sigma = 0.5), method = "L-BFGS-B")


library(extraDistr)
set.seed(89)
x <- rlst(1000,df = 8, mu = 10, sigma = 1 )
hist(x, breaks = 15,
     main= "Histograma dos dados gerados com rlst",
     ylab = "Frequência")

curve(dlst(x,8,10,1), 6, 14,
      xlab = "Valor de X",
      ylab = "Probabilidade",
      main = "Densidade da função lst (x|8,10,1)")

