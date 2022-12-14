#Gerando valores aleat?rios usando rlst() com df = 8
set.seed(123)
x <- extraDistr::rlst(100, 8, mu = 10, sigma = 1)

#Adiciconando a log verossimilhan?a ao R
lstLL <- function(mu, sigma){
  -sum(extraDistr::dlst(x, 8, mu, sigma, log = TRUE)
  )
}

#Testando a fun??o log
lstLL(8,1)

#maximizando a fun??o log utilizando o m?todo MLE incluso no R
mlelst <- stats4::mle(lstLL, start = list(mu = 11, sigma = 0.5), method = "L-BFGS-B")

#Teste de hipoteses(qui quadrado)
wald <- function(hchapeu, hzero, vari){
  (hchapeu - hzero)/vari
}

#Calculo de p-valor para teste wald 
q <-wald(hchapeu = ,hzero = ,vari = )

pchisq(q, 1)





