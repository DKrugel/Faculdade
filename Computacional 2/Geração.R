N <- numeric(1000)

f_x <- function(x){
  (gamma(17)/(gamma(12)*gamma(5)))*(x^12)*(1-x)^5
}

values <- numeric(2)
inicial.kick <- 0.5
values[1] <- inicial.kick
t <- 0.16
i <- 1
while(i <= length(N)){
values[2] <- runif(1, min = values[1] - t, max = values[1] + t)
alpha <- min(f_x(values[2])/f_x(values[1]),1)
 if(values[2] > 1 | values[2]<0){
   next
 }
if(runif(1) < alpha){
  values[1] <- values[2]
  N[i] <- values[1]
  i <- i+1
}
}

plot(density(N))

Criei a função de prob
enquanto i for menor que N continua
Escolhe um valor inicial para a funcao começar a amostrar
Definir a vizinhança U e [-t,t]
calcular o minimo entre o valor comprovado e o valor sorteado na vizinhança e chamar de alpha
Sortear um numero da uniforme, se ele for menor que alpha o valor 1 recebe o valor 2


