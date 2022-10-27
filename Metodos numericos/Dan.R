it1 <- function(fcd,i,ite){
  print(i)
  x1 <- fcd(i)
  print(c(x1,0,abs(x1-i)))
  iter <- 1
  while (ite>iter){
    i <- x1
    x1 <- fcd(x1)
    print(c(x1,0,abs(x1-i)))
    iter <- iter + 1
  }
}###metodo iteração com argumento nº de iterações


metsec <- function(fcd,o,i,e){
  fi <- 0
  fo <- 0
  er <- abs(i-o)
  u <- 0
  print(c(i,o,er))
  while(er>e){
    fi <- fcd(i)
    fo <- fcd(o)
    u <- i - ((i - o) * fi) / (fi - fo)
    o <- i
    i <- u
    er <- abs(i-o)
    print(c(i))
  }
}#####metodo secante com arg erro


install.packages("Deriv") ### PACOTE NECESSARIO
library(Deriv)

new <- function(fcd,i,e){
  d <- Deriv(fcd)
  f <- fcd(i)
  fl <- d(i)
  x1 <- i - (f/fl)
  er <- abs(x1-i)
  print(x1)
  while (er>e){
    f <- fcd(x1)
    fl <- d(x1)
    i <- x1
    x1 <- i - (f/fl)
    er <- abs(x1-i)
    print(x1)
  }
}# metodo newton argumento erro

