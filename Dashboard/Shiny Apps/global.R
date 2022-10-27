#Pacotes utilizados na aplica??o
library(ggplot2)
library(plyr)
library(dplyr)
library(plotly)


#Carregando base de dados
setwd("H:\Faculdade\Dashboard\Shiny Apps\GPUs")
df <- read.csv2("gpu_db.csv", header = TRUE, sep = ",")

#Removendo dados com NA?s nos anos
IND <- which(is.na(df$releaseYear))
df <- df[-IND,]

#Criando botão para selecionar qual tipo de placa será feita a analise
#Onboard=1 #dedicada=2
BOTAO <- c("Onboard","Dedicada")
Valor.botao <- c()

if(length(BOTAO) == 1){
  if("Onboard" %in% BOTAO){
    Valor.botao <- 1
  }else{
    Valor.botao <- 2
  }
}else{
  Valor.botao <- 3
}

#Criei uma variavel para receber o data frame cortado na coluna releaseYear entre os limites inferior e superior usando dplyr
#LIANO <- 1999
#LSANO <- 2005
#opções <- c(1985:2022)
LSANO <- max(as.numeric(SLIDERANO))
LIANO <- min(as.numeric(SLIDERANO))
df.timestamped <- df[between(df$releaseYear, LIANO, LSANO), ]

#Calculo de gpuclock m?dio e gr?fico
df.split.ano <- split(df.timestamped, df.timestamped$releaseYear)
vetor.medio.gpuclock <- c()
vetor.ano <- c()

for(i in 1:(LSANO - LIANO+1)){
  vetor.medio.gpuclock[i] <- mean(df.split.ano[[i]]$gpuClock, na.rm = TRUE)
  vetor.ano[i] <- df.split.ano[[i]]$releaseYear[i]
}
Ano <- as.character(vetor.ano)
Média <- as.numeric(vetor.medio.gpuclock)
df.gpu.clock.medio <- data.frame(Ano,Média)
plot.linha <- ggplot(df.gpu.clock.medio, aes(x = Ano,y = Média, group = 1)) + geom_line() + geom_point() + labs(x = "Anos", y = "Velocidade das GPUS(MHz)")
ggplotly(plot.linha)

#Gr?fico de disper??o gpu clock, memory clock
plot.dispercao <- ggplot(df.timestamped, aes(x=gpuClock, y=memClock, color = releaseYear)) + geom_point() + labs(x = "Clock da GPU", y = "Clock da memória", color = "Ano de lançamento")
ggplotly(plot.dispercao)

#placas lan?adas nos anos setados
df.split.ano <- split(df.timestamped, df.timestamped$releaseYear)
vetor.ano <- c()
vetor.quantidade.ano <- c()
for(i in 1:(LSANO - LIANO+1)){
  vetor.quantidade.ano[i] <- length(df.split.ano[[i]]$productName)
  vetor.ano[i] <- df.split.ano[[i]]$releaseYear[i]
}
Quantidade <- vetor.quantidade.ano
df.quantidade.ano <- data.frame(Ano, Quantidade)

plot.ano <- ggplot(df.quantidade.ano,aes(x = Ano, y = Quantidade))+
  geom_bar(stat="identity") + labs(x = "Ano", y = "Quantidade de placas lançada")
ggplotly(plot.ano)


#Montando gráfico dos conectores
list.conector <- split(df.timestamped, df.timestamped$bus)
vetor.quantidade.conector <- c()
vetor.conector <- c()
for(i in 1:length(list.conector)){
  vetor.conector[i] <-  names(list.conector[i])
  vetor.quantidade.conector[i] <- length(list.conector[[i]]$productName)
  }

#Utilizando dataframe anterior e transformando em obj Ploty

Conector <- reorder(vetor.conector, vetor.quantidade.conector)
Quantidade <- vetor.quantidade.conector

df.quantidade.conector <- data.frame(Conector, Quantidade)

plot.quantidade.conector<- ggplot(df.quantidade.conector, aes(x = Conector, y = Quantidade)) + geom_bar(stat = "identity") + coord_flip() + labs(y = "Quantidade", x = "Conector")
ggplotly(plot.quantidade.conector)



