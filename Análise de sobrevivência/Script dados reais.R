library(prettyR)
library(tidyverse)

View(dados)


class(dados)
str(dados)


for(i in 1:length(dados)){
  boxplot(dados[,i])
  title(colnames(dados)[i])
}

describe(dados)
which(dados$Cor == 1.77)
dados_ <- dados[-577,]
dados_$Cor <- fct_drop(dados_$Cor)

ddados_ <- describe(dados_)
dados_$Idade

my3cols <- c("#E7B800", "#2E9FDF", "#FC4E07")

d <- dados_ %>% 
  filter(Sexo == 1) %>% 
  select(Idade) %>% 
  ggplot() + 
  geom_histogram(aes(x = Idade), fill = "#FC4E07")

dp <- dados_ %>% 
  filter(Sexo == 2) %>% 
  select(Idade) %>% 
  ggplot() + 
  geom_histogram(aes(x = Idade), fill = "#E7B800")

dados_ %>%
  ggplot(aes(x = Idade, fill = Sexo))+
    geom_histogram(position = "identity", alpha = 0.3)
  
graficos <- list()

graficos <- dados_ %>% 
  split(~Cor) %>% 
  map(~ggplot(aes(x = Idade, fill = Sexo), data = .)+geom_histogram(position = "identity",alpha = 0.3))

graficos[[3]]

graficos2 <- list()


graficos2 <- dados_ %>% 
  split(~AneurismaVD) %>% 
  map(~ggplot(aes(x = tempo), data = .)+geom_density() + ylim(c(0,0.001)))

graficos2[[1]]
graficos2[[2]]

graficos3 <- dados_ %>% 
  split(~SorologiaChagas) %>% 
  map(~ggplot(aes(x = tempo), data = .)+geom_density() + ylim(c(0,0.001)))

graficos3[[1]]
graficos3[[2]]


