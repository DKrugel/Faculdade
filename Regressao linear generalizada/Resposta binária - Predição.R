# Este arquivo é de autoria de César Augusto Taconelli

require(ROCR)

### Dados sobre concessão numa instituição crédito alemã.
### Na página da disciplina há um arquivo com informações sobre as variáveis.

### Início da análise
dados <- read.table('https://docs.ufpr.br/~taconeli/CE22516/Credito.txt', 
                    header = T,
                    stringsAsFactors = TRUE)
head(dados)
summary(dados)

dados$Status <- factor(dados$Status)
levels(dados$Status) <- c('Good', 'Bad')
dados$Status <- relevel(dados$Status, ref = 'Bad')
### O motivo de redefinir o nível de referência é para modelarmos a probabilidade
### de um bom pagador. Para isso, devemos definir o nível de referência como o
### outro (Bad).

dim(dados)
summary(dados$Status)

### Exercício - Fazer uma análise descritiva (uni e bivariada), buscando avaliar, 
### de forma preliminar por meio de gráficos e medidas resumo, 
### a relação entre as covariáveis e a resposta.

### Vamos separar, aleatoriamente, a base em duas: uma para o ajuste do 
### modelo (com 700 observações) e outra para validação (com 300 observações).

set.seed(898)
indices <- sample(1:1000, size = 700) 
### indices é um vetor com números de 1 a 1000 numa sequência aleatória.

dadosajuste <- dados[indices,]
### dataframe com as 700 linhas para ajuste.

dadosvalid <- dados[-indices,]
### dataframe com 300 linhas, apenas para validação.


########################################################################

### Vamos ajustar um modelo de regressão logística, com as covariáveis selecionadas
### via Backward(AIC)

ajuste <- step(glm(Status ~ .,family=binomial,data = dadosajuste), direction = 'backward') 
data.frame(ajuste$y, dadosajuste$Status)
### Apenas para conferir para quais categorias está atribuindo 0 e 1.

summary(ajuste)
### Resumo do ajuste.

paj2 <- predict(ajuste, type='response', newdata=dadosajuste)
### Probabilidades estimadas para os indivíduos da base de ajuste;

pval2 <-predict(ajuste, type='response', newdata=dadosvalid) 
### Probabilidades estimadas para os indivíduos da base de validação.

round(pval2 * 1000) ### Escores de crédito.

################################################################################
### Usando o modelo ajustado para classificação dos clientes da base de validação.

### Como fica se usarmos pc=0,5 como ponto de corte para classificação, ou seja:
# Classificar como mau pagador se p < 0,5;
# Classificar como bom pagador se p >= 0,5.

### Vamos cruzar realidade e predição para diferentes pontos de corte.

classp0.5 <- factor(ifelse(pval2 >= 0.5, 'PredGood', 'PredBad'))
classp0.5 ### Classificações usando o ponto de corte 0,5.
data.frame(pval2, classp0.5) ### Probabilidades estimadas e classificações.

### Classificando como "Devedores" indivíduos com p > 0,5.
tabela1 <- table(classp0.5,dadosvalid$Status) 
tabela1

### Vamos estimar a sensibilidade e a especificidade referentes a esta regra de decisão.
sensp0.5 <- sum(classp0.5 == 'PredGood' & dadosvalid$Status == 'Good')/sum(dadosvalid$Status == 'Good')
sensp0.5
espec0.5 <- sum(classp0.5 == 'PredBad' & dadosvalid$Status == 'Bad')/sum(dadosvalid$Status == 'Bad')
espec0.5

### E se usássemos os dados de ajuste para calcular a sensibilidade e a especificidade?

classp0.5aj <- factor(ifelse(paj2 >= 0.5, 'PredGood', 'PredBad'))
sensp0.5aj <- sum(classp0.5aj == 'PredGood' & dadosajuste$Status == 'Good')/sum(dadosajuste$Status == 'Good')
sensp0.5aj
espec0.5aj <- sum(classp0.5aj == 'PredBad' & dadosajuste$Status == 'Bad')/sum(dadosajuste$Status == 'Bad')
espec0.5aj
# Observe que ao usar os dados de ajuste, tanto a sensibilidade quanto a especificidade
# são maiores. Isso reforça a importância de se usar dados de validação para
# avaliar adequadamente o desempenho preditivo do modelo.

# Observe que ao usar pc = 0,5, temos uma regra de classificação com elevada 
# sensibilidade (0,84), mas baixa especificidade (0,49). Assim, a capacidade
# de se predizer um bom pagador é alta, mas o poder preditivo para maus pagadores
# é baixo (vamos acertar aproximadamente metade das vezes).


### E se relaxássemos, classificando como pagador (e concedendo empréstimo a) todo indivíduo com p>=0,3?
classp0.3 <- factor(ifelse(pval2 >= 0.3,'PredGood', 'PredBad'))
classp0.3
tabela2 <- table(classp0.3 , dadosvalid$Status) 
tabela2 

sensp0.3 <- sum(classp0.3 == 'PredGood' & dadosvalid$Status == 'Good')/sum(dadosvalid$Status == 'Good')
sensp0.3 
espec0.3 <- sum(classp0.3 == 'PredBad' & dadosvalid$Status == 'Bad')/sum(dadosvalid$Status == 'Bad')
espec0.3 

### Ao tomar pc=0,3, a sensibilidade aumenta ainda mais, mas a especificidade
# (probabilidade de classificar corretamente um mau pagador) fica baixíssima.


### E se fossemos mais rigorosos, classificando como pagador (e concedendo empréstimo a) apenas indivíduos com p>=0,7?

classp0.7 <- factor(ifelse(pval2 >= 0.7,'PredGood', 'PredBad'))
classp0.7

### Classificando como "Devedores" indivíduos com p>0,7.
tabela3 <- table(classp0.7,dadosvalid$Status)
tabela3
sensp0.7 <- sum(classp0.7 == 'PredGood' & dadosvalid$Status == 'Good')/sum(dadosvalid$Status == 'Good')
sensp0.7
espec0.7 <- sum(classp0.7 == 'PredBad' & dadosvalid$Status == 'Bad')/sum(dadosvalid$Status == 'Bad')
espec0.7

### Para essa regra, temos sensibilidade e especificidade aproximadamente iguais.

### Botando num quadro:

datacomp <- data.frame(c(sensp0.3,sensp0.5,sensp0.7),
                       c(espec0.3,espec0.5,espec0.7))
names(datacomp)<-c('Sensibilidade','Especificidade')
rownames(datacomp)<-c('pc=0,3','pc=0,5','pc=0,7')
datacomp

################################################################################

### Vamos usar o pacote ROCR para calcular algumas medidas de qualidade preditiva.

pred <- prediction(pval2, dadosvalid$Status)
help('performance')

### Vamos plotar a curva ROC
perf <- performance(pred, measure = "tpr" , x.measure = "fpr") 
# tpr: True Positive Rate; fpr: False Positive Rate.

plot(perf, colorize=TRUE,  print.cutoffs.at=seq(0.05,0.95,0.05), lwd = 2)
abline(0,1, lty = 2)

print(performance(pred, 'auc'))
# Extraindo a área sob a curva.

### Como ficaria a curva ROC se usássemos os dados de ajuste para sua construção?

predaj <- prediction(paj2, dadosajuste$Status)
perfaj <- performance(predaj, measure = "tpr" , x.measure = "fpr") 
plot(perfaj, col = 'red', lwd = 2, add = T)
performance(predaj, 'auc')

### Agora, vamos supor que o custo de classificar um bom pagador como mau seja
### três vezes o de classificar um mau como bom (3*C(M|B) = C(B|M)). Assim, para
### uma regra de classificação com probabilidades de classificação incorretas
### P(M|B) e P(B|M), o custo esperado de má classificação (ECMC) fica dado por:

##ECMC = C(M|B)*P(M|B)*P(B) + C(B|M)*P(B|M)*P(M)##

## Como na base toda há 30% de maus pagadores, vamos tomar P(M) = 0,3; P(B) = 0,7.

# Assim, ECMC = 0.7*P(M|B) + 3*0.3*P(B|M). Vamos avaliar ECMC para diferentes regras.

perf2 <- performance(pred, measure = 'fpr', x.measure = 'fnr')
PBM <- perf2@y.values[[1]] ### P(B|M) para os diferentes pontos de corte (pc)
PMB <- perf2@x.values[[1]] ### P(M|B) para os diferentes pontos de corte.
pc <- perf2@alpha.values[[1]] ### pontos de corte.

ECMC <- 0.7*PMB + 3*0.3*PBM ### Custos esperados de má-classificação.

x11()
plot(pc, ECMC)
### Uma regra de decisão baseada num ponto de corte em torno de 0.8 parece satisfatória.

### Exercício - Repetir a análise considerando outros modelos. Você pode tentar
### modelos com preditores e/ou funções de ligação diferentes. 