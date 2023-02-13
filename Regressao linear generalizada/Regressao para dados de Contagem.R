# Este arquivo é de autoria de César Augusto Taconelli

### Análise dos dados de quedas de idosos, disponível no arquivo geriatra.dat 
### da página do professor Gilberto de Paula e no pacote labestData.

### Objetivo - analisar o efeito de duas diferentes intervenções na prevenção de
### quedas de idosos, ajustando o efeito de outras covariáveis.

### Pacotes necessários
require(car)
require(lmtest)
require(effects)
require(statmod)
require(labestData)
require(hnp)
require(coefplot)
require(latticeExtra)

options(device = 'x11')

################################################################################

### Carregando os dados e análise descritiva.

geriatra <- read.csv2('Idosos.csv')  ### Leitura dos dados.
names(geriatra) ### Listando os nomes das variáveis
head(geriatra,10) ### Visualizando as 10 primeiras linhas da base.
summary(geriatra) ### Breve sumário.

### As próximas linhas de comandos declaram sexo e intervenção como fatores, e renomeiam seus níveis.
geriatra$interv <- factor(geriatra$interv)
geriatra$sexo <- factor(geriatra$sexo)
levels(geriatra$interv) <- c('Educ','Educ+Exerc')
levels(geriatra$sexo) <- c('Fem','Masc')

### Vamos calcular o número médio de quedas e a variância para os idosos separados por intervenção.
tapply(geriatra$nquedas,geriatra$interv,mean)
tapply(geriatra$nquedas,geriatra$interv,var)

### Gráficos para a distribuição de frequências para o número de quedas.

par(cex=1.4,las=1)
barplot(table(geriatra$nquedas),col='red',xlab='Número de quedas',ylab='Frequência')

par(cex=1.2, las=1, mfrow=c(1,2))
barplot(table(geriatra$nquedas[which(geriatra$interv=='Educ')]),col='red',
        xlab='Número de quedas',ylab='Frequência',xlim=c(0,12),main='Educação')
barplot(table(geriatra$nquedas[which(geriatra$interv=='Educ+Exerc')]),col='blue',
        xlab='Número de quedas',ylab='Frequência',xlim=c(0,12),main='Educação + Exercícios')

################################################################################

### Antes de ajustar um glm, vamos ver como ficaria um modelo linear normal ajustado a este problema.
ajuste <- lm(nquedas~.,data=geriatra)
par(mfrow=c(2,2))
plot(ajuste)
### Repare, no gráfico do canto superior direito, a falta de normalidade dos resíduos e, 
### no gráfico do canto inferior esquerdo, como a dispersão dos resíduos
### aumenta conforme a média, claro indicativo de que a variância dos erros não é constante. 

################################################################################

### Agora, o ajuste do modelo log-linear Poisson.
ajuste <- glm(nquedas~., family=poisson, data=geriatra) 
### Modelo com todas as covariáveis, sem interações.

summary(ajuste)

### Vamos verificar se alguma interação envolvendo o efeito de intervenção é necessária.

ajusteint1 <- update(ajuste, ~.+interv:balan)
ajusteint2 <- update(ajuste, ~.+interv:forca)
ajusteint3 <- update(ajuste, ~.+interv:sexo)

anova(ajuste, ajusteint1, test='Chisq')
anova(ajuste, ajusteint2, test='Chisq')
anova(ajuste, ajusteint3, test='Chisq')

### O que vocês dizem?

### Vamos investigar a necessidade de incluir o efeito quadrático das covariáveis
# numéricas.

ajustequad1 <- update(ajuste, ~. + I(balan**2))
ajustequad2 <- update(ajuste, ~. + I(forca**2))

anova(ajuste,ajustequad1,test='Chisq')
anova(ajuste,ajustequad2,test='Chisq')
### Não há necessidade de inclusão dos termos quadráticos.


### Assim, vamos optar pelo modelo original, mas excluindo o efeito de sexo,
# que não se mostrou significativo.

ajuste2 <- update(ajuste, ~.-sexo)
summary(ajuste2)

### INTERPRETAÇÔES!!!

################################################################################

### Diagnóstico do ajuste.

### Gráficos padrão para a função glm.
par(mfrow = c(2,2))
plot(ajuste2, which = 1:4)
# Não há indicativos sérios de falta de ajuste. Os resíduos têm variância aprox.
# constante, distribuição próxima da Normal, não parece haver outliers.

### Gráfico dos resíduos da deviance com envelope simulado.
hnp(ajuste2)
# No geral, os pontos se distribuem no interior do envelope. 

### Análise dos resíduos quantílicos aleatorizados.

resquant <- qres.pois(ajuste2)
qqnorm(resquant)
qqline(resquant)
### Aproximação satisfatória à distribuição Normal, ligeiro desajuste na cauda
# à direita.

shapiro.test(resquant) 
### Não se rejeita a hipótese de normalidade.

influenceIndexPlot(ajuste,vars=c('Studentized','Cook','Hat'),id.n=3)

### Vamos avaliar algumas observações destacadas no diagnóstico do modelo.

summary(geriatra)
geriatra[c(52,62,93),]
plot(geriatra$balan, geriatra$forca)

# Os indivíduos 52 e 93 apresentaram os maiores números de quedas na amostra 
# (11 e 10, respectivamente). O de número 62 se destaca pelo maior escore de balanço,
# com escore de força abaixo do 1º quartil.

### Vamos reajustar o modelo sem esses indivíduos.

ajuste2alt <- update(ajuste2,subset=-c(52,62,93)) 
### Atualizando o modelo, excluindo as três observações.

### Vamos comparar os ajustes.
compareCoefs(ajuste2,ajuste2alt) 
multiplot(ajuste2, ajuste2alt) ### Pacote coefplot
summary(ajuste2alt)

# A única mudança substancial corresponde ao efeito do escore de força que,
# mediante exclusão das três observações, perde sua significância. O efeito
# da intervenção (e decorrentes conclusões) não são alteradas.

########################################################################
### Vamos explorar um pouco mais os resultados.

### Intervalos de confiança.

confint.default(ajuste2)
exp(confint.default(ajuste2))
### Intervalos de confiança baseados na normalidade assintótica dos estimadores.

confint(ajuste2)
exp(confint(ajuste2))
### Intervalos de confiança baseados nos perfis da verossimilhança.

### Explorando um pouco mais os efeitos das variáveis explicativas 
# (consultar a documentação da função effect, pacote effects).
plot(allEffects(ajuste2)) 
plot(allEffects(ajuste2), type = 'response')


########################################################################

### Um pouco de predição. Vamos estimar o número médio de quedas para
# indivíduos com dois perfis distintos, submetidos a cada uma das 
# intervenções.

# Perfil 1- Balanço=30, força=50; Perfil 2- Balanço=70, força=80.

novosdados <- data.frame(interv=c('Educ','Educ+Exerc','Educ','Educ+Exerc'),
                         balan=c(30,30,50,50),
                         forca=c(70,70,80,80))
novosdados

estimativas <- predict(ajuste2,novosdados,type='response'); estimativas
data.frame(novosdados,estimativas)

### Vamos olhar para as distribuições ajustadas para os números de quedas 
# dois perfis, sob cada intervenção.

x11()
par(mfrow=c(2,2),cex=1,2)
plot(0:10,dpois(0:10,estimativas[1]),type='h',xlab='Número de quedas',
     ylab='Probabilidade estimada',main='Perfil 1, Educ',lwd=2)

plot(0:10,dpois(0:10,estimativas[2]),type='h',xlab='Número de quedas',
     ylab='Probabilidade estimada',main='Perfil 1, Educ + Exerc',lwd=2)

plot(0:10,dpois(0:10,estimativas[3]),type='h',xlab='Número de quedas',
     ylab='Probabilidade estimada',main='Perfil 2, Educ',lwd=2)

plot(0:10,dpois(0:10,estimativas[4]),type='h',xlab='Número de quedas',
     ylab='Probabilidade estimada',main='Perfil 2, Educ + Exerc',lwd=2)


### Vamos estimar, para cada perfil, a probabilidade de haver alguma queda no período.

ppois(0, estimativas[1], lower.tail=F) # P(Y>1|mu=estimativa[1]).
ppois(0, estimativas[2], lower.tail=F) # P(Y>1|mu=estimativa[2]).
ppois(0, estimativas[3], lower.tail=F) # P(Y>1|mu=estimativa[3]).
ppois(0, estimativas[4], lower.tail=F) # P(Y>1|mu=estimativa[4]).


### Vamos trabalhar um pouco com simulação.
### Podemos utilizar simulação (bootstrap) como alternativa ao uso da 
# teoria assintótica para avaliação dos erros das estimativas. 
# Vamos usar a função Boot do pacote car. 

ajusteboot <- Boot(ajuste2, R=999)

### Armazenamos em ajusteboot 999 os resultados dos R=999 ajustes do glm
# proposto para 999 re-amostras de tamanho n=100 geradas com reposição da base original.

summary(ajusteboot)
### Repare que as estimativas bootstrap (em bootMed) e os erros padrões (em bootSE) 
# são bem similares às obtidas originalmente, baseadas na teoria assintótica.
### Além disso, o viés é bastante pequeno (desprezível).

hist(ajusteboot) 
### Podemos verificar que a distribuição dos estimadores, obtida via 
# simulação, se aproxima bastante da distribuição Normal.

exp(confint(ajusteboot))
exp(confint(ajuste2)) 
### Intervalos de confiança obtidos das duas formas são bastante próximos. 

### Exercício - testar a qualidade do ajuste com base nas estatísticas 
# da deviance e X2 de Pearson.

### Exercício - Obter intervalos de confiança (95%) para o número médio 
# de quedas de idosos com os dois perfis apresentados.

### Exercício - Avaliar o efeito das três observações destacadas no diagnóstico
# tirando uma a uma do ajuste do modelo e checar a alteração nos resultados. 
