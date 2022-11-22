### Exemplo - Mínimos quadrados ponderados. Vamos utilizar o data frame 
# cars, disponível na base do R.

require(car) 
### Deste pacote vamos usar a função ncvTest, que permite 
# testar a hipótese de variância constante para o erro.  
require(nlme) 
### Desse pacote vamos utilizar a função gls, para ajustar 
# um modelo alternativo baseado em mínimos quadrados ponderados.

help(cars) 
### Consultando a documentação dos dados, para entender as variáveis.

options(scipen = 3, device = 'x11', width = 80)

head(cars,10) 
### Visualizando as dez primeiras linhas da base.

summary(cars) 
### Extraindo algumas descritivas.

par(cex=1.4, las=1)
plot(cars, pch=20, xlab='Velociadade (mph)', ylab='Distância de frenagem (m)') 
### Gráfico de dispersão da distância de frenagem em relação à velocidade do veículo.

with(cars, lines(lowess(dist~speed), col='red', lwd=2)) 
### Ajustando uma curva de regressão não paramétrica 
### (apenas para verificar melhor a relação entre as variáveis. Não entraremos em 
### detalhes sobre este procedimento agora).


### Claramente há uma relação entre as variáveis (a distância de frenagem 
### aumenta conforme a velocidade do veículo). Vamos assumir que a relação 
### entre as variáveis é linear. Fica como exercício para o aluno tentar 
### outras possibilidades, como ajustar um modelo polinomial ou transformar 
### uma das variáveis (ou ambas). 


########################################################################
###1º Ajuste

ajuste <- lm(dist ~ speed, data = cars) 
### ajuste armazena o modelo de regressão linear ajustado para as duas variáveis.

summary(ajuste) 
### Estima-se que a distância de frenagem  aumente, em média, aproximadamente 
### 4 metros (3,93) a cada aumento de 1 mph na velocidade do veículo. 
### Na prática, o intercepto não tem interpretação. Por que?

residuos <- rstandard(ajuste) 
### Extraindo os resíduos (padronizados) do ajuste.

par(cex=1.4, las=1, mfrow=c(1,2))
plot(cars$speed, residuos, xlab='Velociadade (mph)', ylab='Resíduos', 
     pch=20,cex=1.5,ylim=c(-2,3))
### Gráfico de resíduos vs velocidade. Há claros indicativos de que a 
# variância dos resíduos aumenta com a velocidade

par(mfrow = c(2,2))
plot(ajuste, which = 1:4)

ncvTest(lm(dist~speed, data=cars)) 
### A hipótese nula do teste é que a variância dos resíduos é constante 
### em relação à velocidade. O teste fornece evidência significativa de 
### que a variância não é constante (p=0,03).


########################################################################
###2º Ajuste

### Vamos considerar que a variância dos resíduos aumente linearmente com 
### a velocidade do veículo, o que nos motiva a usar como pesos 1/velocidade.

ajuste2 <- lm(dist ~ speed, weights=1/speed, data=cars) 
### Incorporamos os pesos por meio do argumento "weights".

residuos2 <- rstandard(ajuste2)
plot(cars$speed, residuos2, xlab='Velociadade (mph)', ylab='Resíduos', 
     pch=20,cex=1.5,ylim=c(-2,3))
### Repare que o padrão verificado nesse gráfico (de variação não constante 
### para os resíduos) não é evidente se comparado ao primeiro ajuste.

ncvTest(lm(dist~speed, weights=1/speed, data=cars)) 
### O teste de Breusch-Pagan não indica a rejeição da hipótese de variância 
### constante (p=0,32).

summary(ajuste2) 
compareCoefs(ajuste,ajuste2) 
### A função compareCoefs dispõe lado a lado estimativas e erros padrões 
### de dois ou mais modelos ajustados.

### Observe que a estimativa do efeito da velocidade na distância de 
### frenagem é menor (3.63, contra 3.93 no ajuste1), mas seu erro padrão
### também é menor (0.34, contra 0.41 no ajuste1). De qualquer forma, novamente 
### se verifica relação significativa entre as variáveis.


############################ 3º Ajuste 
### Vamos considerar agora que a relação entre a variação dos erros e a 
### velocidade seja desconhecida, mas desejamos estimá-la. Uma maneira de 
### fazer isso é assumir uma forma não completamente especificada para essa 
### relação, envolvendo parâmetros, e estimar esses parâmetros juntamente 
### com os demais parâmetros do modelo.

### Poderíamos especificar varias funções. Vamos considerar uma, implementada 
# no pacote nlme: DP(Erros)=alpha+velocidade^beta, ou seja, estamos assumindo 
# uma relação do tipo potência, onde alpha e beta são parâmetros a serem estimados.

ajuste3 <- gls(dist ~ speed, data=cars, weight = varConstPower(form =~ speed))
residuos3 <- residuals(ajuste3, type='normalized')

plot(cars$speed,residuos3, xlab='Velociadade (mph)', ylab='Resíduos', pch=20,
     cex=1.5, ylim=c(-2,3))
compareCoefs(ajuste, ajuste2, ajuste3)
summary(ajuste3)

### Observe que a estimativa da potência (power=1,022) indica relação 
### (aproximandamente) linear entre o desvio padrão dos resíduos e a velocidade, e,
### consequentemente, quadrática entre a variância dos erros e a velocidade.
