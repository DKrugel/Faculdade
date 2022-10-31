require(effects)
require(car)
require(leaps) 

### Exemplo - Regressão Linear Múltipla para os dados de vendas de cosméticos.
### O objetivo é propor e ajustar um modelo de regressão para explicar o 
### total em vendas em função do tempo de serviço, idade, anos de estudo 
### e Populacao atendida pelas vendedoras.

vendas <- read.csv2("C://Users//cetac//Dropbox//CE225_Oferta_2022//Graduacao//UDs//UD02//Normal2.csv")

names(vendas) <- c('Tempo', 'Idade', 'Estudo', 'Populacao', 'Venda')

summary(vendas) ### Resumo numérico dos valores das cinco variáveis disponíveis na amostra.

options(digits = 5, scipen = 5)

x11()
require(car)
par(cex=1.2)
scatterplotMatrix(vendas, pch = 20, cex.labels = 1.25) 
### Matriz de gráficos de dispersão 
### (com gráficos das densidades estimadas na diagonal).
### Os gráficos de dispersão fornecem um primeiro indicativo de tendência 
### crescente entre Populacao atendida e total de vendas e, com menor 
### intensidade, entre total de vendas e tempo de serviço.

ajuste1 <- lm(Venda ~ Tempo+Estudo+Populacao+Idade, data=vendas) 
### Ajuste da regressão linear múltipla, considerando as quatro covariáveis.
### Nota: uma forma equivalente de ajustar o modelo acima seria ajuste1=lm(Venda~.,data=vendas)

print(ajuste1) 
### Extraindo as estimativas dos parâmetros da regressão.

summary(ajuste1)
### Os resultados do ajuste indicam que o total em vendas aumenta conforme 
### o tempo de experiência e a Populacao atendida.
### Para as demais variáveis, não há evidência significativa de que estejam 
### relacionadas ao total em vendas.
### Estima-se que o total em vendas aumente, em média, R$174,37 para um ano 
### a mais de experiência, mantendo-se fixas as demais covariáveis.
### Quanto à Populacao atendida, estima-se, em média, R$0,628 a mais por pessoa. 
### Assim, para 1000 pessoas a mais na Populacao, estima-se, em média, R$628 
### a mais no total de vendas, mantendo-se fixas as demais covariáveis.

ajuste1sc <- lm(Venda ~ scale(Tempo)+scale(Idade)+scale(Estudo)+scale(Populacao), data=vendas)
summary(ajuste1sc)
### Neste novo modelo, todas as variáveis explicativas foram padronizadas
### (subtraímos, de cada valor, a média da variável e dividimos pelo
### respectivo desvio padrão). Assim, as variáveis agora têm mesma 
### escala e seus efeitos são diretamente comparáveis.

fitted(ajuste1) 
### Valores ajustados pelo modelo de regressão linear múltipla para as 200 vendedoras.

plot(fitted(ajuste1), vendas$Venda,pch=20, xlab='Vendas ajustadas', ylab='Vendas observadas',las=1,cex = 1.2) 
abline(0,1,col='red',lwd = 2)
### Gráfico de valores observados pelos valores ajustados.

confint(ajuste1) 
### Intervalos de confiança (95%) para os parâmetros do modelo.


### Agora, algumas predições.

dnovos <- data.frame(Tempo=c(3,15), Idade=c(30,30), Estudo=c(10,10), Populacao=c(1000,10000)) 
dnovos
### Dois perfis de vendedoras para estimarmos o total médio de vendas e predizer o total de vendas

predict(ajuste1, newdata=dnovos) ### Estimativas pontuais.

predict(ajuste1, interval='confidence', newdata=dnovos) 
### Intervalos de confiança (95%) para o total médio de vendas.

predict(ajuste1, interval='prediction', newdata=dnovos) 
### Intervalos de predição (95%) para a predição do total de vendas.

### Agora, vamos fazer estimativas e predições "no atacado", usando todos 
### os vetores de covariáveis (perfis de vendedoras) da base.

p1 <- predict(ajuste1, interval='confidence', newdata=vendas)
vendas2 <- cbind(vendas[,1:4], p1); head(vendas2, 10) 
### Em vendas2 armazenamos os dados e os correspondentes ICs (95%) para 
### o total médio de vendas.

p2 <- predict(ajuste1, interval='prediction', newdata=vendas)
vendas3 <- cbind(vendas[,1:4], p2) ;head(vendas3, 10) 
### Em vendas3 armazenamos os dados e os correspondentes IPs (95%) para o 
### total de vendas de novas vendedoras.

### Vamos usar o teste F, baseado na extra soma de quadrados para testar algumas hipóteses.
# H0: Beta(idade)=0 vs H1: Beta(idade)!=0 (testando a significância do efeito da idade)

ajuste2 <- lm(Venda~.-Idade,data=vendas) 
### Modelo de regressão linear múltipla sem considerar a covariável idade.

anova(ajuste2,ajuste1) 
### Repare que o acréscimo na soma de quadrados (vamos chamar de ASQ) de 
### regressão associado à inclusão do efeito de idade é de 2363660, ao qual 
### temos um grau de liberdade associado.

### A estatística do teste é F=(ASQ/1)/QMRes, onde QMRes é o quadrado médio 
### de resíduos do modelo "maior", que pode ser verificado batendo anova(ajuste1). 

### Sob H0, F~F(1,195) (faça o teste à mão, para um nível de significância de 5%).

### Algumas considerações:
# 1 - O efeito de idade é não significativo, o que poderia 
# justificar a exclusão dessa variável do modelo.

# 2 - O teste F é equivalente ao teste t apresentado no summary.
# Isso não é coincidência, e sempre ocorrerá quando houver apenas um 
# parâmetro do modelo sob teste.

# 3 - Repare a saída da função anova() aplicada a um único modelo. 
# Ela apresenta a seguinte sequência de testes:

anova(ajuste1)

# I-  Linha 1 - Testa a inclusão do efeito de Tempo ao modelo nulo 
# (sem covariáveis) - significativa.

# II-  Linha 2 - Testa a inclusão do efeito de Idade ao modelo com a 
# covariável tempo - não significativa. 

# III-  Linha 3 - Testa a inclusão do efeito de Anos de estudo ao modelo 
# com as covariáveis tempo e idade - não significativa. 

# IV-  Linha 4 - Testa a inclusão do efeito de Populacao ao modelo com as 
# covariáveis tempo, idade e anos de estudo - significativa. 

# Nota - Em todos os testes, usa-se QMRes produzida pelo modelo ajustado 
# com mais parâmetros (no caso, o modelo com as quatro covariáveis).
# Repare que os testes não são equivalentes aos do summary (exceto o último). 
# Por que?

# Pergunta - o que aconteceria com a sequência de testes se você mudasse 
# a ordem de entrada das variáveis na função lm? 

ajuste2 <- lm(Venda~Populacao+Estudo+Idade+Tempo, data=vendas) 
### Ajuste da regressão linear múltipla, considerando as quatro covariáveis, 
### com elas inseridas em ordem invertida.
anova(ajuste2)

# Agora, vamos dar uma olhada no resultado da função Anova (com A maiúsculo), 
# disponível no pacote car.

Anova(ajuste1) ### A sequência de testes agora é a seguinte:
# I-  Linha 1 - Testa a inclusão do efeito de Tempo ao modelo ajustado 
# com as outras três covariáveis - significativa. 

# II-  Linha 2 - Testa a inclusão do efeito de Idade ao modelo ajustado 
# com as outras três covariáveis - não significativa. 

# III-  Linha 3 - Testa a inclusão do efeito de Anos de estudo ao modelo 
# ajustado com as outras três covariáveis - não significativa. 

# IV-  Linha 4 - Testa a inclusão do efeito de Populacao ao modelo ajustado 
# com as outras três covariáveis - significativa. 

# Repare que os testes fornecidos por essa função são equivalentes aos do summary. Por que?

Anova(ajuste2) ### Com as variáveis inseridas em ordem diferente... Mesmo resultado.

### Agora, vamos usar o teste F para testar a hipótese de nulidade conjunta 
### H0:Beta(Idade)=Beta(Estudo)=0. Esse teste permite verificar a contribuição 
##conjunta das duas variáveis ao modelo.

ajuste3 <- lm(Venda ~ Populacao+Tempo,data=vendas)  
### ajuste3 armazena o modelo ajustado sob H0, ou seja, não incluindo as 
### covariáveis para as quais estamos postulando que o correspondente parâmetro é nulo.

anova(ajuste3,ajuste1) ### Repare que a extra-soma de quadrados (10399916, 
###com 2 graus de liberdade) não é significativa (p=0,2421), o que pode 
### justificar a exclusão das duas covariáveis (Estudo e Idade) do modelo.

### O teste é obtido por (ASQ/2)/QMRes, sendo QMRes=3639505 extraída do 
### modelo ajustado com mais parâmetros.

### Nota: Ao observar que duas covariáveis não são marginalmente significativas 
### no ajuste de um modelo, isso não implica que elas sejam não significativas 
### conjuntamente. A não significância conjunta deve ser testada com base 
### no teste F da extra-soma de quadrados.

### Agora, vamos explorar o efeito das covariáveis usando gráficos.
plot(allEffects(ajuste1))
### Cada gráfico apresenta a estimativa do total de vendas (e bandas de 
### confiança 95%) para os valores de uma variável (representados no eixo
### horizontal) fixando as demais variáveis na média.


##########################################################################################################################################################

### Diagnóstico - primeiramente vamos extrair os diferentes tipos de resíduos.

e1 <- resid(ajuste1);e1 #
## Resíduos ordinários.

sigma <- summary(ajuste1)$sigma;qmres 
### Extraindo (a raiz do) quadrado médio de resíduos produzido pelo ajuste.

d1 <- e1/sigma;d1 
### Resíduos padronizados.

r1 <- rstandard(ajuste1);r1 
### Resíduos studentizados (internamente).

r2 <- rstudent(ajuste1);r2 
### Resíduos studentizados (externamente).

### Veja a documentação da função rstudent para relembrar a diferença desses
### dois tipos de resíduos.

plot(r1,r2) 
### Repare que há pouca diferença nos valores dos dois tipos de resíduos.

fit <- fitted(ajuste1)

### Daqui por diante, vamos usar apenas os resíduos studentizados internamente.

par(mfrow=c(2,2))
plot(ajuste1, which = 1:4, cex = 1.2, pch = 20) 
### Conjunto de gráficos de diagnóstico padrão do R.
### O gráfico do canto superior esquerdo é de resíduos ordinários versus 
### valores ajustados. Repare que os resíduos estão dispersos aleatoriamente,
### sem valores extremos ou padrões não aleatórios. 
### Vamos fazer um gráfico semelhante, mas baseado nos resíduos studentizados:

par(cex=1.4,las=1)

plot(fit, r1, xlab='Valores ajustados', ylab='Resíduos studentizados', pch=20)
### Repare que uma das observações gerou resíduo superior a 3. Vamos identificá-la.

### Vamos identificar a observação com resíduo superior a 3.
identify(fit,r1,rownames(vendas))
### A observação com resíduo superior a 3 está na linha 195. Vamos avaliá-la:

vendas[195,]
summary(vendas)
### Essa é a vendedora com maior total em vendas. Repare que, embora ela
### tenha 14 anos de experiência em vendas, atende uma Populacao pequena, de tamanho 11995. 
### Logo, seu desempenho real foi bem superior ao esperado.

### Voltando aos gráficos do R. O gráfico do canto superior direito é o 
### gráfico probabilístico normal. Repare um leve afastamento da distribuição normal
### à esquerda (para valores negativos). Isso não é preocupante, mas vamos 
### fazer um teste para checar a suposição de normalidade.

shapiro.test(r1) 
### A hipótese nula do teste de Shapiro-Wilks é a hipótese de normalidade. 
### Repare que não se tem evidência significativa contra essa hipótese.
### Logo, a normalidade dos resíduos está verificada.


### O gráfico do canto inferior esquerdo é o gráfico da raiz quadrada dos 
### resíduos studentizados (em módulo) versus valores ajustados. 
### Serve para avaliar a suposição de variância constante (dentre outros).

### Não há indícios de heterocedasticidade (variância não constante). 
### Repare que no gráfico são assinalados os maiores resíduos, o que já 
### havíamos identificado anteriormente. 

### Podemos usar o teste de Breusch-Pagan para testar a hipótese de variância 
### constante para os erros. A hipótese nula é a de variância constante e o teste
### baseia-se na distribuição qui-quadrado. Pode-se testar a homogeneidade de 
### variâncias quanto à média ou a algum termo específico do modelo.

# No R - função ncvTest, pacote car.

ncvTest(ajuste1) 
### Não há evidência de que a variância dos erros varie conforme a média (p=0,608)

ncvTest(ajuste1,~'Populacao') 
### Não há evidência de que a variância dos erros varie conforme o tamanho 
### da Populacao atendida (p=0,564) 

### Falaremos sobre o gráfico do canto inferior direito mais adiante. 
### Vamos construir mais alguns gráficos.

x11()
qqPlot(ajuste1, main="QQ Plot",id.n=3) 
### Gráfico quantil-quantil dos resíduos studentizados vs quantis teóricos 
### da distribuição t-Student com envelopes simulados.
### Nesse tipo de gráfico, espera-se que os pontos, que correspondem aos resíduos, 
### estejam dispersos em torno da reta identidade, entre os limites do envelope.
### Nota-se um pequeno afastamento na cauda inferior, com um ponto externo ao envelope.

### Gráfico de resíduos versus variável adicionada no modelo.
par(cex=1.2,las=1)
plot(vendas$Populacao, r1, xlab='Populacao atendida', ylab='Resíduos studentizados', pch=20)
plot(vendas$Estudo, r1, xlab='Anos de estudo', ylab='Resíduos studentizados', pch=20)

### A ausência de padrões nos dois gráficos indica que não há a necessidade 
### de considerar a inclusão dessas variáveis ao modelo em alguma outra escala.

### Gráfico de resíduos versus variáveis não inseridas ao modelo.

ajuste12 <- update(ajuste1,~ .-Populacao) 
### Ajustando o modelo sem o efeito de Populacao.

plot(vendas$Populacao, rstudent(ajuste12),xlab='Populacao atendida',
     ylab='Resíduos studentizados',pch=20)

lines(lowess(vendas$Populacao, rstudent(ajuste12)),col='red',lwd=2)
### A relação linear entre resíduos e Populacao atendida indica a necessidade 
### da inclusão dessa variável ao modelo. Adicionalmente, indica o efeito
### linear dessa variável no total de vendas.


ajuste13=update(ajuste1,~.-Estudo) 
### Ajustando o modelo sem o efeito de estudo.
plot(vendas$Estudo,rstudent(ajuste12),xlab='Anos de estudo',
     ylab='Resíduos studentizados',pch=20)
lines(lowess(vendas$Estudo,rstudent(ajuste12)),col='red',lwd=2)
### A ausência de relação entre os resíduos e anos de estudo fornece evidencia  
### de não ser necessária a inclusão dessa variável ao modelo.

require(car)
crPlots(ajuste1) 
### Os gráficos de resíduos parciais indicam a relação entre resíduos 
### parciais e as variáveis "anos de estudo" e "Populacao atendida". Aparentemente,
### as relações são lineares, indicando a forma como as variáveis devem 
### ser inseridas no modelo.

### Agora, vamos calcular medidas de influência com o objetivo de identificar possíveis outliers.

matinf <- influence.measures(ajuste1) 
### matinf armazena os valores de diversas medidas de influência vistas 
### em aula, como DFBETAs, DFFIT, leverage. 

summary(matinf) ### Aplicando o summary, o R destaca as observações com 
### valores que ultrapassam o ponto de corte para ao menos uma das medidas.
### Repare que exceto uma das observações destacadas, todas as demais tem 
### valor acima do ponto de corte para a estatística COVRATIO.
### Repare que a observação que produz menor valor do COVRATIO é a 195 que, 
### se você lembrar, é a vendedora com maior total de vendas (volte alguns códigos atrás).

### A observação 69 é marcada como tendo elevado leverage, configurando 
### um ponto de alavancagem. Vamos ver quem é ela.

summary(vendas)
vendas[69,] 
### Repare que essa vendedora atende à região com maior Populacao. 
### Adicionalmente, é uma das vendedoras com maior idade e com menor tempo de estudo.

### Vamos verificar o impacto (conjunto) das observações 69 e 195 no modelo, 
### ajustando um novo modelo sem essas observações.

ajuste2 <- update(ajuste1, subset=-c(69,195)) 
### A função update permite o ajuste de um novo modelo, atualizando o anterior.
### Repare que a atualização corresponde a definição de um subconjunto dos dados. 

compareCoefs(ajuste1, ajuste2) 
### A função compareCoefs, do pacote car, 
### dispõe lado a lado as estimativas e erros padrões de dois modelos.
### Repare que a alteração mais substancial nos dois ajustes se refere à 
### estimativa do efeito de ajuste (de 67,8 para 52,8). No entanto, nos dois ajustes
### o efeito de anos de estudo é não significativo. Portanto, as conclusões 
### para os dois modelos são semelhantes.

### Mais um pouco de diagnóstico. 
influenceIndexPlot(ajuste1, vars=c('Studentized','Cook','Hat'), id.n=3, cex = 1.4)


### Seleção de covariáveis.

allsub <- regsubsets(Venda~., data=vendas) 
### A função regsubsets determina, para cada tamanho de modelo (número de 
### variáveis incluídas), o melhor modelo.
plot(allsub)

s1 <- summary(allsub) 
s1
### Resumo do processo. Verifique que o melhor modelo com uma variável 
### tem a variável Populacao, com duas Populacao e Tempo...

### Agora, vamos extrair algumas medidas de qualidade de ajuste desses modelos:

s1$rsq ### Coeficientes de determinação;
s1$adjr2 ### Coeficientes de determinação ajustados;
s1$cp ### CP de Mallows;
s1$bic ### Critério de informação de Schwartz. 

### Vamos observar os resultados num gráfico:

subsets(allsub, statistic='adjr2')
### Repare que os melhores modelos com duas, três e quatro variáveis têm 
### coeficientes de determinação bastante próximos. Nesse caso, a opção 
### poderia ser pelo modelo mais simples, no caso o modelo com duas variáveis 
### (Tempo de experiência e Populacao atendida).

### Agora, só para praticar, usando algoritmos do tipo stepwise.

### Usando o método backward.
selb <- step(ajuste1, direction = 'backward')
summary(selb)

### Usando o método forward
self <- step(ajuste1, direction = 'forward')
summary(self)

### Usando o método forward
selbf <- step(ajuste1, direction = 'both')
summary(selbf)

# Avalie os modelos resultantes das aplicações dos três métodos.