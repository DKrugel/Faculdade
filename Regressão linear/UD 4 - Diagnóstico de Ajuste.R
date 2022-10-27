########################################################################
### UD4- Diagnóstico do ajuste

require(faraway)
require(car)
require(corrplot)
### Carregando os pacotes necessários para a análise.

data("teengamb")
help("teengamb")
summary(teengamb)
### Dados sobre gastos de jovens em apostas.

options(device = 'x11')

pairs(teengamb, cex = 2, pch = 20)
### Matriz de gráficos de dispersão.

ajuste <- lm(gamble ~ ., data = teengamb)
summary(ajuste)
### Ajuste da regressão linear. A resposta é o gasto com apostas, e as demais
### variáveis presentes na base foram usadas como explicativas. Deixo como exercício
### a interpretação dos resultados.

########################################################################
### Vamos extrair diferentes tipos de resíduos estudados em sala de aula.

### Resíduos ordinários.
rord <- resid(ajuste) 
rord

### Resíduos padronizados.
QMRes <- summary(ajuste)$sigma^2 
rp <- resid(ajuste)/sqrt(QMRes) 
rp

### Resíduos studentizados.
rs <- rstandard(ajuste) 
rs

### Resíduos studentizados externamente.
rse <- rstudent(ajuste) 
rse

### Vamos avaliar a relação entre os diferentes tipos de resíduos extraídos.
pairs(data.frame(rord, rp, rs, rse), cex = 2, pch = 20)
cor(data.frame(rord, rp, rs, rse))


########################################################################
### Análise de resíduos.

### Gráfico de resíduos versus valores ajustados
fit <- fitted(ajuste)
res <- rstudent(ajuste)
par(cex = 1.4, las = 1)
plot(fit, res, xlab = 'Valores ajustados', ylab = 'Resíduos studentizados', pch = 20)
### Fica bastante evidente que os resíduos não apresentam variância constante.

ncvTest(ajuste)
### Teste da hipótese nula de variância constante para os resíduos.
### Temos evidência altamente significativa de variância não constante.

shapiro.test(rstudent(ajuste))
### Teste da hipótese nula de normalidade para os resíduos. A hipótese
### de normalidade é rejeitada.

### Gráfico quantil-quantil com envelope de confiança
qqPlot(ajuste, pch = 20, cex = 1.5, xlab = 'Quantis t-Student', ylab = 'Resíduos studentizados')
### Os resíduos não apresentam boa aderência à distribuição t-Student de 
### referência.

### Resíduos vs variáveis incluídas no modelo
plot(teengamb$sex, res, xlab = 'Sexo', ylab = 'Resíduos studentizados', pch = 20)
plot(teengamb$status, res, xlab = 'Status', ylab = 'Resíduos studentizados', pch = 20)
plot(teengamb$income, res, xlab = 'Income', ylab = 'Resíduos studentizados', pch = 20)
plot(teengamb$verbal, res, xlab = 'Verbal', ylab = 'Resíduos studentizados', pch = 20)
lines(lowess(res ~ teengamb$verbal), lwd = 2, col = 'red')
### A variância dos resíduos não aparenta ser constante em relação às variáveis
### sexo e income.

### Gráficos de resíduos parciais
termplot(ajuste, partial.resid = TRUE, terms = 1, pch = 20, col.res = 'black')
termplot(ajuste, partial.resid = TRUE, terms = 2, pch = 20, col.res = 'black')
termplot(ajuste, partial.resid = TRUE, terms = 3, pch = 20, col.res = 'black')
termplot(ajuste, partial.resid = TRUE, terms = 4, pch = 20, col.res = 'black')
### Os gráficos de resíduos parciais evidenciam a relação entre sexo e 
### income (linear?) com a resposta (ajustado pelo efeito das demais variáveis).

### Gráficos de diagnóstico (padrão do R)
par(mfrow = c(2,2))
plot(ajuste, which = 1:4)

### Usando recursos de diagnóstico do pacote car:
crPlots(ajuste, cex = 2, pch = 20) 
### Gráficos de resíduos parciais

residualPlots(ajuste, cex = 2, pch = 20) 
### Gráfico de resíduos versus variáveis no modelo.

marginalModelPlots(ajuste) 
### Gráfico de resíduos marginais (resíduos versus variável não ajustada).

influenceIndexPlot(ajuste, cex.lab = 1.4, cex = 1.4, las = 1, pch = 20, id = list(n=3))
### Diagnóstico de outliers, pontos de alavanca e influência.

######################################################################################
### Diagnóstico de pontos atípicos

### Vamos relevar, por um instante, as demais inadequações do ajuste e 
### nos concentrar no efeito da observação 24, indicada como outlier e
### observação influente.

teengamb[24,]
summary(teengamb)
### Trata-se do adolesente com maior valor gasto em apostas. Vamos reajustar
### o modelo desconsiderando este indivíduo.

ajuste_v2 <- update(ajuste, subset = -24)
### Agora vamos comparar estimativas e erros padrões produzidas pelos dois
### modelos.

compareCoefs(ajuste, ajuste_v2)
### Nota-se redução expressiva na estimativa referente ao efeito de sexo.
### Os erros padrões de sexo e income são reduzidos ao desconsiderar o
### dado sob investigação. De qualquer forma, as conclusões se mantêm com
### relação aos efeitos significativos e não significativos na análise.

plot(ajuste_v2, which = 1:4)
qqPlot(ajuste_v2)
### Mesmo após a exclusão dos dados do indivíduo 24 os resíduos ainda apresentam
### variância não constante. Ainda há um resíduo com valor absoluto próximo 
### a 3 que mereceria alguma atenção.

par(mfrow = c(2,2))
dfbetaPlots(ajuste, id.n = 3, cex.lab = 1.4, las = 1, pch = 20)
### Gráfico para os dfbetas. Novamente a observação 24 aparece como potencial
### ponto influente na estimativa de cada parâmetro do modelo.

help("influence.measures")
i_med <- influence.measures(ajuste)
i_med
summary(i_med)


######################################################################################
### Diagnóstico de multicolinearidade

data("seatpos")
help("seatpos")

### Nosso objetivo aqui é ajustar um modelo de regressão linear considerando 
### a variável hipcenter como resposta e as demais variáveis como explicativas.

### Como diagnóstico preliminar de multicolinearidade vamos analisar as correlações
### bivariadas.

mat_cor <- cor(seatpos)
round(mat_cor, 2)

x11()
corrplot.mixed(mat_cor, upper = 'ellipse', number.cex = 1.2)

### É possível observar correlações lineares bastante fortes entre as covariáveis,
### algumas delas maiores que 0,90. Fortíssimo indicador de multicolinearidade.

modelo <- lm(hipcenter ~ . , data = seatpos)
summary(modelo)

### Embora as correlações bivariadas entre a resposta e as explicativas sejam
### numericamente bastante elevadas, o modelo ajustado não indica significância
### para nenhuma variável explicativa. Esse resultado é contraditório, ainda,
### com o valor do R2 (0.687), que é bastante expressivo e pouco compatível
### com um modelo sem qualquer variável associada à resposta.


### Antes de prosseguir com a análise dos dados, vamos simular dados para 
### uma ilustração. Neste exemplo, vamos considerar três variáveis explicativas  
### (x1, x2 e x3, tal que x3 = x1 + x2) e a resposta (y).

x1 <- runif(100, 0, 1)
x2 <- runif(100, 0, 1)
x3 <- x1 + x2
y <- rnorm(100, mean = x1 + x2 - 0.5*x3, sd = 0.1)

pairs(data.frame(x1, x2, x3, y))

ajuste <- lm(y ~x1 + x2 + x3)
summary(ajuste)
### Observe que o R identifica a dependência linear e consequente singularidade,
### e impõe uma restrição, ao excluir a variável resultante da soma da análise.

### Retornando ao exemplo, vamos calcular os valores de VIF. Primeiro vamos calcular
### um deles na mão:

ajuste_Leg <- lm(Leg ~ Age + Weight + HtShoes + Ht + Seated + Arm + Thigh, data = seatpos)
R2 <- summary(ajuste_Leg)$r.squared
vif_Leg <- (1/(1-R2)) 
vif_Leg

vif(modelo)
### Há muita colinearidade nos dados. Se extrairmos a raiz quadrada de qualquer
### um desses valores, teremos quantas vezes o respectivo erro padrão é maior
### do que aquele que teríamos se as variáveis fossem ortogonais.

sqrt(vif(modelo))

### Para avlaiar o efeito da multicolinearidade, vamos perturbar levemente
### a variável resposta, adicionando erros aleatórios com distribuição
### Normal(0,5) a cada valor de y.

modelo2 <- lm(hipcenter + rnorm(38, 0, 5) ~ . , data = seatpos)
summary(modelo2)
summary(modelo)

### Embora o R2 dos dois modelos seja praticamente o mesmo, percebe-se uma
### boa variação nas estimativas pontuais geradas pelos dois modelos. Esse
### tipo de comportamento frente a pequenas perturbações nos dados é típico
### em casos de multicolinearidade.

### Com exceção da idade, as demais variáveis explicativas estão, em geral,
### fortemente correlacionadas. O que aconteceria se usássemos apenas uma
### delas na especificação do modelo? Vamos considerar a altura aferida
### com o indivíduo descalço.

modelo3 <- lm(hipcenter ~ Ht, data = seatpos)
summary(modelo3)

### A associação entre a altura e a variável resposta é altamente significativa. 
### Além disso, a variação no coeficiente de determinação, resultante da
### eliminação de todas as outras variáveis, foi bem pequena. Algo semelhante
### ocorreria se escolhessemos alguma outra variável ao invés de Ht. Fica
### como exercício.

modelo4 <- lm(hipcenter ~ Ht + HtShoes, data = seatpos)
summary(modelo4)

### A inclusão da variável HtShoes ao modelo faz com que os erros padrões
### aumentem consideravelmente, a tal ponto que não se tem evidência
### significativa de efeito para qualquer uma das duas variáveis.

### Vamos usar a técnica de análise de componentes principais para encontrar 
### uma (ou poucas) combinações lineares ortogonais das variáveis explicativas
### que conservem a maior parte possível da variação dos dados.

pca <- princomp(seatpos[,1:8], cor = TRUE)
### O argumento cor = T serve para executar a análise com base nos dados
### padronizados. NA ACP, isso é importante devido às diferentes escalas
### das variáveis.

summary(pca)
### O primeiro componente explica 70.9% da variação original dos dados; o
### segundo componente explica 15,4% e, somados, os dois explicam 86,3% da
### variação total.

pca$loadings
### As cargas (loadings) dos componentes correspondem aos coeficientes 
### das combinações lineares. Assim, o primeiro componente é praticamente
### uma média aritmética de todas as variáveis explicativas, com exceção
### da idade. É um componente referente ao tamanho do motorista, de forma geral.
### No segundo componente o coeficiente correspondente à idade
### é muito maior que os demais, de forma que esse componente expressa,
### de forma predominante, a idade dos motoristas.

scores <- pca$scores; scores
### Os escores são os valores dos componentes calculados para cada indivíduo.

round(cor(scores),3)
### Observe que os escores são ortogonais.

### Vamos fazer um novo ajuste do modelo de regressão agora substituindo as
### variáveis originais pelos escores dos dois componentes principais.

seatpos_novo <- data.frame(seatpos$hipcenter, scores[,1:2])
ajuste_pca <- lm(seatpos.hipcenter ~ ., data = seatpos_novo)
summary(ajuste_pca)

### Observe que ambos os componentes estão associados de forma significativa
### à posição de dirigir. Além disso, o R2 e a significância do modelo 
### ajustado são bastante próximos aos do modelo original.