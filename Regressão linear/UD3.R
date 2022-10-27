########################################################################
### UD3- Regressão linear múltipla

### Parte 1 (Regressão linear múltipla com duas covariáveis)

options(device = 'x11')

### Carregando os pacotes necessários para a análise.
require(car)
require(scatterplot3d)
require(emmeans)

### Exemplo- dados sobre escolaridade, renda e prestígio de 45 profissões (EUA-1950).
help("Duncan") ### Consultando a documentação da base.
head(Duncan)
summary(Duncan)

### O objetivo é ajustar um modelo de regressão para prestigio em função 
### de escolaridade e renda.

scatterplotMatrix( ~ prestige + income + education, smooth = FALSE, 
                   data = Duncan, pch = 20, lwd = 2, cex = 1.5)
### Os gráficos indicam que escolaridade e renda estão (linearmente) 
### relacionados ao prestígio da profissão. Em ambos os casos a tendência
### é positiva (maior renda, maior prestigio; maior escolaridade, maior prestigio)

scatterplot3d(Duncan$education, Duncan$income, Duncan$prestige,
              xlab = 'Education', ylab = 'Income', zlab = 'Prestige',
              highlight.3d =TRUE, col.axis="blue",
              col.grid = "lightblue",  pch = 20,type = 'h')
### Gráfico de dispersão 3D. Mais adiante, vamos adicionar ao gráfico a 
### regressão ajustada.

ajuste <- lm(prestige ~ income + education, data = Duncan) 
### Ajuste do modelo de regressão linear múltipla para o prestigio da 
### profissão em função de escolaridade e renda.

print(ajuste) ### Estimativas de mínimos quadrados para os parâmetros do modelo.

summary(ajuste) 
### Repare que os coeficientes associados a renda e educação são positivos, 
### indicando que o prestigio aumenta quanto maiores os percentuais de 
### "alta renda" e "alta escolaridade" da profissão.

### Algumas interpretações:
### O intercepto não tem uma interpretação prática, pois percentuais iguais 
### a zero para escolaridade e renda não fazem parte do escopo do estudo.

### Estima-se, em média, um aumento de 0,598 no grau (percentual) de prestígio 
### da profissão para 1% a mais de profissionais com ensino médio, mantendo 
### a renda fixa.

### Estima-se, em média, um aumento de 0,545 no grau (percentual) de prestígio 
### da profissão para 1% a mais de profissionais que recebem mais de $3500,  
### mantendo a escolaridade fixa.

fitted(ajuste) ### Prestigio ajustado pelo modelo para as 45 profissões.
plot(Duncan$prestige, fitted(ajuste), xlab='Prestígio observado', ylab='Prestígio ajustado', pch = 20)
### Gráfico dos valores observados versus valores ajustados pelo modelo.

resid(ajuste) ### Resíduos produzidos pelo modelo ajustado.
plot(fitted(ajuste), resid(ajuste), xlab = 'Valores ajustados', ylab = 'Resíduos', pch = 20)
### Gráfico de resíduos vs valores ajustados.

prestige3d <- scatterplot3d(Duncan$education, Duncan$income, Duncan$prestige,
                            xlab = 'Education', ylab = 'Income', zlab = 'Prestige',
                            highlight.3d =TRUE, col.axis="blue",
                            col.grid = "lightblue",  pch = 20,type = 'h')
prestige3d$plane3d(ajuste, lty.box = "solid")
### Adicionando ao gráfico de dispersão a regressão ajustada.

### E se ajustarmos uma regressão linear simples para cada variável explicativa?

ajuste_srs1 <- lm(prestige ~ income, data = Duncan)
summary(ajuste_srs1)

ajuste_srs2 <- lm(prestige ~ education, data = Duncan)
summary(ajuste_srs2)

### Observem que, embora as conclusões sejam semelhantes, as estimativas
### são bem diferentes. No modelo de regressão linear múltipla estamos
### avaliando o efeito da educação no prestígio controlando o efeito
### de renda (o mesmo vale para o efeito de renda). Nos modelos de regressão
### linear simples estamos avaliando o efeito de cada variáveil explicativa
### não controlando o efeito da segunda variável. Como renda e escolaridade
### estão correlacionadas, o efeito das variáveis muda se ajustarmos ou não
### o efeito da outra variável.

vcov(ajuste) 
### Matriz de variâncias e covariâncias estimada para os estimadores dos betas.

confint(ajuste) ### Intervalos de confiança (95%) para os parâmetros do modelo.
predict(ajuste, newdata = data.frame(income = 50,education = 50), interval = 'confidence') 
### Intervalo de confiança (95%) para o prestigio esperado de profissões para as quais
### 50% dos profissionais tem ensino médio e 50% tem renda superior a $3500.

predict(ajuste,newdata=data.frame(income=50,education=50),interval='prediction') 
### Intervalo de predição (95%) para o grau de prestigio de uma "nova"  
### profissão para a qual 50% dos profissionais tem ensino médio e 50% 
### tem renda superior a $3500.


### Agora, um exemplo envolvendo dados simulados.

### Vamos simular dados para duas variáveis explicativas correlacionadas,
### e a resposta como função dessas duas variáveis.

set.seed(98) ### Fixando a semente para fins de reprodução dos resultados.

x1 <- runif(100) ### Simulação de 100 valores para x1~Unif(0,1)
x2 <- x1 + rnorm(100, 0, 1.5) ### x2 = x1 + e2, e2 ~ Normal(0, 1.5^2)
erro <- rnorm(100, 0, 0.05) ### epsilon ~ Normal(0, 0.05^2) (erro)
y <- 0.1*x1 + 0.005*x2 + erro ### Valores simulados para y.

ajuste_rlm <- lm(y ~ x1 + x2)
summary(ajuste_rlm)
### Observe que x2 não apresenta efeito significativo ao ajustarmos y também  
### por x1.

ajuste_rls <- lm(y ~ x2)
summary(ajuste_rls)
### Quando não ajustamos o efeito de x1, x2 passa a apresentar efeito significativo
### no modelo.

################################################################################
### Sessão R - Parte 2 (Regressão linear múltipla)

options(device = 'x11')

### Carregando os pacotes necessários para a análise.
require(car)
require(corrplot)
require(effects)
require(ggplot2)

### Exemplo- Regressão linear múltipla aplicada à modelagem do preço de venda de
### imóveis. As variáveis são as seguintes:

### area: Área do imóvel (m^2);
### idade: Idade do imóvel (anos);
### distancia: Distância do imóvel ao marco central do município (em km);
### ncomodos: Número de cômodos;
### pcomerc: Número de pontos comerciais num raio de um quilômetro;
### valor: Preço de venda do imóvel (variável resposta, em milhares de dólares).

dados <- read.csv2('https://docs.ufpr.br/~taconeli/CE07118/Imoveis18.csv') ### Importando os dados.
head(dados,10)

summary(dados) 

par(cex=1.2)
scatterplotMatrix(dados) 
### Matriz de gráficos de dispersão. Os resultados fornecem um primeiro indicativo 
### de correlação positiva entre o preço de venda e a área e o preço de venda 
### e o número de cômodos do imóvel. Também há alguma evidência de correlação negativa 
### em relação à idade do imóvel.

corrplot.mixed(cor(dados), upper = "ellipse", lower.col = 'black')
### Matriz de correlações.

### Vamos ajustar o modelo de regressão linear múltipla:
ajuste1 <- lm(valor ~ area + idade + distancia + ncomodos + pcomerc, data = dados)

### O mesmo modelo pode ser ajustado, de forma mais breve, através do seguinte 
### comando:
ajuste1 <- lm(valor ~ ., data = dados)

### Vamos extrair os principais resultados da análise.

model.matrix(ajuste1)[1:10,]
### Dez primeiras linhas da matriz do modelo.

print(ajuste1) 
### Estimativas de mínimos quadrados dos parâmetros de regressão.

summary(ajuste1)
### Os resultados do ajuste indicam que o valor de venda está associado à área,
### número de cômodos e idade do imóvel.

### Estima-se, em média, um aumento de 1496 dólares para cada m^2 a mais de área;
### uma redução de 713 dólares a cada ano a mais de idade e redução de 917 dólares
### a cada cômodo a mais. Em cada uma dessas afirmativas estamos considerando fixos
### os valores das demais variáveis explicativas.

### Vamos ajustar modelos de regressão linear simples para cada variável
### explicativa individualmente e comparar os resultados.

ajuste_area <- lm(valor ~ area, data = dados)
summary(ajuste_area)

ajuste_idade <- lm(valor ~ idade, data = dados)
summary(ajuste_idade)

ajuste_distancia <- lm(valor ~ distancia, data = dados)
summary(ajuste_distancia)

ajuste_ncomodos <- lm(valor ~ ncomodos, data = dados)
summary(ajuste_ncomodos)

ajuste_pcomerc <- lm(valor ~ pcomerc, data = dados)
summary(ajuste_pcomerc)

### Observe o efeito do número de cômodos no valor de venda do imóvel no 
### modelo de regressão linear múltipla e no modelo de regressão linear
### simples. O que ocorre? Você tem uma justificativa? Conversamos na
### sala de aula.

fitted(ajuste1) ### Valores ajustados pelo modelo de regressão linear múltipla.
residuals(ajuste1) ### Resíduos.

s2 <- summary(ajuste1)$sigma^2; s2
### Estimativa de sigma^2 (quadrado médio de resíduos).

summary(ajuste1)$r.squared ### Coeficiente de determinação.
### Aproximadamente 60% da variação dos dados é explicada pelo modelo.

summary(ajuste1)$adj.r.squared
### Coeficiente de determinação ajustado.

plot(fitted(ajuste1), dados$valor, pch = 20,xlab = 'Vendas ajustadas', 
     ylab = 'Vendas observadas', las = 1) 
### Gráfico de valores observados vs valores ajustados.

confint(ajuste1) 
### Intervalos de confiança (95%) para os parâmetros do modelo.

### Agora vamos fazer algumas predições.

dnovos <- data.frame(area = c(80, 100, 150), idade = c(5, 10, 10), 
                     distancia = c(15, 12, 10), ncomodos = c(5, 6, 8),
                     pcomerc = c(10, 10, 10)) 
dnovos

p1 <- predict(ajuste1, newdata = dnovos) 
### Estimativas pontuais para os preços de venda para os novos dados. 

### Antes de proceder com a obtenção de intervalos de confiança, vamos abordar
### alguns outros problemas de estimação:

### Qual a probabilidade de um imóvel a venda com preço inferior a 150 mil, 
### considerando imóveis semelhantes aos fornecidos para predição?

prob1 <- pnorm(150, mean = p1, sd = sqrt(s2), lower.tail = TRUE)
round(prob1, 6)

### Qual a probabilidade de um imóvel a venda com preço superior a 250 mil, 
### considerando imóveis semelhantes aos fornecidos para predição?

prob2 <- pnorm(250, mean = p1, sd = sqrt(s2), lower.tail = FALSE)
round(prob2, 6)

### Qual o valor que delimita 25% dos imóveis mais baratos (quantil de ordem 0.25), 
### considerando imóveis semelhantes aos fornecidos para predição?

q025 <- qnorm(0.25, mean = p1, sd = sqrt(s2))
q025

### Qual o valor que delimita 10% dos imóveis mais caros (quantil de ordem 0.90), 
### considerando imóveis semelhantes aos fornecidos para predição?

q090 <- qnorm(0.9, mean = p1, sd = sqrt(s2))
q090


### Agora, os intervalos de confiança para o preço médio de venda e para a predição.

predict(ajuste1, interval = 'confidence', newdata = dnovos)
### Intervalos de confiança (95%) para os preços médios de venda de imóveis com 
### os três "perfis".

predict(ajuste1, interval = 'prediction', newdata = dnovos) 
### Intervalos de predição (95%) para o valor de venda de três novos imóveis com
### as características especificadas.

p1 <- predict(ajuste1, interval = 'confidence', newdata = dados)
### Intervalos de confiança para os preços médios de venda de imóveis com os perfis
### dos imóveis da base.
cbind(dados[1:20,1:5], p1[1:20,])

p2 <- predict(ajuste1, interval = 'prediction', newdata = dados)
### Intervalos de predição para os preços de venda de novos imóveis com os perfis
### dos imóveis da base.
cbind(dados[1:20,1:5], p2[1:20,])

ajuste0 <- lm(valor ~ 1, data = dados) ### Modelo nulo (apenas com intercepto)
anova(ajuste0, ajuste1) ### Teste da significância do modelo

summary(ajuste0) 
### Observe que os resultados para o teste da significância do modelo são apresentados
### na linha final do summary.

### Agora vamos testar a hipótese nula H_0: beta_distancia = beta_pcomerc = 0,
### ou seja, a hipotese de não efeito da distancia e do numero de pontos comerciais,
### conjuntamente, no valor de venda do imovel.

ajuste3 <- lm(valor ~. -distancia - pcomerc, data = dados)

### Teste da razão de verossimilhanças (baseado na distribuição qui-quadrado)
anova(ajuste3, ajuste1, test = 'Chisq')

### Na mão
est_teste <- -2*log((262520/262817.7)^250)
pchisq(est_teste, df = 2, lower.tail = FALSE)

### Agora usando o teste F
anova(ajuste3, ajuste1, test = 'F')

### Na mão
est_teste <- (297.67/2)/(262520/494)
pf(est_teste, df1 = 2, df2 = 494, lower.tail = FALSE)

### Resumo do modelo sem as duas variáveis.
summary(ajuste3)
### Não há evidência significativa contrária à hipótese nula (p = 0.7558). 


### Agora, alguns testes de hipóteses. Começamos pelo quadro da anova.
anova(ajuste1)

### O que podemos concluir:

### Efeito significativo da inclusão de area (conforme a redução na soma de 
### quadrados de resíduos) ao modelo nulo (p < 0.0001);

### Efeito significativo da inclusão de idade (conforme a redução na soma de 
### quadrados de resíduos) ao modelo ajustado pelo efeito de area (p = 0.0055);

### Efeito não significativo da inclusão de distancia (conforme a redução na soma de 
### quadrados de resíduos) ao modelo ajustado pelos efeitos de area e idade
### (p = 0.6358);

### Efeito significativo da inclusão de ncomodos (conforme a redução na soma de 
### quadrados de resíduos) ao modelo ajustado pelos efeitos de area, idade e distancia
### (p = 0.0444);

### Efeito não significativo da inclusão de pcomerc (conforme a redução na soma de 
### quadrados de resíduos) ao modelo ajustado pelos efeitos de area, idade, distancia
### e ncomodos (p = 0.5856).

### E se mudarmos a ordem de declaração das variáveis?

ajuste1_alt <- lm(valor ~ pcomerc + ncomodos + area + idade + distancia, data = dados)
anova(ajuste1_alt)

### Os resultados são idênticos? Justifique.


### Usando a função Anova do pacote car, temos uma sequência de testes diferentes:

Anova(ajuste1)

### O que podemos concluir (ao nível de 5% de significância):

### Efeito significativo da inclusão de area (conforme a redução na soma de 
### quadrados de resíduos) ao modelo ajustado por todas as demais variáveis (p < 0.0001);

### Efeito significativo da inclusão de idade (conforme a redução na soma de 
### quadrados de resíduos) ao modelo ajustado por todas as demais variáveis (p = 0.0042);

### Efeito não significativo da inclusão de distancia (conforme a redução na soma de 
### quadrados de resíduos) ao modelo ajustado por todas as demais variáveis (p = 0.64817);

### Efeito significativo da inclusão de ncomodos (conforme a redução na soma de 
### quadrados de resíduos) ao modelo ajustado por todas as demais variáveis (p = 0.0439);

### Efeito não significativo da inclusão de pcomerc (conforme a redução na soma de 
### quadrados de resíduos) ao modelo ajustado por todas as demais variáveis (p = 0.5856).

### E se mudarmos a ordem de declaração das variáveis?

ajuste1_alt <- lm(valor ~ pcomerc + ncomodos + area + idade + distancia , data = dados)
Anova(ajuste1_alt)

### Os resultados são idênticos? Justifique.

### Observe que, no segundo caso os testes F são equivalentes ao teste t apresentados 
### no summary. Isso não é coincidência, e sempre ocorrerá quando houver apenas 
### um parâmetro do modelo avaliado sob cada teste.

### Vamos testar o efeito parcial da idade do imóvel. Esse teste já está apresentado
### na saída da função Anova, e é equivalente ao teste apresentado no summary.
### Mas usando essa forma de proceder o teste teremos condições de realizar testes
### envolvendo, conjuntamente, dois ou mais parâmetros.

ajuste2 <- lm(valor ~. -idade, data = dados) 
### Modelo de regressão linear múltipla sem considerar a covariável idade.

anova(ajuste2, ajuste1)
### O acréscimo na soma de quadrados (ASQ) de resíduos, resultante da eliminação do 
### termo idade do modelo, é igual a 4392. A estatística do teste é dada por
### F = (ASQ/df)/QMRes = 4392/531 = 8.26. Sob a hipótese nula, a estatística F
### tem distribuição F-Snedecor com parâmetros 1 (associado ao número de parãmetros)
### fixados pela hipótese nula) e 494 (associado à soma de quadrados de resíduos).
### Vamos ver quais seriam as conclusões aos níveis de 5 e 1% de significância:

qf(0.95, df1 = 1, df2 = 494)
### Como F = 8.26 > 3.86, podemos rejeitar a hipótese nula e comprovar o efeito
### da idade do imóvel ao nível de significância de 5%;

qf(0.99, df1 = 1, df2 = 494)
### Como F = 8.26 > 6.68, podemos rejeitar a hipótese nula e comprovar o efeito
### da idade do imóvel ao nível de significância de 5%;

pf(8.26, df1 = 1, df2 = 494, lower.tail = FALSE)
### Valor p do teste (ligeiramente diferente por erros de arredondamento).

### Teste da significância do modelo de regressão
ajuste0 <- lm(valor ~ 1, data = dados)
summary(ajuste0)
### Modelo nulo.

anova(ajuste0, ajuste1, test = 'F')
summary(ajuste1)
### Vale a pena reforçar que o teste da significância do modelo de regressão
### já aparece no rodapé do summary.

### Região de confiança para area e idade
plot(ellipse::ellipse(ajuste1, which = c('area','idade'), level = 0.95), type = 'l')

### Voltando ao modelo 1, a elipse de confiança para os parâmetros de área e ncomodos:
plot(ellipse::ellipse(ajuste1, which = c('area','ncomodos'), level = 0.95), type = 'l')

### Vamos calcular também os intervalos de confiança 95%:
confint(ajuste1)
abline(v = confint(ajuste1)['area',], lty = 2, col = 'blue')
abline(h = confint(ajuste1)['ncomodos',], lty = 2, col = 'blue')

### Agora, intervalos de confiança com a correção de Bonferroni (confiança
### conjunta de 95%)

confint(ajuste1, level = 0.975)
abline(v = confint(ajuste1, level = 0.975)['area',], lty = 2, col = 'red')
abline(h = confint(ajuste1, level = 0.975)['ncomodos',], lty = 2, col = 'red')

### Observe que há pontos (valores dos parâmetros) que pertencem à elipse de confiança
### mas não à região retangular delimitada pelos intervalos de confiança e vice-versa.

### Explorando os efeitos das variáveis.
plot(allEffects(ajuste3))
### Gráficos de efeitos para cada variável. No eixo vertical temos a resposta esperada
### (preço médio de venda). Em cada gráfico as demais variáveis são fixadas em suas
### médias. Além da estimativa pontual (representada pela reta sólida), também são
### apresentadas as bandas de confiança (95%).

### Vamos ajustar um novo modelo, agora considerando as variáveis explicativas
### centradas, isso é, subtraindo de cada variável a respectiva média na
### amostra.

dados$areac <- dados$area - mean(dados$area)
dados$ncomodosc <- dados$ncomodos - mean(dados$ncomodos)
dados$idadec <- dados$idade - mean(dados$idade)

ajuste3c <- lm(valor ~ areac + ncomodosc + idadec, data = dados)
summary(ajuste3c)
summary(ajuste3)

### Observe que centrar os dados não tem qualquer efeito sobre as estimativas,
### erros padrões e significâncias dos betas (exceto para o intercepto). 
### Isso se repetiria para qualquer outra constante que fosse adicionada ou subtraída 
### aos valores de cada variável.

### Adicionalmente:

### 1- O intercepto agora tem uma interpretação válida. Podemos interpretá-lo
### como a estimativa do preço médio de venda para imóveis com características
### "médias", ou seja, com valores para as covariáveis definidas pelas médias
### amostrais. Como o valor ajustado de y avaliado nas médias de x1, x2, ...,
### xp é a média amostral de y, então a estimativa do intercepto será
### simplesmente ybarra.

### 2- O intercepto, agora, é ortogonal aos demais parâmetros do modelo.
### Para verificar isso, nesta aplicação, basta extrair as matrizes de covariâncias
### dos estimadores produzidas pelos dois modelos.

vcov(ajuste3)
vcov(ajuste3c)

### Agora vamos ajustar um novo modelo em que, além de centradas, as variáveis
### explicativas estão escalonadas. Ou seja, para uma variável x qualquer,
### consideramos x' = (x - media(x))/desv.padrão(x).

dados$areas <- (dados$area - mean(dados$area))/sd(dados$area)
dados$ncomodoss <- (dados$ncomodos - mean(dados$ncomodos))/sd(dados$ncomodos)
dados$idades <- (dados$idade - mean(dados$idade))/sd(dados$idade)
### Alternativamente poderíamos usar a função scale()

ajuste3s <- lm(valor ~ areas + ncomodoss + idades, data = dados)
summary(ajuste3s)
summary(ajuste3c)
summary(ajuste3)

### Observe que agora, embora as significâncias das variáveis sejam as
### mesmas, as estimativas e os erros padrões mudaram, devido à mudança
### de escala. Neste modelo as interpretações dos parâmetros não são
### mais em relação ao "acréscimo de uma unidade em x", mas sim quanto ao
### "acréscimo de um desvio padrão de x".

### Como resultado de eliminar o efeito de escala, os valores das estimativas
### (suas grandezas) são diretamente comparáveis. Assim, fica evidente o
### maior efeito da área do imóvel no preço de venda. Nos modelos anteriores
### os valores das estimativas dependiam da escala (e da unidade de medida)
### das respectivas variáveis. Aqui isso já não acontece.

### A interpretação do intercepto é a mesma do ajuste anterior.

estim <- data.frame(coef(ajuste3s), confint(ajuste3s))[-1,]
### Data frame com as estimativas pontuais e intervalos de confiança 95%
### para os betas (exceto o intercepto).

names(estim) <- c('Estimativa', 'LI', 'LS')

p <- ggplot(aes(y = Estimativa, ymin = LI, ymax = LS, x = rownames(estim)),
            data = estim) + geom_pointrange()

p + coord_flip() + xlab("Variável") + geom_hline(yintercept = 0)

################################################################################
### Sessão R - Parte 3 (Ortogonalidade)

require(faraway)
data(odor)
help("odor")

### As variáveis explicativas foram transformadas de suas escalas originais.
### Para a temperatura (em ºF), por exemplo, os valores originais eram 40,
### 80 e 120, e a transformação aplicada foi (Temp-80)/40.

cov(odor[,-1])
### As covariâncias nulas indicam ortogonalidade entre as variáveis.

### Vamos analisar o efeito da temperatura no odor do produto em três momentos:

ajuste1 <- lm(odor ~ temp, data = odor)
summary(ajuste1)
### Efeito de temperatura não ajustado por gas ou pack;

ajuste2 <- lm(odor ~ temp + gas, data = odor)
summary(ajuste2)
### Efeito de temperatura ajustado apenas por gas;

ajuste3 <- lm(odor ~ temp + gas + pack, data = odor)
summary(ajuste3)
### Efeito de temperatura ajustado por gas e pack.

### Observe que a estimativa do efeito de temperatura não muda. Os erros padrões
### são ligeiramente diferentes, devido à alteração nos graus de liberdade,
### mas isso não implica em mudanças substanciais nas inferências.

################################################################################
### Sessão R - Parte 4 (Inferência conjunta e para combinações lineares dos parâmetros)

require(multcomp)
setwd("C:\\Users\\cetac\\Dropbox\\Backup Parana\\Parana\\Disciplinas\\Regressão Linear\\Oferta_2018\\Codigos")
options(device = 'x11')

### Notas de alunos de uma faculdade em quatro exames do processo seletivo
### (V1, V2, V3 e V4) e a nota final no exame de conhecimentos gerais ao
### término do primeiro semestre (y). Os dados estão disponíveis na página
### da disciplina.

notas <- read.csv2('Notas.csv')
plot(notas, col="navy", pch = 20, cex = 1.4)

### Ajuste do modelo de regressão linear múltipla.
ajuste <- lm(y ~ ., data = notas)

summary(ajuste)

### Vamos testar a hipótese H0: beta2 = beta4 E beta3 = 2*beta2.
### Podemos escrever as duas conjecturas que compõem H0, de maneira equivalente,
### por beta2 - beta4 = 0 e -2*beta2 + beta3 = 0. Vamos declarar os vetores
### contendo os coeficientes dessas combinações lineares.

L <- matrix(c(0,0,1,0,-1,0,0,-2,1,0), nrow = 2, byrow = TRUE)
L
c0 <- matrix(c(0,0), nrow = 2)
c0

### Os comandos a seguir servem apenas para ilustrar as contas feitas "na mão".
q <- 2 ### Número de restrições definidas em H0
X <- model.matrix(ajuste); X ### matiz do modelo
beta <- coef(ajuste); beta ### vetor de estimativas
sigma2 <- summary(ajuste)$sigma^2; sigma2 ### estimativa da variância
n <- nrow(X); n
p <- ncol(X); p

### Cálculo da estatística F:
F_calc <- (t(L %*% beta - c0) %*% solve(L %*% solve(t(X) %*% X) %*% t(L)) %*% (L %*% beta - c0))/(q*sigma2)
F_calc

### Valor da tabela F de referência, considerando 5% de significância:
qf(0.95, df1 = q, df2 = n-p)

### Logo, não se tem evidências ao nível de 5% para rejeitar o par de contrastes 
### definidas em H0.

pf(F_calc, df1 = q, df2 = n-p, lower.tail = FALSE)
### Valor p do teste.

### Agora vamos obter as estimativas pontuais e IC's 95% para cada um dos contrastes.

### Para beta2 - beta4:
l <- matrix(L[1,], ncol = 1)
t(l) %*%beta ### Estimativa pontual.
c(t(l) %*%beta + qt(0.025, n-p) * sqrt(sigma2 * t(l) %*% solve(t(X) %*% X) %*% l),
  t(l) %*%beta - qt(0.025, n-p) * sqrt(sigma2 * t(l) %*% solve(t(X) %*% X) %*% l))

### Para beta3 - 2*beta2:
l <- matrix(L[2,], ncol = 1)
t(l) %*%beta ### Estimativa pontual.
c(t(l) %*%beta + qt(0.025, n-p) * sqrt(sigma2 * t(l) %*% solve(t(X) %*% X) %*% l),
  t(l) %*%beta - qt(0.025, n-p) * sqrt(sigma2 * t(l) %*% solve(t(X) %*% X) %*% l))


### Na sequência, vamos explorar os recursos da função glht do pacote multcomp 
### para inferência sobre combinações lineares dos psrâmetros.

help(glht)

L <- rbind('V2 - V4' = c(0, 0, 1, 0, -1),
           '-2*V2 + V3' = c(0, 0, -2, 1, 0))
### Especificando os contrastes.

g1 <- glht(ajuste, linfct = L)
g1
### Estimativas pontuais.

summary(g1, test = Ftest())
### Testando a hipótese nula para o par de contrstes.

summary(g1, adjusted(type = "none"))
### Estimativas, erros padrões e testes de hipóteses para cada um dos dois
### contrastes. 

summary(g1)
### Semelhante à saída anterior, mas neste caso os p-valores são corrigidos
### devido aos múltiplos testes (dois contrastes avaliados simultaneamente).

confint(g1, calpha = univariate_calpha())
### Intervalos de confiança não corrigidos (cada um com 95% de confiança)

confint(g1)
### Intervalos de confiança corrigidos (95% de confiança para o par de intervalos).

### Vamos usar a função glht para produzir inferências para outras combinações
### lineares dos parâmetros do modelo.

### H0: beta1 = 0.
L <- rbind('V1' = c(0, 1, 0, 0, 0)); L
g2 <- glht(ajuste, linfct = L); g2
summary(g2)

### H0: beta2 = 0.
L <- rbind('V2' = c(0, 0, 1, 0, 0)); L
g3 <- glht(ajuste, linfct = L); g3
summary(g3)

### H0: beta1 = beta2 = beta3 = beta4 = 0.
L <- rbind('V1' = c(0, 1, 0, 0, 0),
           'V2' = c(0, 0, 1, 0, 0),
           'V3' = c(0, 0, 0, 1, 0),
           'V4' = c(0, 0, 0, 0, 1)); L
g4 <- glht(ajuste, linfct = L); g4
summary(g4, test = Ftest())
summary(g4, adjusted(type = "none"))
### Observe que, neste caso, estamos testando a significância da regressão.


### H0: beta2 - (1/2) * (beta1 + beta4) = 0
### As notas nos exames V1 e V4 contribuem, em média, igual a V2
### na nota média em CG.
L <- rbind('cte' = c(0, -0.5, 1, 0, -0.5)); L
g5 <- glht(ajuste, linfct = L); g5
summary(g5)

### Agora, vamos definir quatro contrastes distintos a serem testados:
### c1: beta2 = beta3 (beta2 - beta3 = 0);
### c2: beta3 = 2*beta1 (beta3 - 2*beta1 = 0);
### c3: beta4 = (1/3) * (beta1 + beta2 + beta3) (beta4 - (1/3) * (beta1 + beta2 + beta3) = 0);
### c4: beta1 = 0.

L <- rbind('c1' = c(0, 0, 1, -1, 0),
           'c2' = c(0, -2, 0, 1, 0),
           'c3' = c(0, -1/3, -1/3, -1/3, 1),
           'c4' = c(0, 1, 0, 0, 0)); L
g6 <- glht(ajuste, linfct = L); g6
summary(g6, adjusted(type = "none"))
### Resultados não ajustados.
summary(g6, test = Ftest())
summary(g6)
### P-valores ajustados pelos fato de estarmos testando conjuntamente quatro contrastes.

confint(g6, calpha = univariate_calpha()) ### IC's 95% (individualmente)
confint(g6) ### IC's 95% (no conjunto).

### Para testar a hipótese H0: beta1 = beta2 = beta4:
L <- rbind('V1 - V2' = c(0, 1, -1, 0, 0),
           'V1 - V4' = c(0, 1, 0, 0, -1),
           'V2 - V4' = c(0, 0, 1, 0, -1)); L
g7 <- glht(ajuste, linfct = L); g7
summary(g7, test = Ftest())