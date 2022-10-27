########################################################################
### UD7 - Regressão linear com covariáveis categóricas.

require(faraway)
require(multcomp)
require(ggplot2)
options(device = 'x11')

########################################################################
### A primeira aplicação refere-se a dados de produção vegetal.

help(PlantGrowth)
head(PlantGrowth)

PlantGrowth$group <- factor(PlantGrowth$group, levels = c('ctrl', 'trt1', 'trt2'))

### Vamos analisar a produção de massa seca sob as três condições experimentais
### (controle e dois tratamentos).

### Inicialmente uma análise descritiva.
boxplot(weight ~ group, data = PlantGrowth, xlab = 'Grupo', ylab = 'Horas extras',
        las = 1)
with(PlantGrowth, by(weight, group, summary))

### Vamos ver como ficaria o ajuste se incluíssemos uma indicadora para
### cada tratamento além do intercepto.

ajuste_0 <- lm(weight ~ I(group=='ctrl') + I(group=='trt1') + I(group=='ctr2'), 
               data = PlantGrowth)
summary(ajuste_0)
model.matrix(ajuste_0)
### Da forma como foi especificado, o modelo não é identificável. Observe,
### na matriz do modelo, que a coluna de uns (intercepto) é a soma das demais
### colunas.

########################################################################
### Vamos considerar, num primeiro momento, apenas o grupo controle e o
### grupo com tratamento 1 (trt1)

PlantGrowth2 <- subset(PlantGrowth, group %in% c('ctrl', 'trt1'))
### Filtrando da base apenas as linhas referentes a esses dois tratamentos.

ajuste1 <- lm(weight ~ group, data = PlantGrowth2)
model.matrix(ajuste1)
summary(ajuste1)

### Neste caso, o intercepto corresponde à estimativa do peso médio das
### plantas do grupo controle (5.03). A segunda estimativa apresentada
### corresponde à diferença no peso médio de plantas submetidas ao
### tratamento 1 em relação a plantas do grupo controle (-0.37). 
### A estimativa para o peso médio de plantas submetidas ao tratamento 1
### é calculada por 5.03 - 0.37 = 4.66.

### Vamos analisar os dados agora usando um teste t de comparação de médias,
### assumindo variâncias iguais nos dois grupos.

t.test(weight ~ group, data = PlantGrowth2, var.equal = TRUE)
### Observe que o resultado do teste, bem como as estimativas apresentadas,
### são rigorosamente iguais às produzidas pelo modelo de regressão linear.


########################################################################
### Vamos continuar a análise agora considerando os três grupos.

ajuste2 <- lm(weight ~ group, data = PlantGrowth)
model.matrix(ajuste2)
summary(ajuste2)

### O intercepto novamente corresponde à estimativa do peso médio de plantas
### para o grupo controle. As demais estimativas correspondem às diferenças 
### nas médias para cada grupo tratado em relação ao grupo controle.

### Os resultados não apontam diferença significativa de peso, ao nível de 5%, para
### plantas submetidas ao tratamento 1 (p = 0.1944) e ao tratamento 2 (p = 0.0877)
### em relação ao grupo controle.

### Vamos usar o teste F da análise de variância para avaliar a diferença
### entre os grupos.

anova(ajuste2)
### O teste F indica diferença significativa, ao nível de significância de
### 5%, para as médias dos três grupos.

### Mas o que dizer sobre a diferença dos efeitos dos tratamentos 1 e 2?

### Podemos redefinir a categoria de referência da variável grupo:
PlantGrowth$group <- relevel(PlantGrowth$group, ref = 'trt2')

ajuste3 <- lm(weight ~ group, data = PlantGrowth)
model.matrix(ajuste3)
summary(ajuste3)

### Há diferença significativa no peso médio de plantas sob os tratamentos 1 e 2.
### Estima-se, em média, 0.8650 a menos de peso sob tratamento 1 em relação ao
### tratamento 2.

### Ao invés de redefinir o intercepto, vamos usar a função glht do pacote 
### multcomp para estimar a diferença entre os dois tratamentos.

### Vamos retomar o ajuste2, em que o grupo controle é a referência.

### Estimativa da diferença dos pesos médios sob os tratamentos 1 e 2.
L <- rbind('Trat 1 - Trat 2' = c(0, 1, -1)); L
g2 <- glht(ajuste2, linfct = L); g2
summary(g2)
confint(g2)

### E se quisermos as estimativas dos pesos médios para cada grupo?
### Para o grupo controle a estimativa é simplesmente o intercepto do 
### modelo ajustado (beta0), e o resultado está no summary. Para o tratamento
### 1 devemos estimar beta0 + beta1 e para o tratamento 2 beta0 + beta2:

L <- rbind('Trat 1' = c(1, 1, 0),
           'Trat 2' = c(1, 0, 1)); L
g2 <- glht(ajuste2, linfct = L); g2
### estimativas pontuais para cada grupo.

summary(g2)
confint(g2)
### Intervalos de confiança.

########################################################################
### Agora vamos ajustar o modelo sem o intercepto.

contrasts(PlantGrowth$group)
### Padrão do R, contrastes de tratamentos, primeiro nível do fator é tomado
### como referência.

### Remoção do intercepto:

ajuste4 <- lm(weight ~ group - 1, data = PlantGrowth)
model.matrix(ajuste4)
summary(ajuste4)
confint(ajuste4)

### Cada estimativa agora corresponde à média de um dos tratamentos.


########################################################################
### Agora, um exemplo de regressão com covariáveis quantitativas e qualitativas.
### Vamos analisar dados de reação enzimática com células tratadas ou não
### com Puromycin. Nesse caso a variável qualitativa tem dois níveis
### (tratado ou não tratado).

help("Puromycin")

ggplot(aes(x = conc, y = rate, color = state), data = Puromycin) + geom_point() +
  theme_bw(base_size = 18)

ggplot(aes(x = conc, y = rate), data = Puromycin) + geom_point() + 
  facet_grid(~state) + theme_bw(base_size = 18) +
  xlab('Concentração de substrato (ppm)') + 
  ylab('Taxa de reação (contagem/min^2)')

### A relação entre a concentração de substrato e a taxa de reação é
### claramente não linear. Vamos prosseguir a análise considerando transformação
### logarítmica para a concentração.


ggplot(aes(x = I(log2(conc)), y = rate), data = Puromycin) + geom_point() + 
  facet_grid(~state) + theme_bw(base_size = 18) +
  xlab('Concentração de substrato (ppm)') + 
  ylab('Taxa de reação (contagem/min^2)')
### Ao aplicar o logaritmo nas concentrações, a relação entre as variáveis
### nos dois grupos fica aproximadamente linear.

### Vamos ajustar quatro modelos, em ordem crescente de complexidade. Para 
### plotar os ajustes, primeiro vamos criar uma base para predição.

new_conc <- seq(min(Puromycin$conc), max(Puromycin$conc), length.out = 100)
trat <- c('treated', 'untreated')
new_data <- expand.grid(new_conc, trat)
names(new_data) <- c('conc', 'state')


### Ajuste 1 - Modelo nulo, sem efeito de concentração.
ajuste1 <- lm(rate ~ 1, data = Puromycin)

### Gráfico.
ggplot(aes(x = conc, y = rate, color = state), data = Puromycin) + geom_point() + 
  facet_grid(~state) + theme_bw(base_size = 18) +
  xlab('Concentração de substrato (ppm)') + 
  ylab('Taxa de reação (contagem/min^2)') +
  geom_line(data = new_data, 
            aes(x = conc, y=predict(ajuste1, newdata=new_data)), 
            color='black')


### Ajuste2 - Modelo com efeito de concentração, sem efeito de tratamento.
ajuste2 <- lm(rate ~ I(log2(conc)), data = Puromycin)

ggplot(aes(x = conc, y = rate, color = state), data = Puromycin) + geom_point() + 
  facet_grid(~state) + theme_bw(base_size = 18) +
  xlab('Concentração de substrato (ppm)') + 
  ylab('Taxa de reação (contagem/min^2)') +
  geom_line(data = new_data, 
            aes(x = conc, y=predict(ajuste2, newdata=new_data)), 
            color= 'black')

### Ajuste3 - Modelo com efeitos aditivos de concentração e tratamento
### (sem efeito de interação)
ajuste3 <- lm(rate ~ I(log2(conc)) + state, data = Puromycin)

ggplot(aes(x = conc, y = rate, color = state), data = Puromycin) + geom_point() + 
  facet_grid(~state) + theme_bw(base_size = 18) +
  xlab('Concentração de substrato (ppm)') + 
  ylab('Taxa de reação (contagem/min^2)') +
  geom_line(data = new_data, 
            aes(x = conc, y=predict(ajuste3, newdata=new_data)), 
            color= 'black')


### Ajuste4 - Modelo com efeito multiplicativo de concentração e tratamento
### (com efeito de interação)
ajuste4 <- lm(rate ~ I(log2(conc)) * state, data = Puromycin)

ggplot(aes(x = conc, y = rate, color = state), data = Puromycin) + geom_point() + 
  facet_grid(~state) + theme_bw(base_size = 18) +
  xlab('Concentração de substrato (ppm)') + 
  ylab('Taxa de reação (contagem/min^2)') +
  geom_line(data = new_data, 
            aes(x = conc, y=predict(ajuste4, newdata=new_data)), 
            color= 'black')

anova(ajuste1, ajuste2, ajuste3, ajuste4, test = 'F')

### De acordo com os resultados:

### A adição da (log)concentração ao modelo nulo tem efeito significativo
### (F=518.87, p < 0.001);

### A adição do tratamento ao modelo ajustado pela (log) concentração
### tem efeito significativo (F=43.258, p < 0.001);

### A adição da interação entre tratamento e (log) concentração ao modelo
### aditivo tem efeito significativo (F=11.892, p = 0.0026).

### Dessa forma, o modelo selecionado para descrever a relação entre a
### concentração de substrato e taxa de reação requer tanto interceptos
### quanto slopes distintos para cada tratamento.

summary(ajuste4)

### Gráficos de resíduos
par(mfrow = c(2,2))
plot(ajuste4)
### Acrescente seus comentários, produza resultados complementares.


########################################################################
### Regressão com covariáveis quantitativas e qualitativas.
### Dados de logevidade de moscas machos submetidas a diferentes
### condições experimentais. Consulte a documentação.

help("fruitfly")

fruitfly$activity <- factor(fruitfly$activity, levels = c('isolated','one','many','low','high'))

ggplot(aes(x = thorax, y = longevity, color = activity), data = fruitfly) + geom_point() +
  theme_bw(base_size = 18)

ggplot(aes(x = thorax, y = longevity), data = fruitfly) + geom_point() + 
  facet_grid(~activity) + theme_bw(base_size = 18) +
  xlab('Tamanho do torax') + 
  ylab('Tempo de vida (em dias)')

### Vamos ajustar quatro modelos, em ordem crescente de complexidade.

### Ajuste 1 - Modelo nulo, sem efeito de tratamento.
ajuste1 <- lm(longevity ~ 1, data = fruitfly)

### Ajuste2 - Modelo com efeito de thorax, sem efeito de tratamento.
ajuste2 <- lm(longevity ~ thorax, data = fruitfly)

### Ajuste3 - Modelo com efeitos aditivos de thorax e tratamento
### (sem interação)
ajuste3 <- lm(longevity ~ thorax + activity, data = fruitfly)

### Ajuste4 - Modelo com efeito multiplicativo de thorax e tratamento
### (com interação)
ajuste4 <- lm(longevity ~ thorax * activity, data = fruitfly)

anova(ajuste1, ajuste2, ajuste3, ajuste4, test = 'F')

### De acordo com os resultados:

### A adição do tamanho do torax ao modelo nulo tem efeito significativo
### (F=130.733, p < 0.001);

### A adição do tratamento ao modelo ajustado pelo tamanho do torax
### tem efeito significativo (F=20.988, p < 0.001);

### A adição da interação entre tamanho do torax e tratamento ao modelo
### aditivo não tem efeito significativo (F=0.053, p = 0.9947).

### Dessa forma, o modelo selecionado para descrever a relação entre a
### concentração de substrato e taxa de reação requer um intercepto para
### cada tratamento, mas inclinação comum (um único slope).

summary(ajuste3)

### Gráficos de resíduos
par(mfrow = c(2,2))
plot(ajuste3)
### Os resíduos visivelmente não tem variância constante. Fica como tarefa
### encontrar uma transformação que contorne isso. Como o objetivo aqui é
### simplesmente obter as restas ajustadas (sem procupação quanto aos erros
### ou outras inferências, prosseguimos com o modelo original.)

### Vamos plotar os ajustes. Para isso, primeiro vamos criar uma base para
### predição.
new_thorax <- seq(min(fruitfly$thorax), max(fruitfly$thorax), length.out = 100)
trat <- c('isolated', 'one', 'low', 'many', 'high')
new_data <- expand.grid(new_thorax, trat)
names(new_data) <- c('thorax', 'activity')

### Agora o gráfico.
ggplot(aes(x = thorax, y = longevity), data = fruitfly) + geom_point() + 
  facet_grid(~activity) + theme_bw(base_size = 18) +
  xlab('Comprimento do torax') + 
  ylab('Longevidade') +
  geom_smooth(method='lm')

fruitfly$activity2 <- fruitfly$activity 
levels(fruitfly$activity2) <- c('iso or preg', 'iso or preg','iso or preg', 'virg1', 'virg2')
ajuste3_alt <- lm(longevity ~ thorax + activity2, data = fruitfly)
summary(ajuste3_alt)
anova(ajuste3_alt, ajuste3)

### Vamos testar agora se uma única reta de regressão é suficiente para 
### explicar a relação entre longevidade e tamanho do torax para controle
### para o grupo isolado e para os dois grupos expostos a fêmeas grávidas.

fruitfly$activity2 <- fruitfly$activity 
levels(fruitfly$activity2) <- c('iso or preg', 'iso or preg', 'iso or preg', 'low', 'high')
### O fator activity2 tem três níveis: 'iso or preg' para animais do grupo 
### controle e submetidos a fêmeas grávidas; 'low' e 'high' para os outros
### dois grupos.

ajuste3_grup <- lm(longevity ~ thorax + activity2, data = fruitfly)
### Modelo de regressão ajustado para o tratamento com três níveis.

### Vamos comparar os ajustes dos modelos considerando o tratamento com cinco
### e com três níveis. 
summary(ajuste3_grup)
anova(ajuste3_grup, ajuste3)
### Observe que o acréscimo na soma de quadrados resultante da redução de
### cinco para três níveis do tratamento é não significativa (p=0.3848).
### Assim, podemos considerar o modelo aditivo com uma única reta de 
### regressão para os grupos 'isolated', 'one' e 'many'.

### Vamos plotar os resultados do ajuste, como feito anteriormente.
new_thorax <- seq(min(fruitfly$thorax), max(fruitfly$thorax), length.out = 100)
trat <- c('virg1', 'iso or preg', 'virg2')
new_data <- expand.grid(new_thorax, trat)
names(new_data) <- c('thorax', 'activity2')

ggplot(aes(x = thorax, y = longevity), data = fruitfly) + geom_point() + 
  facet_grid(~activity2) + theme_bw(base_size = 18) +
  xlab('Comprimento do torax') + 
  ylab('Longevidade') +
  geom_smooth(method='lm')