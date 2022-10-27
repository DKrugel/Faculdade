################################################################################
### UD2- Regressão linear simples

options(device = 'x11')

library(MASS) 
library(ggplot2)
data(cats) ### Carregando a base.
help(cats) ### Acessando a documentação da base.

### O objetivo é ajustar um modelo de regressão para o peso do coração  
### (Hwt, em gramas) em função do peso corporal (Bwt, em kg). 
### Num primeiro momento vamos desconsiderar o sexo dos animais, mas este 
### problema pode ser revisitado mais adiante, incluindo essa variável.

summary(cats)

### Gráfico de dispersão
g1 <- ggplot(data = cats, aes(x = Bwt, y = Hwt))+
  geom_point(size = 2, col = 'grey50')+
  theme_bw(base_size = 14)+
  xlab('Peso corporal (kg)')+
  ylab('Peso do coração (g)')
g1

### A relação entre o peso do coração e o peso corporal é aparentemente
### linear.

### Ajuste da regressão linear por mínimos quadrados.
ajuste <- lm(Hwt ~ Bwt, data = cats)
ajuste ### Declaração do modelo e estimativas de mínimos quadrados.

summary(ajuste) ### Resumo do modelo ajustado.
names(ajuste) ### Saídas produzidas pela função lm.
coef(ajuste) ### Estimativas de mínimos quadrados dos parâmetros do modelo.

### Estima-se que um quilo a mais no peso corporal esteja associado a um aumento
### médio de aproximadamente 4 gramas no peso do coração.

### O intercepto, por sua vez, não tem interpretação válida neste problema (por que?)

g1 + geom_abline(intercept = coef(ajuste)[1], slope = coef(ajuste)[2],
                 col = 'red', size = 1)
### Gráfico de dispersão com reta de mínimos quadrados ajustada.

fitted(ajuste) ### Valores ajustados pelo modelo para cada indivíduo da base.
residuals(ajuste) ### Resíduo para cada indivíduo da base.

plot(cats$Hwt, fitted(ajuste), xlab = 'Peso do coração observado',
     ylab = 'Peso do coração ajustado', pch = 20, col = 'grey60',
     xlim = c(6.5,21), ylim = c(6.5,21), las = 1)
abline(a = 0, b = 1, lwd = 2, col = 'red')
### Gráfico de valores ajustados vs valores observados. A reta identidade
### representa o que seria um ajuste 'perfeito'.

cor(cats$Hwt, fitted(ajuste))^2 
### Observe que o coeficiente de determinação (R2), que aparece na saída 
### do summary, equivale ao quadrado do coeficiente de correlação linear
### para os valores observados e ajustados da resposta.
### Neste caso, aproximadamente 65% da variação dos pesos dos corações é explicada
### pela reta de regressão ajustada.

plot(fitted(ajuste), residuals(ajuste), pch = 20, xlab = 'Valores ajustados', 
     ylab = 'Resíduos', col = 'grey60', las = 1, cex = 1.3)
### Gráfico de resíduos vs valores ajustados pelo modelo. Este gráfico serve
### para diagnóstico do ajuste. Pontos dispersos aleatoriamente em torno de y=0,
### com variação constante para os diferentes valores de x, indicam ajuste 
### satisfatório do modelo. Comportamentos sistemáticos (não aleatórios)
### seriam indicativos de problemas de ajuste.

confint(ajuste) ### Intervalos de confiança (95%) para os parâmetros do modelo.
### O intervalo (3,54;4,53) contém a alteração no peso médio dos corações decorrente  
### de 1 kg a mais de peso corporal com 95% de confiança.

confint(ajuste, level=0.99) ### ICs (99%) para os parâmetros do modelo.

anova(ajuste) ### Quadro da análise de variância.

predict(ajuste) ### Predições (valores ajustados) para cada indivíduo da base.. 
pred1 <- predict(ajuste, interval = 'confidence') 
### Intervalos de confiança 95% para a resposta média (calculados para 
### cada x na amostra).
head(pred1)

pred2 <- predict(ajuste, interval = 'confidence', level = 0.99) 
### Intervalos de confiança 99% para a resposta média (calculados para cada 
### valor de x na amostra).
head(pred2)

pred3 <- predict(ajuste, interval = 'prediction') 
### Intervalos de predição 95% (calculados para cada valor de x na amostra).
head(pred3)

pred4 <- predict(ajuste, interval = 'prediction',level = 0.99) 
### intervalos de predição 99%  (calculados para cada valor de x na amostra).
pred4

### Vamos estimar a resposta média e fazer previsões para valores de x 
### diferentes dos valores na amostra (no caso, 2.65 e, depois, 2.65, 2.76 e 3.02).

predict(ajuste, newdata=data.frame(Bwt = 2.65), interval='confidence')
### Estima-se em 10,33 gramas o peso médio dos corações para a população de gatos 
### com peso corporal igual a 2,65kg. Um IC(95%) para o peso médio dos corações 
### para a população de gatos com peso corporal igual a 2,65kg tem limites
### (10,09;10,57).

predict(ajuste, newdata=data.frame(Bwt = c(2.65, 2.76, 3.02)), interval='confidence')

predict(ajuste, newdata=data.frame(Bwt = 2.65), interval='prediction')
### Predizemos em 10,33 gramas o peso médio do coração de um "novo" gato 
### com peso corporal igual a 2,65kg. Um IC(95%) para o peso do coração desse gato 
### tem limites (7,45;13,21).

predict(ajuste, newdata=data.frame(Bwt = c(2.65, 2.76, 3.02)), interval='prediction')

### Agora vamos adicionar ao gráfico as bandas de confiança e de predição.
predictions <- predict(ajuste, interval = "predict")
cats_completo <- cbind(cats, predictions)
### Base com os dados originais e as predições (para construção do gráfico)

ggplot(data = cats_completo, aes(x = Bwt, y = Hwt))+
  geom_point(size = 2, col = 'grey50')+
  theme_bw(base_size = 14)+
  xlab('Peso corporal (kg)')+
  ylab('Peso do coração (g)')+
  stat_smooth(method = lm) + #bandas de confiança
  geom_line(aes(y = lwr), col = "coral2", linetype = "dashed", size = 1) + #banda de predição inferior
  geom_line(aes(y = upr), col = "coral2", linetype = "dashed", size = 1) #banda de predição superior

################################################################################
### Exrecício RLS 1

### Dados sobre erupções de um vulcão (Old Faithful geyser in Yellowstone National 
### Park, Wyoming, USA).

data(faithful)
help(faithful)

### O objetivo é ajustar um modelo de regressão para a duração da erupção
### (eruptions) em função do tempo decorrido desde a erupção mais recente
### (waiting), ambos em minutos.

plot(eruptions ~ waiting, data = faithful, pch = 20, col = 'grey60',
     xlab = 'Tempo desde a última erupção',
     ylab = 'Duração da erupção')
### Novamente, parece haver uma relação linear crescente entre as variáveis.

### Exercício: dar sequência à análise, conforme realizado para os dados sobre
### a anatomia dos gatos domésticos.


################################################################################
### Exercício RLS 2

### Para uma amostra de 64 ovinos machos castrados, a necessidade diária de energia
### e o peso foram registrados.

require('GLMsData')
data(sheep)
help(sheep)

### O objetivo é ajustar um modelo de regressão para a energia diária (Energy)
### em função do peso (Weight).

plot(Energy ~ Weight, data = sheep, pch = 20, col = 'grey60', cex = 2, las = 1,
     xlab = 'Peso (kg)',
     ylab = 'Energia (Mcal)')

### Exercício: dar sequência à análise, conforme realizado para os dados sobre
### a anatomia dos gatos domésticos.


################################################################################
### Sessão R - Parte 2 (Simulação - distribuição amostral dos estimadores de mínimos quadrados)

options(device = 'x11')

### Vamos simular dados segundo o modelo de regressão linear simples 
### especificado como y = 5 + 10x + epsilon, com epsilon~Normal(0,2^2).

set.seed(123) ### Fixando a semente, para fins de reprodução dos resultados.
n <- 20 ### Tamanho da amostra
x <- runif(n, 0, 1) ### Gerando valores para a variável explicativa.
beta0 <- 5
beta1 <- 10
sigma <- 2

epsilon <- rnorm(n, 0, sigma) ### Simulando valores para o erro.
y <- beta0 + beta1 * x + epsilon ### Valores simulados de y.


plot(x, y, las = 1, pch = 20, cex = 1.5)
abline(a = 5, b = 10, col = 'red', lwd = 2)
### Gráfico de dispersão com a adição do modelo de regressão teórico (em vermelho).

ajuste <- lm(y ~ x) ### Ajuste da regressão linear simples.
coefficients(ajuste) ### Estimativas dos parâmetros de regressão.
abline(coefficients(ajuste), col = 'green', lwd = 2)
### Adicionando o modelo ajustado ao gráfico (em verde).

### Vamos repetir essa simulação 1000 vezes e adicionar as retas dos modelos
### ajustados.

nsim <- 1000 ### Número de simulações.
plot(x, y, type = 'n', las = 1)

estimativas <- matrix(0, ncol = 2, nrow = nsim)
### Matriz que vai armazenar as esyimativas de mínimos quadrados para cada
### simulação.

### Os códigos a seguir repetem a simulação anterior por 1000 vezes.
for(i in 1:nsim){
  epsilon <- rnorm(n, 0, 2)
  y <- beta0 + beta1 * x + rnorm(n, 0, 2)
  ajuste <- lm(y ~ x)
  estimativas[i,] <- coefficients(ajuste)
  abline(estimativas[i,], lty = 2, col = 'grey')}
abline(a = 5, b = 10, col = 'red', lwd = 2) ### Modelo teórico.

head(estimativas, 20) 
### Matriz que armazena, em cada linha, as estimativas de uma das simulações.

summary(estimativas)

### Agora, vamos analisar a distribuição das estimativas obtidas via simulação.
par(mfrow = c(1,3), cex = 1.4, las = 1)
hist(estimativas[,1], xlab = expression(hat(beta)[0]), main = '')
hist(estimativas[,2], xlab = expression(hat(beta)[1]), main = '')
plot(estimativas, xlab = expression(hat(beta)[0]), ylab = expression(hat(beta)[1]))

################################################################################
### Sessão R - Parte 3 (Simulação - delineamento para regressão)

### Função usada para simular nsim amostras de tamanho n=10 de y~Normal(5+10x, 10^2).
### O vetor x é fixo para todas as simulações, sendo que quatro alternativas foram
### consideradas:

x1 <- 1:10; x1 ### Alocação 1
x2 <- rep(seq(2, 10, 2), 2); x2 ### Alocação 2
x3 <- c(1, rep(5,8), 10); x3 ### Alocação 3
x4 <- c(rep(1, 5), rep(10, 5)); x4 ### Alocação 4

### Esta função vai ser usada para simular dados do modelo especificado, para cada uma 
### das alocações.
fsimul_reg <- function(x, nsim, vet){
  plot(c(1,10), c(0,120), type = 'n', xlab = 'x', ylab = 'y', las = 1, main = vet)
  for(i in 1:nsim){
    y <- 5 + 10 * x + rnorm(length(x), 0, 10)
    ajuste <- lm(y ~ x)
    estimativas[i,] <- coefficients(ajuste)
    abline(estimativas[i,], lty = 2, col = 'grey')}
  abline(a = 5, b = 10, col = 'red', lwd = 2)
  return(estimativas)
}

### Vamos usar apenas nsimul=100 simulações para permitir uma melhor visualização
### dos resultados no gráfico. A fim de avaliar a precisão das estimativas,
### vamos calcular as respectivas variâncias em cada um dos quatro cenários
### simulados.

par(mfrow = c(2,2))
fx1 <- fsimul_reg(x1, 100, 'x1'); apply(fx1, 2, var) 
fx2 <- fsimul_reg(x2, 100, 'x2'); apply(fx2, 2, var) 
fx3 <- fsimul_reg(x3, 100, 'x3'); apply(fx3, 2, var) 
fx4 <- fsimul_reg(x4, 100, 'x4'); apply(fx4, 2, var)

### A variância é maior (menor precisão) para a alocação 3, isso é, quando
### a maioria das observações é alocada no ponto central (x = 5). Por outro
### lado, a variância é menor (maior precisão) sob alocação 4, isto é,
### quando as observações são alocadas nos extremos (metade delas em cada
### um dos extremos). Esses resultados apenas confirmam os resultados teóricos
### estudados antes, em sala de aula.

################################################################################
### Sessão R - Parte 4 (Simulação - intervalos de confiança e testes de hipóteses)

set.seed(456) ### Fixando a semente (para fins de reprodução)

n <- 20
x <- runif(n, 0, 1) ### Gerando valores para a variável explicativa.
x <- x - mean(x)
beta0 <- 5
beta1 <- 10
sigma <- 2

nsim <- 100 ### Número de simulações.
ics <- matrix(0, ncol = 2, nrow = nsim)
### Matriz que vai armazenar em cada linha os intervalos de confiança para beta1.

for(i in 1:nsim){
  epsilon <- rnorm(n, 0, sigma) ### Simula os erros.
  y <- beta0 + beta1 * x + epsilon ### Calcula y.
  ajuste <- lm(y ~ x) ### Ajuste do modelo.
  ics[i,] <- confint(ajuste)[2,] ### Calcula os ICs e armazena os de beta1. 
}
### Para cada simulação uma amostra de tamanho n = 20 é simulada de
### y ~ Normal(5 + 10x, 2^2), o modelo de regressão linear simples é
### ajustado e o intervalo de confiança para beta1 é armazenado.

head(ics, 20)
ics100 <- ics
### Vamos visualizar os 100 ICs:
plot(1, 1, xlim = c(min(ics100[,1]), max(ics100[,2])), type = 'n', ylim = c(0,100))
for(i in 1:nrow(ics100))
  lines(x = c(ics100[i,1], ics100[i,2]), y = c(i,i), col = "blue")
abline(v=beta1, col = 'red')

### Agora, vamos repetir o mesmo processo um número maior de vezes (5000) para
### calcular a taxa de cobertura, isto é, a porcentagem de vezes que o IC obtido
### de fato continha o valor especificado de beta1.

nsim <- 5000 ### Número de simulações.
ics <- matrix(0, ncol = 2, nrow = nsim)
### Matriz que vai armazenar em cada linha os intervalos de confiança para beta1.

for(i in 1:nsim){
  epsilon <- rnorm(n, 0, sigma) ### Simula os erros.
  y <- beta0 + beta1 * x + epsilon ### Calcula y.
  ajuste <- lm(y ~ x) ### Ajuste do modelo.
  ics[i,] <- confint(ajuste)[2,] ### Calcula os ICs e armazena os de beta1. 
}

indica <- ics[,1] < beta1 & ics[,2] > beta1 
### Vetor lógico. Se TRUE o intervalo de confiança simulado contém o valor
### de beta1. Se FALSE, o intervalo não contém beta1.

100*mean(indica) ### Porcentagem de intervalos simulados que cobriram beta1.
### Observe que taxa de cobertura simulada é bastante próxima ao nível de 
### confiança fixado (95%).


################################################################################
### Sessão R - Parte 5 (Teste de hipóteses para a significância da regressão
### linear e análise da falta de ajuste)

### Dados sobre corrosão de ligas sob diferentes teores de ferro. Consultar a 
### docuumentação da base para maiores detalhes.

library(faraway)
data(corrosion)
help(corrosion)
plot(corrosion, pch = 20, cex = 1.4)
ajuste <- lm(loss ~ Fe, data = corrosion)
### Regressão linear simples.

coefficients(ajuste)
abline(coefficients(ajuste))

summary(ajuste) 
### O coeficiente de determinação é aproximadamente 0.97, ou seja, aproximadamente
### 97% da variação da resposta é explicada pela reta de regressão ajustada.

### Teste de hipóteses para a significância da regressão linear

anova(ajuste) 
### Quadro de análise de variância para a regressão linear.
### Para testar a significância da regressão linear consideramos a estatística
### F = QMReg/QMRes = (3293.8/1)/(102.9/11) = 352.27. 

### Sob a hipótese nula de que \beta_1 = 0 (não significância da regressão linear)
### F tem distribuição F-Snedecor com 1 e 11 graus de liberdade.
### Vamos achar os valores críticos para as decisões aos níveis de 5 e 1% 
### de significância (consulte também a tabela).

qf(0.95, df1 = 1, df2 = 11) ### Valor crítico do teste para alpha = 5%;
qf(0.99, df1 = 1, df2 = 11) ### Valor crítico do teste para alpha = 1%.

### Como em ambos os casos os F calculado excede o valor crítico,
### temos evidências de que \beta_1 é diferente de zero (regressão linear 
### significativa) para os dois níveis de significância considerados.

### Vamos extrair o p-valor:
pf(q = 352.27, df1 = 1, df2 = 11, lower.tail = FALSE)

### Lembre-se que para o caso da regressão linear simples o teste F realizado
### é equivalente ao teste t para a hipótese H_0: \beta_1 = 0.


### Análise da falta de ajuste

### O gráfico de resíduos vs valores ajustados é importante para se avaliar
### possível falta de ajuste.

plot(ajuste, which = 1)
### Percebe-se padrão não aleatório na dispersão dos resíduos. A tendência
### observada reflete possível não linearidade na relação entre as variáveis
### e, consequentemente, falta de ajuste.


### Agora, vamos proceder formalmente com o teste da falta de ajuste.
### Para isso, vamos considerar um modelo alternativo, em que o valor ajustado
### para y|x_i é simplesmente a média dos y's avaliados em x_i.

ajuste2 <- lm(loss ~ 0 + factor(Fe), data = corrosion)
summary(ajuste2)
### Observe o maior número de parâmetros do modelo, definido pelo número
### de valores distintos de x. Para cada valor distinto de x o modelo ajusta
### uma média. Assim, o modelo pode ser escrito por y_ij = beta_i + epsilon_ij,
### i = 1, 2, ..., m (valores disntos de x); j = 1, 2, ..., n_i.  

anova(ajuste2) 
### Quadro da análise de variância para o segundo ajuste.

anova(ajuste, ajuste2)
### A primeira linha do quadro traz a soma de quadrados de resíduos e correspondente
### número de graus de liberdade para a regressão linear simples.

### A segunda linha do quadro traz a soma de quadrados de resíduos e correspondente
### número de graus de liberdade para o modelo de médias. Importante notar
### que, para o modelo de médias, a soma de quadrados de resíduos corresponde
### à soma de quadrados de erros puros do primeiro.

### A terceira e a quarta coluna do quadro apresentam as diferenças dos dois
### modelos para as somas de quadrados de resíduos e graus de liberdade 
### correspondentes. Logo, tratam-se da soma de quadrados e graus de
### liberdade para a falta de ajuste. 

### A estatística F é calculada por QMFA/QMEP, ou seja: (91.069/5)/(11.782/6) = 9.275.

### Sob a hipótese nula de que não há falta de ajuste, F ~ F-Snedecor, 
### com 5 e 6 graus de liberdade.

### Vamos testar a hipótese nula aos níveis de 5 e 1% de significância.
### Recomendo consulta às tabelas.

qf(0.95, df1 = 5, df2 = 6) ### Valor crítico do teste para alpha = 5%;
qf(0.99, df1 = 5, df2 = 6) ### Valor crítico do teste para alpha = 1%.

### Como em ambos os casos o F calculado excede o valor crítico, temos evidências
### de falta de ajuste nos dois níveis de significância considerados.

### Vamos extrair o p-valor:

pf(q = 9.275, df1 = 5, df2 = 6, lower.tail = FALSE)

### Vamos ajustar um modelo polinomial (cúbico) e avaliar o ajuste.

ajuste3 <- lm(loss ~ Fe + I(Fe^2) + I(Fe^3), data = corrosion)
pred_Fe <- data.frame(Fe = seq(0, 2, 0.05))
pred_loss <- predict(ajuste3, newdata = pred_Fe)
plot(corrosion, pch = 20, cex = 1.4)
lines(pred_Fe$Fe, pred_loss, col = 'red')

summary(ajuste3)
anova(ajuste3, ajuste2)
### Embora o modelo polinomial naturalmente tenha proporcionado melhor ajuste,
### ainda assim o teste acusa que a falta de ajuste é significativa frente
### ao erro puro.

### Deve-se tomar cuidado para não se exagerar na complexidade do modelo
### para contornar a falta de ajuste. Como exemplo, poderíamos tentar
### um polinômio de grau 6 para este problema.

ajuste4 <- lm(loss ~ Fe + I(Fe^2) + I(Fe^3) + I(Fe^4) + I(Fe^5) + I(Fe^6), data = corrosion)
pred_Fe <- data.frame(Fe = seq(0, 2, 0.05))
pred_loss <- predict(ajuste4, newdata = pred_Fe)
plot(corrosion, pch = 20, cex = 1.4, ylim = c(60,130))
lines(pred_Fe$Fe, pred_loss, col = 'red')

### Embora esse modelo se ajuste melhor aos dados, ele claramente não explica  
### adequadamente a relação entre as variáveis.

### Exercício - Estudo do tipo dose-resposta sobre eclosão de ovos sob 
### diferentes doses de herbicida.

# Os dados apresentados foram produzidos por um experimento com uma amostra de 
# 50 C.dubia (pequeno animal invertebrado aquatico de agua doce), que foram 
# submetidos a dosagens diferentes do herbicida Nitrofen: 0, 80, 160, 235 e 310 mg/l.
# Para cada nível de Nitrofen 10 animais ficaram expostos e foi observado o
# total de ovos eclodidos após 3 ninhadas. Faça inicialmente uma análise
# descritiva dos dados, por exemplo um diagrama de dispersão entre o
# numero de ovos eclodidos (tovos) contra o nível de exposição do herbicida (dose). 
# Ajuste o modelo de regressão linear simples e acrescente a reta ajustada ao gráfico.
# Depois, teste formalmente a significância e a falta de ajuste do modelo. Caso
# seja verificada falta de ajuste, proponha e ajuste um modelo alternativo.

dose <- c(0,   0,   0,   0,   0,   0,   0,   0,   0,   0,  
          80,  80,  80,  80,  80,  80,  80,  80,  80,  80,
          160, 160, 160, 160, 160, 160, 160, 160, 160, 160,
          235, 235, 235, 235, 235, 235, 235, 235, 235, 235,
          310, 310, 310, 310, 310, 310, 310, 310, 310, 310)

tovos <- c(37, 32, 34, 33, 36, 34, 33, 30, 24, 31,
           33, 33, 35, 33, 36, 26, 27, 31, 32, 29,
           29, 29, 23, 27, 30, 31, 30, 26, 29, 29,
           23, 21,  7, 12, 27, 16, 13, 15, 21, 17,
           6,  6,  7,  0, 15,  5,  6,  4,  6,  5)   

################################################################################
### Sessão R - Parte 6 (Regressão intrinsicamente linear)

### Exemplo 1 - Dados sobre desempenho de estudantes num exame de Matemática.

require(AER)
data("CASchools")
head(CASchools)
help(CASchools)
options(device = 'x11')

### Objetivo - ajustar um modelo de regressão que explique a relação entre
### o desempenho no exame de Matemática e a renda média dos habitantes
### dos distritos.

par(mar = c(4.5,4,2,2), las = 1)
plot(CASchools$income, CASchools$math, xlab = 'Renda média do distrito (em U$1.000)',
     ylab = 'Escore médio (Matemática)', pch = 20, col = 'grey60', cex = 1.4)
### A relação entre as variáveis claramente não é linear.


### Modelo 1 - Regressão linear simples

ajuste1 <- lm(math ~ income, data = CASchools)
summary(ajuste1) ### R2 = 0.4892

abline(coef(ajuste1), col = 'red', lwd = 2)
### Adicionando a reta de regressão ajustada ao gráfico de dispersão.

plot(ajuste1, which = 1, lwd = 2)
### Os resíduos apresentam uma curvatura em sua distribuição, e o gráfico
### de dispersão com a reta ajustada confirma a falta de ajuste do modelo.


### Modelo 2 - Regressão com a covariável transformada (log(renda)).
plot(log(CASchools$income), CASchools$math, xlab = 'Log (Renda)',
     ylab = 'Escore médio (Matemática)', pch = 20, col = 'grey60', cex = 1.4)
### Quando tomamos a log-renda, a relação entre as variáveis praticamente
### lineariza. Vamos ajustar um novo modelo, com a renda inserida na escala
### logarítmica.

ajuste2 <- lm(math ~ log(income), data = CASchools)
summary(ajuste2) ### R2 = 0.526

ndata  <- data.frame(income = seq(min(CASchools$income), max(CASchools$income), 0.05))
### Grid de valores de income para plotar o modelo ajustado.

plot(CASchools$income, CASchools$math, xlab = 'Renda média do distrito (em U$1.000)',
     ylab = 'Escore médio (Matemática)', pch = 20, col = 'grey60', cex = 1.4)
lines(ndata$income, predict(ajuste2, newdata = ndata), col = 'red', lwd = 2)
### O modelo com a covariável transformada se ajusta melhor aos dados que o modelo
### anterior (sem transformação).

plot(ajuste2, which = 1, lwd = 2)
### O gráfico de resíduos não apresenta a curvatura observada antes.


### Exemplo 2 - Dados sobre velocidade de moinhos de vento (wind) e energia
### gerada (DC)

### Entrando com os dados
wind <- c(5.00, 6.00, 3.40, 2.70, 10.0, 9.70, 9.55, 3.05, 8.15, 6.20, 2.90, 6.35, 4.60,
          5.80, 7.40, 3.60, 7.85, 8.80, 7.00, 5.45, 9.10, 10.2, 4.10, 3.95, 2.45)

energy <- c(1.582, 1.822, 1.057, 0.500, 2.236, 2.386, 2.294, 0.558, 2.166, 1.866,
            0.653, 1.930, 1.562, 1.737, 2.088, 1.137, 2.179, 2.112, 1.800, 1.501,
            2.303, 2.310, 1.194, 1.144, 0.123)

plot(wind, energy, pch = 20, cex = 1.4, las = 1, col = 'grey60')

### Modelo 1 - Regressão linear simples.


ajuste1 <- lm(energy ~ wind)
abline(coefficients(ajuste1), col = 'red', lwd = 2)

### A relação entre as variáveis não é linear. Percebe-se um aumento mais
### acentuado na geração de energia para acréscimos na velocidade do vento
### sob velocidades menores do que para velocidades maiores.

summary(ajuste1) ### R2 = 0.8745.
plot(ajuste1, which = 1, pch = 20, cex = 1.4, lwd = 2)
### O gráfico de resíduos apresenta uma clara curvatura, sinalizando a falta 
### de ajuste do modelo.


### Modelo 2 - Regressão polinomial (inclusão de termo quadrático para wind)
ajuste2 <- lm(energy ~ wind + I(wind^2))
summary(ajuste2) ### R2 = 0.9676.

wind_grid <- data.frame(wind = seq(min(wind), max(wind), length.out = 100))
### Grid de valores para a velocidade do vento.

pred_m2 <- predict(ajuste2, newdata = wind_grid)
### Valores ajustados pelo modelo para cada valor de velocidade do vento
### no grid criado.

plot(wind, energy, pch = 20, cex = 1.4, las = 1, col = 'grey60')
lines(wind_grid$wind, pred_m2, lwd = 2, col = 'green')
### Gráfico de dispersão com modelo de regressão ajustado.

plot(ajuste2, which = 1, pch = 20, cex = 1.4, lwd = 2)
### Gráfico de resíduos.

### Embora o modelo quadrático se ajuste melhor aos dados eue o linear,
### os resíduos ainda apresentam curvatura, indicando que ainda há
### alguma falta de ajuste.

### Modelo 3 - Regressão intrínseca: y = beta_0 + beta_1 (1/x) + epsilon.

plot(1/wind, energy, pch = 20, cex = 1.4, las = 1, col = 'grey60')
### Observe que a relação entre energia gerada e o inverso da velocidade
### do vento parece bastante linear. Vamos ajustar um modelo de regressão
### inserindo o inverso da velocidade como variável explicativa. 

ajuste3 <- lm(energy ~ I(1/wind))
summary(ajuste3) ### R2 = 0.98.

pred_m3 <- predict(ajuste3, newdata = wind_grid)

plot(wind, energy, pch = 20, cex = 1.4, las = 1, col = 'grey60')
lines(wind_grid$wind, pred_m3, lwd = 2, col = 'blue')

plot(ajuste3, which = 1, pch = 20, cex = 1.4, lwd = 2)
### O modelo 3 se ajusta satisfatoriamente Não há indicativos de falta de
### ajuste com base nos resíduos.


### Exemplo 3 - Dados de pressão do vapor para água sob diferentes temperaturas.

temp <- c(273,  283, 293, 303, 313,	323, 333, 343, 353, 363, 373)
pres <- c(4.6, 9.2,	17.5, 31.8,	55.3, 92.5,	149.4, 233.7, 355.1, 525.8,	760.0)

plot(temp, pres, pch=20, cex = 1.4, las = 1) 

### A relação entre temperatura e pressão é claramente não linear.
### Da literatura da Química sabe-se que ln(p) é proporcional a -1/t
### (equação de Clausius-Claperyon). 

### Assim, vamos ajustar o seguinte modelo: ln(y) = \beta_0 + \beta_1 (1/t) + epsilon.
### Na escala original teríamos y = exp{\beta_0 + \beta_1 (1/t)} * epsilon',
### em que epsilon' = exp{epsilon}. Trata-se, portanto, de um modelo com 
### erros multiplicativos (e não aditivos).

log_p <- log(pres)
inv_t <- 1/temp

ajuste <- lm(log_p ~ inv_t)
summary(ajuste)
### Com base nas estimativas obtidas, a expressão do modelo ajustado fica
### dada por \hat{pres} = exp{20.61 - 5201 * (1/temp)}.

p_pred <- function(temp, beta) exp(beta[1]+betas[2]/temp)
betas <- coef(ajuste)
curve(p_pred(x, beta = betas), from = min(temp), to = max(temp), add = T, col = 'red')


################################################################################
### Análise de correlação

### O banco de dados parana, disponível na página da disciplina, contém informações 
### obtidas no Censo 2010 para os municípios do Estado do Paraná. As variáveis 
### que compõem a base são as seguintes:

# Município: Nome do município;

# Urbanização: Porcentagem da população residente na área urbana em relação à 
# população total;

# Idosos - Relação (%) entre o nº de pessoas idosas (65 anos ou mais) e o número 
# de pessoas nos grupos etários mais jovens (15 anos ou menos).

# P60 - Probabilidade de sobrevivência (60 anos)

# Renda - Renda média (R$)

# Analfabetismo - Taxa de analfabetismo (15 anos ou mais)

require("ggplot2")
require("GGally")
require("dplyr")

dados <- read.csv2('parana.csv') 
### Importação dos dados.

summary(dados) ### Resumo dos dados.
head(dados) ### Seis primeiras linha.

### Vamos começar avaliando a correlação entre a taxa de analfabetismo e a probabilidade
### de sobreviver aos 60 anos.

ggplot(dados, aes(x = Analfabetismo, y = P60))+
  geom_point()+
  theme_bw(base_size = 14) +
  geom_smooth(col = 'red')

cor(dados$Analfabetismo, dados$P60)
### Coeficiente de correlação linear de Pearson


cor.test(dados$Analfabetismo, dados$P60) 
### Teste de hipótese e intervalo de confiança para a correlação.
### Lembre-se que a hipótese nula é de correlação igual a zero. Verifique, na 
### documentação da função, como é especificada a hipótese alternativa.

cor.test(dados$Analfabetismo, dados$P60, alternative='less') 
### Trocando a hipótese alternativa.

cor(dados[,-1])
### Matriz de correlações

x11()
ggpairs(dados[,-1])+
  theme_bw(base_size = 14)
### Matriz de gráficos de dispersão.


################################################################################
### Exercício 1 - Dados sócio econômicos do censo do Paraná.
### Variáveis:
# Município;
# Urbanização: Grau de urbanização (%);
# Idosos: Índice de idosos (%);
# P60: Probabilidade de sobrevivência até 60 anos;
# Renda: Renda média domiciliar;
# Analfabetismo: Taxa de analfabetismo.

dados <- read.csv2('parana.csv') ### Importação dos dados.
names(dados)
with(dados, plot(Analfabetismo, Renda, pch = 20)) 

### Proponha um modelo de regressão que explique adequadamente a renda média 
### domiciliar em função da taxa de analfabetismo. Você pode começar
### por uma regressão linear simples e, num segundo momento, tentar alguma regressão
### intrinsicamente linear, transformando uma ou ambas as variáveis.


################################################################################
### Exercício 2- Dados sobre capacidade pulmonar de jovens.

require('GLMsData')

data("lungcap")
help("lungcap")

plot(FEV ~ Ht, data = lungcap, pch = 20, col = 'grey60', cex = 2, las = 1,
     xlab = 'Altura (polegadas)',
     ylab = 'Volume expiratório (litros)')

### O objetivo é ajustar um modelo de regressão para o volume expiratório em 
### função da altura. Perceba que a relação não é linear, e tente transformar
### a resposta para log(FEV). Finalmente, usando o fato que log(E(FEV)) pode
### ser satisfatoriamente aproximado por E(log(FEV)), apresente suas interpretações
### para as estimativas dos parâmetros do modelo.