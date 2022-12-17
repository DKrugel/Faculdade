library(tidyverse)
challenger <-  read.csv(file = "http://leg.ufpr.br/~lucambio/CE073/20222S/challenger.csv")

challenger %>% 
  group_by(O.ring) %>% 
  summarise(total = n()) %>% 
  ggplot(aes(x = O.ring, y = total)) + 
  geom_bar(stat = "identity")

summary(challenger$O.ring) # Aquele summary para verificar se não há nenhum dado faltante 

# Como O.ring é um dado de contagem imaginei que ele poderia fazer parte da família Poisson de distribuições generalizadas, observando o gráfico
# Me pareceu a melhor escolha realmente, então ajustando o glm usando a função de ligação canônica, temos:

ajuste <- glm(O.ring ~ Temp + Pressure, 
              family = poisson(link = "log"),
              data = challenger)

coef(ajuste)

## e^(3.533-0.0868*Temp+0.00795*Pressure)
## Forma linear da regressão logística

car::Anova(ajuste, test = "LR")

# Pressure não foi significante com um -2log(lambda) = 1.4041 e com um p-valor de P(A > 1.4041) = 0.236. Enquanto Temp foi significativa porém
# A hipótese nula de que B_change = 0 não seria rejeitado caso meu nível de significância alpha fosse igual a 0.01.

cor(challenger$Temp, challenger$Pressure)
cor.test(challenger$Temp, challenger$Pressure,
         alternative = c("two.sided"))

# O teste pearson para correlação entre os dados deu que a correlação entre os dados é de aproximadamente 0.04 com um p valor de 0.8569, me
# Não me fornecendo indicativos o suficiente para rejeitar a hipótese nula de que a cor entre Temp e Pressure é 0.
# Portanto não devemos ver resultados diferentes caso optemos por comparar modelos em que a entrada das covariáveis seja diferente 
# Ou na realização de um teste de análise das deviances de forma sequencial.

anova(ajuste, test = "Chisq")

ajuste.transposto <- glm(O.ring ~ Pressure + Temp, 
                family = poisson(link = "log"),
                data = challenger)

anova(ajuste, ajuste.transposto) # A mudança entre a ordem de entrada não foi significativa

ajuste.real  <- glm(O.ring ~ Temp, 
                      family = poisson(link = "log"),
                      data = challenger)

anova(ajuste.real, ajuste, test = "Chisq") # A remoção do parâmetro não foi significativa a qualidade do ajuste

# Sendo que o pvalor de P(A > 1.4041) = 0.236 a adição da variável não é estatísticamente significativa a nenhum valor abaico de alpha = 0.05
# Portanto a escolha de permanecer com o modelo mais simples faz sentido. Claro que a base de dados tem apenas 23 observações, então caso o 
# experimento seja refeito, com uma amostra maior possamos indentificar efeitos mais extremos que podem ter passado despercebidos e desenvolver
# um modelo mais preciso, porém com base neste experimento este é o modelo minimal para estre problema.


#####################
##### Questão 7 #####
#####################

# Carregando dados
tb <-  read.csv ( file = "http://leg.ufpr.br/~lucambio/CE073/20222S/placekick.BW.csv")
placekick <- read.csv(file = "http://leg.ufpr.br/~lucambio/CE073/20222S/Placekick.csv")


#### tentativa de fazer um gráfico sigmoidal
for(i in 1:length(tb$Good)){
  if(tb$Good[i] == "Y"){
    tb$Good[i] <- 1
  }else{
    tb$Good[i] <-0
  }
}
 tb$Good <- as.numeric(tb$Good)
# 
# plot(x =tb$Distance, y = tb$Good, pch=20, ylim = c(-0.2, 1.2), yaxt = 'n')
# axis(2, seq(0, 1, 0.2), las = 1)
# abline(h = 0, lty = 2)
# abline(h = 1, lty = 2)
# 
# ### Agora para o glm:
# x.grid <- seq(10,70, length.out = 100)
# ajuste7 <- glm(Good ~ Distance, data = tb, family = binomial(link = 'logit'))
# lines(x.grid, predict(ajuste, newdata = data.frame(x = x.grid), 
#                       type = 'response'), col = 'red', lwd = 2)
# 
# summary(ajuste7)

ajuste7 <- glm(Good ~ Distance, family = binomial(link = 'logit'), data = tb)
summary(ajuste7)

# Neste caso ajustando a fórmula Good ~ Distance utilizando a família binomial ( Já que a variável resposta é dicotômica ) usando a função de ligação
# logito podemos verificar se o R está modelando a probabilidade de sucesso ou de falha ao olhar na estimativa de Beta1, caso ela seja negativa
# Estamos olhando para uma regressão modelando a probabilidade de FALHA, caso a estimativa seja positiva uma probabilidade de SUCESSO.

# Isto se dá pelo formato da curva sigmoidal gerada através da função de ligação, quando a estimativa Beta1 é negativa a curva é decrescente
# Dando a entender que as probabilidades começam altas onde há um peso maior 

beta0 <- coef(ajuste7)[[1]]
beta1 <- coef(ajuste7)[[2]]
curve ( expr = exp( beta0 + beta1 *x) / (1+ exp( beta0 + beta1 *x)), xlim = c(0, 100) , 
        col = " black ", main = expression (pi == frac (e ^{5.4092-0.1062* x[1]} , 1+e ^{5.4092-0.1062* x [1]}) ), 
        xlab = expression (x [1]) , ylab = expression (pi))


curve ( expr = exp( beta0 + beta1 *x) / (1+ exp( beta0 + beta1 *x)), xlim = c(0, 100) , 
        col = " black ", main =  "Modelos sobrepostos", 
        xlab = expression (x [1]) , ylab = expression (pi))
curve ( expr = exp( 5.8121 - 0.1150 *x) / (1+ exp( 5.8121 - 0.1150 *x)), xlim = c(0, 100), col = 'red', ylab = "", xlab = "", add = TRUE)

# Como ambos os estudos são parecidos analisando uma variável em função de outra e ambas idealmente seguem as mesmas distribuições de probabilidade
# na prática é como se os dados fornecidos no conjunto placekick.BW fossem uma repetição de um experimento. (Acredito que seja algo por este pensamento, meu conhecimento de futebol é bem fraco, não entendi a maior parte das variáveis).


######################
##### Questão 19 #####
######################

#Carregando dados
data  <-  read.csv(file = "http://leg.ufpr.br/~lucambio/ADC/healthcare_worker.csv") 

#Reordenando os níveis dos grupos de ocupação para que a parcela de menor contato ser o grupo controle
class(data$Occup.group)
data$Occup.group <- as.factor(data$Occup.group)
data$Occup.group <- relevel(data$Occup.group, ref = "No patient contact")
levels(data$Occup.group)

#Ajustando o GLM da família Binomial com a funçào de ligação LOGITO
ajuste19 <- glm(Hepatitis/Size ~ Occup.group, 
                family = binomial(link = 'logit'),
                weights = Size,
                data = data)

#Análise das deviances
car::Anova(ajuste19)

#Nenhum dos grupos foi significativo a um alpha de 0.05

# Vamos dar uma olhada nas razões de chances (odds ratio) para estudar mais a fundo 

as.data.frame(exp(1*ajuste19$coefficients[1:5]))

# Nenhuma das Odds Ratio apresenta um grande aumento, o mais destacado é o grupo responsável pelo manejamento de líquidos em laboratório.

# Consultando o aluno Adam Domingues de Enfermagem da PUC e a aluna Eduarda Santini de Farmácia da UFPR e alguns dos motivos para que os grupos onde se
# foi estudado a contaminação de Hepatite C não serem significativos para o aumento da contaminação é de que a doença não tem vacina, ao contrário da variaçãoo B da doença
# Que e obrigatória para agentes de saúde segundo o programa nacional de saúde (PNI), portanto há um cuidado muito grande para a prevenção com materiais de proteção. A exceção seria justamente nos laboratórios,
# que apresentam o maior risco de descuido e o maior número de ponto de falhas, o que explica a sua odds ratio ser levemente maior do que a dos outros grupos.

######################
##### Questão 22 #####
######################

picloram <- read.csv( file = "http://leg.ufpr.br/~lucambio/ADC/picloram.csv")


#Ajustando o número de ervas danínhas mortas por nível de herbicída
ajuste22 <- glm(kill ~ picloram,
                     family = poisson(link = 'log'),
                     weights = total,
                     data = picloram)

summary(ajuste22)


## utilizei a família poisson com a função de ligação canônica já que o número de plantas mortas é um dado de contagem, utilizei o argumento weights já que os dados
## não são balanceados. O resultado do ajuste deu que o nível de Picloram é significativo.

# Ajustando a proporção como variável resposta

ajuste22.prop <- glm(kill/total ~ picloram,
                family = binomial(link = 'logit'),
                weights = total,
                data = picloram)

summary(ajuste22.prop)

## Ajustando para a proporção de mortes das ervas danínhas por efeito do herbicída utilizei a família binomial com o link Logito. Assim como 
## ajustando pela quantidade, ambas foram significativas, portanto irei fazer um teste de tabela de variância para comparar ambos os modelos.

anova(ajuste22, ajuste22.prop)

## A diferença entre os modelos não teve uma diferença significativa, portanto ambos os modelos são viáveis.


beta0 <- coef(ajuste22.prop)[[1]]
beta1 <- coef(ajuste22.prop)[[2]]
curve ( expr = exp( beta0 + beta1 *x) / (1+ exp( beta0 + beta1 *x)), xlim = c(0, 4) , 
        col = " black ", 
        xlab = expression (x [1]) , ylab = "kill/total")


root.func <- function (x, mod.fit.obj , pi0 , alpha ) { 
  beta.hat <- mod.fit.obj$coefficients 
  cov.mat <- vcov (mod.fit.obj)
  var.den <- cov.mat [1 ,1] + x^2* cov.mat [2 ,2] + 2*x* cov.mat [1 ,2] 
  abs ( beta.hat [1] + beta.hat [2]* x - log (pi0 /(1 - pi0))) /  sqrt ( var.den) - qnorm (1- alpha /2) } 

lower <- uniroot (f = root.func , interval = c(min( picloram$picloram ), LD.x), mod.fit.obj = ajuste22 , pi0 = 0.9 , alpha = 0.95)
lower # lower$root contains the lower bound 

upper <- uniroot (f = root.func , interval = c(LD.x, max ( set1$picloram )), mod.fit.obj = mod.fit , pi0 = 0.9 , alpha = 0.95)
upper # upper$root contains the upper bound


x <- data.frame(seq(1,12, by = 1))



segment()
