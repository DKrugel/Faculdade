########################################################################
### UD6 - Seleção de covariáveis

### Para começar, vamos carregar os pacotes necessários para as análises.
require('SportsAnalytics')
require(car)
require(leaps)
options(device = 'x11')

### Vamos usar a base de dados EURO4PlayerSkillsSep11 do pacote SportsAnalytics.
### O objetivo é modelar a precisão em passes de longa distância de jogadores
### de futebol em função de covariáveis referentes a características dos jogadores.

load("EURO4PlayerSkillsSep11.Rdata")
head(EURO4PlayerSkillsSep11)
summary(EURO4PlayerSkillsSep11)

### Selecionando as covariáveis para análise.
dados <- EURO4PlayerSkillsSep11[-1166,c('Agility' , 'Acceleration' , 'TopSpeed' 
                                        ,'Balance', 'Jump', 'Teamwork', 'Mentality', 
                                        'ConditionFitness','Height', 'Age', 'Weight',
                                        'LongPassAccuracy')]
dados <- na.omit(dados)
### O jogador da linha 1166 foi excluído pois os dados correspondentes, em
### sua maioria, eram iguais a zero (provavelmente dados missing). Além
### disso, 37 jogadores foram excluídos da base por não terem todas
### as informações disponíveis. 

dim(dados) ### Dimensão da base de dados final.

plot(dados, cex = 1.4, col = 'navy', pch = 20)

### Antes de usar a função step para selecionar covariáveis via testes
### de hipóteses usando os algoritmos backward, forward e stepwise,
### vamos fazer isso no braço, para fins de ilustração.

########################################################################
### Algoritmo backward

ajuste <- lm(LongPassAccuracy ~ ., data = dados)
### Ajuste com todas as covariáveis.

drop1(ajuste, test = 'F')
### A função drop1 apresenta os resultados do teste F produzidos mediante
### extração das covariáveis do modelo (uma a uma). A variável Age tem
### maior p-valor (p = 0.9698) e será eliminada do modelo.

ajuste2 <- update(ajuste, ~.-Age) ### Modelo ajustado sem a variável Age.
drop1(ajuste2, test = 'F')
### A variável ConditionFitness tem maior p-valor (p = 0.6295) e será 
### eliminada do modelo.

### Segue o processo.

ajuste3 <- update(ajuste2, ~.-ConditionFitness)
drop1(ajuste3, test = 'F') ### Sai Acceleration.

ajuste4 <- update(ajuste3, ~.-Acceleration)
drop1(ajuste4, test = 'F') ### Sai Mentality.

ajuste5 <- update(ajuste4, ~.-Mentality)
drop1(ajuste5, test = 'F') ### Sai Weight.

ajuste6 <- update(ajuste5, ~.-Weight)
drop1(ajuste6, test = 'F')
### Todas as variáveis remanescentes têm efeito significativo. O processo
### é encerrado com o modelo atual, sem excluir novas covariáveis.

summary(ajuste6) ### Resumo do modelo final.

########################################################################
### Algoritmo forward

ajuste0 <- lm(LongPassAccuracy ~ 1, data = dados)
### Modelo nulo (só com intercepto).

add1(ajuste0, scope=~Agility + Acceleration + TopSpeed 
     + Balance + Jump + Teamwork + Mentality + ConditionFitness 
     + Height + Age + Weight, test = 'F')
### A função add1 apresenta os resultados do teste F produzidos mediante
### inclusão das covariáveis do modelo (uma a uma). A variável TopSpeed tem
### maior valor para a estatística F (F = 865.2358), e p-valor praticamente
### igual a zero.

### Nota - neste caso avaliar menor p-valor e maior valor da estatística F
### é equivalente apenas porque todas as covariáveis têm um grau de liberdade
### associado.

ajuste2 <- lm(LongPassAccuracy ~ TopSpeed, data = dados)
### Modelo com a inclusão de TopSpeed.

add1(ajuste2, scope=~Agility + Acceleration  + TopSpeed
     + Balance + Jump + Teamwork + Mentality + ConditionFitness 
     + Height + Age + Weight, test = 'F')
### A inclusão da variável Teamwork produziu maior valor para a estatística
### F (e p-valor extremamente baixo) e será incluída ao modelo.

### Segue o processo.

ajuste3 <- lm(LongPassAccuracy ~ TopSpeed + Teamwork, data = dados)
add1(ajuste3, scope=~Agility + Acceleration  + TopSpeed
     + Balance + Jump + Teamwork + Mentality + ConditionFitness 
     + Height + Age + Weight, test = 'F') ### Entra Jump.

ajuste4 <- lm(LongPassAccuracy ~ TopSpeed + Teamwork + Jump, data = dados)
add1(ajuste4, scope=~Agility + Acceleration  + TopSpeed
     + Balance + Jump + Teamwork + Mentality + ConditionFitness 
     + Height + Age + Weight, test = 'F') ### Entra Balance.

ajuste5 <- lm(LongPassAccuracy ~ TopSpeed + Teamwork + Jump + Balance, data = dados)
add1(ajuste5, scope=~Agility + Acceleration  + TopSpeed
     + Balance + Jump + Teamwork + Mentality + ConditionFitness 
     + Height + Age + Weight, test = 'F') ### Entra Agility.

ajuste6 <- lm(LongPassAccuracy ~ TopSpeed + Teamwork + 
                Jump + Balance + Agility, data = dados)
add1(ajuste6, scope=~Agility + Acceleration  + TopSpeed
     + Balance + Jump + Teamwork + Mentality + ConditionFitness 
     + Height + Age + Weight, test = 'F') ### Entra Height.

ajuste7 <- lm(LongPassAccuracy ~ TopSpeed + Teamwork + 
                Jump + Balance + Agility + Height, data = dados)
add1(ajuste7, scope=~Agility + Acceleration  + TopSpeed
     + Balance + Jump + Teamwork + Mentality + ConditionFitness 
     + Height + Age + Weight, test = 'F') 

### As variáveis que ainda não foram incluídas no modelo não apresentam
### efeito significativo, segundo o teste F. O processo é encerrado
### com o modelo atual, sem adicionar novas covariáveis.

########################################################################
### Agora usando a função step do R.

### Seleção backward.
mod_back <- step(ajuste, direction = 'backward', test = 'F')

### Seleção forward
mod_for <- step(ajuste0, scope=~Agility + Acceleration  + TopSpeed
                + Balance + Jump + Teamwork + Mentality + ConditionFitness 
                + Height + Age + Weight, direction = 'forward', test = 'F')

### Seleção stepwise.
mod_step <- step(ajuste, direction = 'both', test = 'F')

compareCoefs(mod_back, mod_for, mod_step)

### Os três métodos produziram o mesmo modelo (mesmo conjunto de covariáveis
### selecionadas). Isso nem sempre acontece, no entanto possível obter
### modelos diferentes usando métodos de seleção diferentes.


########################################################################
### Análise baseada em critérios de qualidade de ajuste.

help("regsubsets")

all_reg <- regsubsets(LongPassAccuracy ~ Agility + Acceleration  + TopSpeed
                      + Balance + Jump + Teamwork + Mentality + ConditionFitness 
                      + Height + Age + Weight, method = "exhaustive",
                      nvmax = 11, data = dados)

### A função regsubsets vai ajustar todos os modelos de regressão possíveis,
### e armazenar os valores dos critérios de qualidade para os melhores ajustes
### com j = 1, j = 2, ..., j = k covariáveis.

plot(all_reg)
### BICs para os modelos ótimos para cada número de covariáveis.

s1 <- summary(all_reg, matrix.logical=TRUE)
s1

### Cada linha da matriz lógica apresenta o melhor modelo para um particular
### número de covariáveis. Neste caso TRUE indica que a variável é incluída
### no modelo e FALSE que ela não é incluída.

### Assim, a título de exemplo, o melhor modelo com uma covariável tem 
### "TopSpeed" como regressora; o melhor modelo com duas covariáveis é
### ajustado por "TopSpeed" e "Teamwork" e assim por diante.

### Como nesse primeiro momento apenas são comparados modelos com igual
### número de parâmetros, qualquer critério de qualidade de ajuste vai 
### indicar a seleção do mesmo modelo.

### Para comparar a sequência de modelos obtidos para diferentes números de
### covariáveis, podemos recorrer aos critérios de qualidade de ajuste 
### estudados.

s1$rsq ### R2
s1$adjr2 ### R2 ajustado
s1$cp ### Cp de Mallows
s1$bic ### BIC

which.max(s1$adjr2) 
### O modelo com seis covariáveis produziu maior valor de R2 ajustado.

which.min(s1$bic) 
### O modelo com seis covariáveis produziu menor valor de BIC.

which.max(s1$rsq) 
### O modelo com onze covariáveis produziu menor valor de R2 (obviamente).

### Vamos produzir gráficos.
n_cov <- 1:11

plot(n_cov, s1$rsq, type = 'b', xlab = 'Número de covariáveis', ylab = 'R2', las = 1, pch = 20)
plot(n_cov, s1$adjr2, type = 'b', xlab = 'Número de covariáveis', ylab = 'Adjusted R2', las = 1, pch = 20)
plot(n_cov, s1$bic, type = 'b', xlab = 'Número de covariáveis', ylab = 'BIC', las = 1, pch = 20)


### Agora a análise do Cp de Mallows.
plot(n_cov, s1$cp, xlab = 'Número de covariáveis', ylab = 'BIC', las = 1, pch = 20)
abline(0,1)
### Os elevados valores de Cp para modelos com uma a três covariáveis 
### dificulta a visualização dos resultados.

plot(n_cov[3:11], s1$cp[3:11], xlab = 'Número de covariáveis', ylab = 'Cp', las = 1, pch = 20)
abline(0,1)
### O modelo com menor Cp próximo à reta identidade é o modelo com seis
### covariáveis.

########################################################################

### Agora vamos repetir a análise usando os algoritmos backward, forward e 
### stepwise com base na minimização dos critérios AIC e BIC. Primeiro
### usando AIC (k=2 é a constante de penalização).

### Seleção backward, critério AIC.
mod_back <- step(ajuste, direction = 'backward', k = 2)
summary(mod_back)

### Seleção forward, critério AIC.
mod_for <- step(ajuste0, scope=~Agility + Acceleration  + TopSpeed
                + Balance + Jump + Teamwork + Mentality + ConditionFitness 
                + Height + Age + Weight, direction = 'forward', k = 2)
summary(mod_for)


### Seleção stepwise, critério AIC.
n <- nrow(dados)
mod_step <- step(ajuste, direction = 'both', k = 2)
summary(mod_step)

### usando BIC (k=log(n) é a constante de penalização).

### Seleção backward, critério BIC.
mod_back <- step(ajuste, direction = 'backward', k = log(n))
summary(mod_back)

### Seleção forward, critério BIC.
mod_for <- step(ajuste0, scope=~Agility + Acceleration  + TopSpeed
                + Balance + Jump + Teamwork + Mentality + ConditionFitness 
                + Height + Age + Weight, direction = 'forward', k = log(n))
summary(mod_for)

### Seleção stepwise, critério BIC.
mod_step <- step(ajuste, direction = 'both', k = log(n))
summary(mod_step)