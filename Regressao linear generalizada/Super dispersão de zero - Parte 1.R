# Este código é de autoria de César Augusto Taconelli

require(car) 
require(MASS)
require(lme4)
require(hnp)
require(effects)
require(geepack)

help("Ornstein")
data(Ornstein)
head(Ornstein) 
summary(Ornstein) 

### Variáveis: assets: ativos da corporação (em milhões de dolares);
# sector: setor de operação (AGR: agriculture; TRN: transportes; MIN: mineração, metais,...
# nation: CAN: Canadá; UK: Reino Unido; US: Estados Unidos; OTH: outro.
# interlocks: Número de diretores e executivos compartilhados com outras companhias.

options(device = 'x11')

### Objetivo: modelar interlocks em função das demais variáveis.

### Alguns gráficos.
boxplot(Ornstein$assets) 
### Distribuição dos ativos extremamente assimétrica, com algumas companhias 
### bastante discrepantes (superiores, em relação aos ativos) em relação às demais.

boxplot(log(Ornstein$assets)) 
### Utilizar o logaritmo simetriza a distribuição dos ativos. Com base 
### nisso, vamos usar log(assets) no modelo.

plot(log(Ornstein$assets),Ornstein$interlocks,pch=20,
     ylab='Número de diretores compartilhados',xlab='Ativos (em milhões de dolares)')
boxplot(Ornstein$interlocks~Ornstein$nation,xlab='País',
        ylab='Número de diretores compartilhados')
boxplot(Ornstein$interlocks~Ornstein$sector,xlab='Setor',
        ylab='Número de diretores compartilhados')

### Modelo loglinear Poisson.
ajuste1 <- glm(interlocks~log(assets)+nation+sector, family=poisson, data=Ornstein)
summary(ajuste1) 
### Repare que a deviance residual excede muito o respectivo número de 
### graus de liberdade (1547>>>234). Este é um indicativo de falta de ajuste.

par(mfrow=c(2,2))
plot(ajuste1)
### Atentem para a escala dos resíduos, com valores acima de 5 e abaixo de -5!!!

hnp(ajuste1) ### Claramente o modelo não se ajusta aos dados.

### Estimando o parâmetro de dispersão com base na estatística X2 de Pearson:
phi <- sum(rstandard(ajuste1, type='pearson')**2)/234; phi ### Muito maior que 1.
summary(ajuste1, dispersion = phi)
### Perceba que os erros padrões estão corrigidos (multiplicados pela raiz de phi).
### Já corresponde aos resultados de um modelo quase poisson.
sqrt(phi) ### Os erros padrões ficam multiplicados por 2.607.

### Como alternativa, vamos ajustar um modelo de regressão com mesma 
### estrutura, mas distribuição binomial negativa.

ajuste2 <- glm.nb(interlocks ~ log(assets) + nation + sector, data=Ornstein)
summary(ajuste2) 
### Repare que o valor da deviance residual, nesse caso, 
### é bem mais próximo do respectivo número de graus de liberdade.

par(mfrow=c(2,2))
plot(ajuste2)
hnp(ajuste2)


### Vamos comparar os modelos de regressão Poisson e binomial negativa 
### por meio dos AICs:

AIC(ajuste1, ajuste2)
# O modelo com melhor ajuste (menor AIC) é o binomial negativo. Vamos seguir com 
# ele.

compareCoefs(ajuste1, ajuste2)
### Observe a diferença dos erros padrões produzidos pelos ajustes dos modelos
### Poisson e binomial negativo. Os erros produzidos pela Poisson estão
### subestimados.

confint(ajuste1)
confint(ajuste2)
### Os intervalos de confiança baseados na Poisson são excessivamente precisos,
### comprometendo a taxa de cobertura.

Anova(ajuste2) 
### Pelo resultado do teste, o efeito de setor é não significativo (quando 
### o número de diretores é ajustado por país e ativos). 
### Vamos eliminar essa variável do modelo.

ajuste3 <- update(ajuste2, ~.-sector)
summary(ajuste3)

### O resumo do ajuste indica maior frequência de diretores de empresas
### norte-americanas em relação às canadenses (referência).
### Além disso, a frequência (média) de diretores aumenta conforme os
### ativos da empresa.

### Vamos testar a igualdade da frequência média de diretores das empresas
### que não são norte-americanas.

Ornstein$IndicaUSA <- factor(ifelse(Ornstein$nation=='US','USA','NãoUSA')) 
ajuste4 <- glm.nb(interlocks ~ log(assets)+IndicaUSA,Ornstein)

### Agora, vamos testar a restição imposta comparando os modelos 3 e 4.
anova(ajuste3, ajuste4) ### A diferença de deviances não é significativa. 
summary(ajuste4)

### A frequência de diretores compartilhados é inferior nos Estados Unidos 
### em relação às demais localidades estudadas. 
### Estima-se que o número médio de diretores compartilhados nos EUA
### seja aproximadamente a metade, em relação às demais localidades (exp(-0,68)).  

### Vamos ajustar um modelo por quasi-verossimilhança, definido pela função 
### de ligação logaritmica e função de variância V(mu)=phi*mu (modelo quasi-Poisson)

ajuste6 <- glm(interlocks ~ log(assets)+IndicaUSA,family=quasi(link='log',variance='mu'),Ornstein)
summary(ajuste6)

### Declaração alternativa:

ajuste6_alt <- glm(interlocks ~ log(assets)+IndicaUSA,family=quasipoisson, Ornstein)
summary(ajuste6_alt)
compareCoefs(ajuste6, ajuste6_alt)

### Vamos tentar extrair a logverossimilhança maximizada para esse modelo:
logLik(ajuste6) ### Como não especificamos completamente uma distribuição paramétrica, não temos uma verossimilhança.

Anova(ajuste6) ### Os testes de hipóteses baseiam-se na função quase-desvio 
### e na diferença entre funções quasi-desvio para modelos encaixados que, segundo 
### McCullagh(1983), funciona como o teste da razão de verossimilhanças.

ajuste7 <- glm(interlocks~log(assets)+IndicaUSA,family = poisson,Ornstein) 
### Modelo de poisson com preditor semelhante aos modelos binomial negativo
### e de quase-verossimilhança.

ajuste8 <- glm(interlocks ~ log(assets)+IndicaUSA,family=quasi(link='log',variance='mu^2'),Ornstein) 
### O ajuste8 também corresponde a um modelo de quase verossimilhança, mas 
### com função de variância quadrática (v(mu) = phi*mu^2).

### Vamos comparar os ajustes dos modelos com distribuição de Poisson, 
### binomial nehativa e por quasi verossimilhança. 
compareCoefs(ajuste7,ajuste4,ajuste6, ajuste8)

### Gráficos de resíduos de Pearson vs valores ajustados para os modelos 
### quasi-poisson (ajuste6) e Poisson (ajuste7). Repare na escala dos resíduos.

par(mfrow=c(1,2),las=1)
plot(fitted(ajuste6),rstudent(ajuste6,type='pearson'),xlab='Preditor ajustado',
     ylab='Resíduo padronizado de Pearson',main='Modelo quasi Poisson')
plot(fitted(ajuste7),rstudent(ajuste7,type='pearson'),xlab='Preditor ajustado',
     ylab='Resíduo padronizado de Pearson',main='Modelo Poisson')

### Um pouco mais de diagnóstico:
influenceIndexPlot(ajuste7,vars=c('Studentized','Cook','Hat'),id.n=3)
influenceIndexPlot(ajuste6,vars=c('Studentized','Cook','Hat'),id.n=3)
### Pode-se observar menor, novamnet, redução na escala dos resíduos e valores 
### consideravelmente menores para as distâncias de Cook para o ajuste6.

### Vamos explorar o efeito da quantidade de ativos.
plot(allEffects(ajuste4), type = 'response')

### E se usarmos o modelo Poisson com estimação por Bootstrap?

ajusteboot <- Boot(ajuste7, R=999)
### Observe que usando bootstrap (NÃO PARAMÉTRICO) também contornamos o problema
### da superdispersão, com erros padrões próximos aos obtidos usando quase
### verossimilhança e a distribuição binomial negativa.


confint(ajusteboot) ### Intervalos de confiança bootstrap.
confint.default(ajuste6) ### Intervalos de confiança baseados no modelo de QL
confint.default(ajuste7) ### Intervalos de confiança baseados no MLG Poisson

### Os ICs baseados no MLG são incorretamete precisos, em relação aos demais.
### Ver estudo de simulação em arquivo a parte.

### Agora, vamos ajustar o modelo quase poisson com estimação robusta dos
### erros padrões (estimador sanduíche). Para isso, vamos usar a biblioteca 
### geepack.

Ornstein$Subj <- 1:nrow(Ornstein)
ajuste9 <- geeglm(interlocks ~ log(assets) + IndicaUSA, family = 'poisson', id = Subj, data=Ornstein)
summary(ajuste9)
