########################################################################
### Exemplo - quase-verossimilhança.
### Incidência de um tipo de mancha observada em folhas da cevada. A variável
### resposta é a proporção da área foliar afetada pela mancha (blotch), tratando-se,
### portanto, de uma proporção contínua. As variáveis explicativas são a
### variedade da cevada (variety, fator com 10 níveis) e o local de plantio 
### (site, fator com 9 níveis). Os dados foram extraídos de MccCullagh e Nelder, 1989.

cevada <- read.table('http://docs.ufpr.br/~taconeli/CE22518/cevada.txt', header = T)
### Se o carregamento dos dados não funcionar, salve o arquivo txt em sua 
### máquina e importe localmente.

require(gnm)
require(car)
require(hnp)

summary(cevada)

names(cevada) <- c('Obs', 'Area', 'local', 'variedade')

cevada$variedade <- factor(cevada$variedade)
cevada$local <- factor(cevada$local)

with(cevada, boxplot(Area ~ variedade, xlab = 'Variedade de cevada', 
                     ylab = 'Proporção da área afetada'))
with(cevada, boxplot(Area ~ local, xlab = 'Local de plantio', 
                     ylab = 'Proporção da área afetada'))

### Vamos ajustar um modelo de quasi-verossimilhança para a proporção da
### área infectada. Vamos usar função de ligação logito log(p/(1-p))
### e função de variância v(p) = phi*p*(1-p)

ajuste1 <- glm(Area ~ local + variedade, family = quasi(link = 'logit', variance = "mu(1-mu)"),
               data = cevada)
summary(ajuste1)
plot(predict(ajuste1), rstandard(ajuste1, type = 'pearson'), pch = 20,
     main = 'V(mu)=mu(1-mu)', xlab = 'Fitted values', ylab = 'Residuals')

### A relação média variância não é constante, sendo que ela parece aumentar
### conforme a média e diminuir novamente Podemos experimentar
### uma nova função de variância, dada pelo quadrado da primeira V(mu) = mu^2 * (1-mu)^2.
### Essa função de ligação está implementada no pacote gnm, sob o nome wedderburn.

ajuste2 <-glm(Area ~ local + variedade, family = wedderburn, data = cevada)
summary(ajuste2)

par(mfrow = c(1,2))
plot(predict(ajuste1), rstandard(ajuste1, type = 'pearson'), pch = 20, 
     main = 'V(mu)=mu(1-mu)', xlab = 'Fitted values', ylab = 'Residuals')
plot(predict(ajuste2), rstandard(ajuste2, type = 'pearson'), pch = 20, 
     main = 'V(mu)=mu2(1-mu)^^2', xlab = 'Fitted values', ylab = 'Residuals')

influenceIndexPlot(ajuste1, vars=c('Studentized','Cook','Hat'))
influenceIndexPlot(ajuste2, vars=c('Studentized','Cook','Hat'))

### O modelo com função de variância V(mu) = mu^2 * (1-mu)^2, aparentemente,
### proporciona melhor ajuste. Embora algum cuidado seja necessário
### quanto a pontos potencialmente influentes.


### Observe que a estimativa do parâmetro de dispersão, neste segundo ajuste,
### é muito próxima de 1, indicando que a função de variância proposta absorve
### praticamente toda a variação dos dados.
