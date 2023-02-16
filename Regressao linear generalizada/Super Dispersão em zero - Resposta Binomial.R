########################################################################
### Regressão para dados binários grupados com super dispersão

### Dados: 

### Teratologia é o estudo de anormalidades do desenvolvimento fisiológico. 
### Alguns experimentos têm por objetivo investigar os efeitos de dietas
### ou agentes químicos no desenvolvimento fetal de ratos. Neste estudo,
### ratos (fêmeas) com deficiência de ferro divididos em quatro grupos.
### Os ratos do grupo 1 receberam injeções de placebo e, os ratos dos outros
### grupos, injeções de suplemento de ferro. As aplicações foram feitas 
### nos dias 7 e 10 para os ratos do grupo 2; nos dias 0 e 7 para os ratos
### do grupo 3, e semanalmente para os ratos dos grupo 4. Os 58 ratos foram
### sacrificados após três semanas e então o número de fetos
### mortos em cada ninhada foi contado, além do nível de hemoglobina da mãe. 
### Devido à variabilidade genética, a probabilidade de morte pode variar 
### entre ninhadas para uma mesma combinação de tratamento e nível de 
### hemoglobina.

### Se o carregamento dos dados não funcionar, salve o arquivo txt em sua 
### máquina e importe localmente.
require(hnp)
require(gamlss)
require(coefplot)
require(car)

options(device = 'x11')
# setwd("C:/Users/cetac/Dropbox/CE225_Oferta_2022/Graduacao/UDs/UD14")
dados <- read.csv2('Ratos.csv') 

### Vamos modelar a probabilidade de morte para um feto em função do tratamento
### (vamos considerar apenas controle vs tratado) ajustado pelo nível de
### hemoglobina. As variáveis são as seguintes:

### litter: indicador de ninhada;
### group: grupo (tratamento);
### h: nível de hemoglobina da mãe;
### n: número de fetos;
### s: número de fetos mortos.

dados$group <- as.factor(ifelse(dados$group == 1, 'Placebo', 'Treatment'))

### Ajuste do glm binomial:
ajuste_bin <- glm(cbind(s,n-s) ~ group + h, weights = n, family = 'binomial', data = dados)
plot(ajuste_bin)
hnp(ajuste_bin)
summary(ajuste_bin)

### Observe o valor da deviance residual bastante acima do correspondente
### número de graus de liberdade. Além disso, os resíduos fornecem
### forte evidência de que o modelo não está bem ajustado.


### Para ajustar o modelo beta binomial, implementado na biblioteca gamlss.
ajuste_bb <- gamlss(cbind(s,n-s) ~ group + h, family=BB, data=dados) 
summary(ajuste_bb)
plot(ajuste_bb)
hnp(ajuste_bb)


### Agora, usando abordagem de quase verossimilhança. Vamos usar com função
### de variância mu*(1-mu):
ajuste_qbin <- glm(cbind(s,n-s) ~ group + h, family = quasibinomial, data = dados)
summary(ajuste_qbin)
hnp(ajuste_qbin)

summary(ajuste_bin) ### Resumo do ajuste - modelo binomial.
summary(ajuste_bb) ### Resumo do ajuste - modelo beta-binomial.
summary(ajuste_qbin) ### Resumo do ajuste - modelo quase-binomial.

compareCoefs(ajuste_bin, ajuste_qbin)
multiplot(ajuste_bin, ajuste_qbin) ### Pacote coefplot

confint.default(ajuste_bin) ### Intervalos de confiança - modelo binomial.
confint.default(ajuste_bb) ### Intervalos de confiança - modelo beta-binomial.
confint.default(ajuste_qbin) ### Intervalos de confiança - quase-binomial.

