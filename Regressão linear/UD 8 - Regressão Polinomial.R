########################################################################
### UD8- Regressão polinomial

options(device = 'x11')

### Vamos simular alguns dados para um par de variáveis x e y.

set.seed(88) ### Setando a semente.
x <- runif(200, 0, 6) ### Variável explicativa
y <- sin(x) + 1 + rnorm(200, 0, 0.5) ### Variável resposta.
plot(x, y, col = 'blue', cex = 1.2, las = 1)
newx <- data.frame(x = seq(min(x), max(x), length.out = 100)) 
### Dados para posterior predição.

########################################################################
### Vamos ajustar modelos polinomiais seguindo uma estratégias dos tipos
### forward e backward. 

### Método forward.

### Modelo nulo (só com intercepto)
ajuste0 <- lm(y ~ 1) 
lines(newx$x, predict(ajuste0, newdata = newx), col = 'red', lwd = 3, lty = 2)

### Modelo com termo linear
ajuste1 <- lm(y ~ x) 
summary(ajuste1)
### O termo linear é significativo, vamos incorporá-lo no modelo.
lines(newx$x, predict(ajuste1, newdata = newx), col = 'blue', lwd = 3)

### Modelo com termo quadrático
ajuste2 <- lm(y ~ x + I(x^2)) 
summary(ajuste2)
### O termo quadrático não é significativo. Pela estratégia forward o modelo
### com termo linear seria escolhido.
lines(newx$x, predict(ajuste2, newdata = newx), col = 'darkgreen', lwd = 3, lty = 2)

### Método backward. Vamos supor que, buscando um modelo parcimonioso, vamos
### fixar o polinômio de ordem 4 como o mais complexo a ser avaliado.

### Modelo polinomial de ordem 4:
ajuste4 <- lm(y ~ x + I(x^2) + I(x^3) + I(x^4)) 
summary(ajuste4)
### O termo de ordem 4 é não significativo e será removido do modelo.
lines(newx$x, predict(ajuste4, newdata = newx), col = 'purple', lwd = 3, lty = 2)

### Modelo com termo cúbico:
ajuste3 <- lm(y ~ x + I(x^2) + I(x^3)) 
summary(ajuste3)
### O termo de maior ordem (cúbico) é significativo. Então o processo se encerra
### com a seleção do modelo com efeito cúbico de x.
lines(newx$x, predict(ajuste3, newdata = newx), col = 'orange', lwd = 3)
### O termo de ordem 4 é não significativo e será removido do modelo.

### Observe que os dois métodos de seleção conduziram à escolha de modelos diferentes.
### Vamos compara os ajustes dos dois modelos usando o teste F.

anova(ajuste1, ajuste3)
### Como a variação na soma de quadrados de resíduos é significativa, o teste
### dá suporte ao modelo com efeito cúbico de x.

### Agora, vamos usar polinomios ortogonais. Partimos para o ajuste do modelo
### maximal (ordem 4)

ajuste_ort <- lm(y ~ poly(x,4))
summary(ajuste_ort)

### Novamente a indicação é pelo ajuste do modelo com efeito cúbico de x, 
### uma vez que o efeito de ordem 4 não é significativo.

########################################################################
### Vamos eliminar os termos de menor ordem e ver o impacto no ajuste.
### Removendo o intercepto.
ajuste5 <- lm(y ~ x + I(x^2) + I(x^3)-1) 
summary(ajuste5)

### Plotagem
plot(x, y, col = 'blue', cex = 1.2, las = 1)
newx <- data.frame(x = seq(0, max(x), length.out = 100))
lines(newx$x, predict(ajuste5, newdata = newx), col = 'red', lwd = 3, lty = 2)

### Removendo o termo linear
ajuste6 <- lm(y ~ I(x^2) + I(x^3)) 
summary(ajuste6)

### Plotagem
plot(x, y, col = 'blue', cex = 1.2, las = 1)
newx <- data.frame(x = seq(0, max(x), length.out = 100))
lines(newx$x, predict(ajuste6, newdata = newx), col = 'red', lwd = 3, lty = 2)
### Discussões.

#########################################################################
### Agora um exemplo de regressão polinomial em duas variáveis

### Vamos usar a base de dados sat para esta aplicação. Tomemos como resposta
### a pontuação média dos alunos dos 50 estados no SAT (total) e como 
### variáveis explicativas o salário médio dos professores do ensino primário
### (salary) e a porcentagem de alunos elegíveis que realizaram o SAT (takers).

### Modelo 1
ajuste_sat <- lm(total ~ takers + salary + I(takers^2) + I(salary^2) + takers:salary, data = sat)
summary(ajuste_sat)

### Vamos criar um grid de valores para predição.
takers_pred <- seq(min(sat$takers), max(sat$takers), length.out = 10)
salary_pred <- seq(min(sat$takers), max(sat$takers), length.out = 10)
pgrid <- expand.grid(takers = takers_pred, salary = salary_pred)
### Figuras mais adiante.

### Apenas o termo quadrático da variável takers apresentou significância.
### Apenas a título de ilustração, vamos plotar os ajustes de diferentes
### modelos quadráticos aplicados a esta base.

### Modelo 2
ajuste_sat2 <- lm(total ~ takers + salary + I(takers^2) + I(salary^2), data = sat)
summary(ajuste_sat2)
### Modelo sem o termo de interação, mas com efeito quadrático para ambas as
### covariáveis.

### Modelo 3
ajuste_sat3 <- lm(total ~ takers + salary + I(takers^2), data = sat)
summary(ajuste_sat3)
### Modelo sem o termo de interação, mas com efeito quadrático apenas para
### takers.

### Modelo 4
ajuste_sat4 <- lm(total ~ takers + salary, data = sat)
summary(ajuste_sat4)
### Modelo com efeitos lineares.

### Modelo 5
ajuste_sat5 <- lm(total ~ takers * salary, data = sat)
summary(ajuste_sat5)


### Agora, as figuras
### Modelo 1
pred <- predict(ajuste_sat, pgrid)
persp(takers_pred, salary_pred, matrix(pred, 10, 10), xlab = 'Takers',
      ylab = 'Salary', zlab = 'SAT', theta = 45, shade = 0.30,
      ticktype = 'detailed', col = 'lightblue')

### Modelo 2
pred <- predict(ajuste_sat2, pgrid)
persp(takers_pred, salary_pred, matrix(pred, 10, 10), xlab = 'Takers',
      ylab = 'Salary', zlab = 'SAT', theta = 45, shade = 0.30,
      ticktype = 'detailed', col = 'lightblue')

### Modelo 3
pred <- predict(ajuste_sat3, pgrid)
persp(takers_pred, salary_pred, matrix(pred, 10, 10), xlab = 'Takers',
      ylab = 'Salary', zlab = 'SAT', theta = 45, shade = 0.30,
      ticktype = 'detailed', col = 'lightblue')

### Modelo 4
pred <- predict(ajuste_sat4, pgrid)
persp(takers_pred, salary_pred, matrix(pred, 10, 10), xlab = 'Takers',
      ylab = 'Salary', zlab = 'SAT', theta = 45, shade = 0.30,
      ticktype = 'detailed', col = 'lightblue')


### Modelo 5
pred <- predict(ajuste_sat5, pgrid)
persp(takers_pred, salary_pred, matrix(pred, 10, 10), xlab = 'Takers',
      ylab = 'Salary', zlab = 'SAT', theta = 45, shade = 0.30,
      ticktype = 'detailed', col = 'lightblue')