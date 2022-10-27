temp <- c(-5,-4,-3,-2,-1,0,1,2,3,4,5)
prod <- c(1,5,4,7,10,8,9,13,14,13,18)
dados <- data.frame(temp,prod)

ajuste <- lm(prod ~ temp, data = dados)
summary(ajuste)
confint(ajuste)

confint(predict(ajuste))

require(car)
help("Prestige")
ajuste1 <-lm(prestige ~ education, data = Prestige)
summary(ajuste1)
# y = -10.732 + 5.361*x + E

data <- carData::Prestige
View(data)
plot(data[,4]~data[,1]); abline(coef(ajuste1), col = "red", lwd = 2)

predict(ajuste1, newdata = data.frame(education = c(12.5))) #56,27899
predict(ajuste1)[1]
residuals(ajuste1)[1]

coefficients(ajuste1)[2] #aumento previsto no escore com a adição de 1 ano de estudo
3*(coefficients(ajuste1)[2]) #aumento previsto para a adição de 3 anos de estudo

coefficients(ajuste1)[1] #-10.73198 não tem interpretação neste caso

anova(ajuste1)

predict(ajuste1, newdata = data.frame(education = c(9, 15, mean(data[,1])))) #37,51592; 69,68118; 46,83333

### Agora vamos adicionar ao gráfico as bandas de confiança e de predição.
predictions <- predict(ajuste1, interval = "predict")
Dados_completos <- cbind(data, predictions)
### Base com os dados originais e as predições (para construção do gráfico)

ggplot(data = Dados_completos, aes(x = education, y = prestige))+
  geom_point(size = 2, col = 'grey50')+
  theme_bw(base_size = 14)+
  xlab('Tempo de estudo')+
  ylab('Escore de Prestigio')+
  stat_smooth(method = lm) + #bandas de confiança
  geom_line(aes(y = lwr), col = "coral2", linetype = "dashed", size = 1) + #banda de predição inferior
  geom_line(aes(y = upr), col = "coral2", linetype = "dashed", size = 1) #banda de predição superior

library(labestData)
