library(tidyverse)
library(lattice)
library(plot3D)
library(gtsummary)
library(car)
library(gt)

cpi <- tibble(Sacarose = c(-1,1,-1,1,0,0,0,-sqrt(2),sqrt(2),0,0),
              Inoculo = c(-1,-1,1,1,0,0,0,0,0,-sqrt(2),sqrt(2)),
              Conversao = c(70.77,72.57,52.8,47.42,54.1,50.65,47.8,74.06,55.01,76.14,56.09))

ajuste_1 <- lm(Conversao ~ Sacarose + Inoculo + Sacarose*Inoculo + I(Sacarose^2) + I(Inoculo^2), data = cpi)

summary(ajuste_1)

anova(ajuste_1)

# Malha fina de valores para predição.
pred <- expand.grid(Sacarose = seq(-sqrt(2), sqrt(2), by = 0.1),
                    Inoculo = seq(-sqrt(2), sqrt(2), by = 0.1),
                    KEEP.OUT.ATTRS = FALSE)
pred <- cbind(pred,
              as.data.frame(predict(ajuste_1,
                                    newdata = pred,
                                    se.fit = TRUE)[1:2]))

# Gráfico da superfície média (valores ajustados).
ggplot(data = pred,
       mapping = aes(x = Sacarose, y = Inoculo, z = fit, fill = fit)) +
  geom_tile() +
  scale_fill_distiller(palette = "Spectral",
                       direction = -1) +
  geom_contour(color = "black") +
  coord_equal()

wireframe(fit ~ Sacarose + Inoculo, data = pred, drape = TRUE)

coeficientes <- ajuste_1$coefficients

M <- mesh(seq(-sqrt(2),sqrt(2), by = 0.01),
          seq(-sqrt(2),sqrt(2), by = 0.01))

Conversao <- coeficientes[1]+coeficientes[2]*M$x+coeficientes[3]*M$y+coeficientes[4]*M$x^2+coeficientes[5]*M$y^2+coeficientes[6]*M$x*M$y

surf3D(M$x,M$y,Conversao,phi = 30, theta = 135, bty = "b2", 
       lighting = TRUE, ltheta = 40, lphi = 0,
       xlab = "Sacarose", ylab = "Inóculo", zlab = "Conversão")

# Interatividade

library(plot3D)
library(plot3Drgl)

coeficientes <- ajuste_1$coefficients

M <- mesh(seq(-sqrt(2),sqrt(2), by = 0.01),
          seq(-sqrt(2),sqrt(2), by = 0.01))

Conversao <- coeficientes[1]+coeficientes[2]*M$x+coeficientes[3]*M$y+coeficientes[4]*M$x^2+coeficientes[5]*M$y^2+coeficientes[6]*M$x*M$y

g1 <- surf3D(M$x,M$y,Conversao,phi = 30, theta = 135, bty = "b2", 
             lighting = TRUE, ltheta = 40, lphi = 0,
             xlab = "Sacarose", ylab = "Inoculo", zlab = "Conversao")

plotrgl()

tbl_regression(ajuste_1) %>% 
  add_global_p()

nova <- Anova(ajuste_1)

tb1 <- as.data.frame(nova) %>% 
          gt(rownames_to_stub = TRUE)
tb1 %>% 
  tab_header(
    title = "Tabela Anova",
    subtitle = 'Sequêncial')
  