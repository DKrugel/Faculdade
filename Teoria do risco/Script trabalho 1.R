x2 <- c(0,1,2,6,7,12)
px2x <- c("16/36","8/36","1/36","8/36","2/36","1/36")
px2mx <- c(0.44,0.66,0.69,0.916,0.972,1)

df2 <- data.frame(x2, px2x, px2mx)
colnames(df2) <- c("x","P[X2 = x]","P[X2 <= x]")

px2xV2 <- c(16/36,8/36,1/36,8/36,2/36,1/36)
Ex2 <- sum(df2$x * px2xV2)

# #Resposta do ex
# O Xc equivale ao preço pago pelos 2 jogadores para que a banca não quebre com probabilidade epsolon, portanto
# o Xc que nos dará uma chance aceitável da banca se manter aberta (1 - E) é de 6 unidades monetárias, dando 3 unidades para cada jogador 

plot(px2mx,
     main = "Probabilidade acumulada de P[X2 < x]",
     ylab = "Probabilidade",
     xlab = "Valor de x",
     type = "s")

# Calculo da variância de x2

SegMomento2 <- sum((df2$x2^2)*px2xV2)

Varx2 <- SegMomento2 - (Ex2^2)

# Calculo para 3 jogadores

# Pelo teorema da convolução sabemos que a soma de 3 jogadores irá recair na soma de 2 variáveis aleatórias, sendo elas Z3 e (Z1 + Z2)
# Portanto aplicando essa ideia ao Teorema de Fourier teremos:

z3 <- c(0,1,6)
z1z2 <- x2

df3V1 <- data.frame("0" = px2xV2*4/6, "1" = px2xV2*1/6, "6" = px2xV2*1/6)
row.names(df3V1) <- x2
colnames(df3V1) <- z3
df3V1 #Tabela com valores de fourier z3 X (z1 + z2)

# Acumulada de z3

x3 <- c(0,1,2,3,6,7,8,12,13,18)
px3x <- c("64/216","48/216","12/216","1/216","48/216","24/216","3/216","12/216","3/216","1/216")
px3mx <- c(0.2962, 0.5185, 0.5740, 0.5786, 0.80,0.91,0.92,0.9814,0.995,1)
px3xV2 <- c(64/216,48/216,12/216,1/216,48/216,24/216,3/216,12/216,3/216,1/216)

df3V2 <- data.frame(x3, px3x, px3xV2, px3mx)
colnames(df3V2) <- c("x","P[X3 = x]","P[X3 = x] calculado","P[X3 <= x]")

Ex3 <- sum(df3V2$x * px3xV2)

plot(px3mx,
     main = "Probabilidade acumulada de P[X3 < x]",
     ylab = "Probabilidade",
     xlab = "Valor de x",
     type = "l")

# Calculo da variância para 3 jogadores

SegMomento3 <- sum((df3V2$x^2)*px3xV2)

Varx3 <- SegMomento3 - (Ex3^2)

# Resposta para 3 jogadores:
# 
# Seguindo a mesma lógica usada para 2 jogadores o Xc que me dará um valor de P[xc <= 1 - E] é aproximadamente 7. Dando um valor para cada jogador de aproximadamente 7/3 = 2,33


# Para 4 jogadores
# 
# ! Atenção !
#   
# Como tive pouco tempo para me planejar para a realização deste trabalho, há um erro nas probabilidades de 4 jogadores, alguma das combinações de probabilidades foi deixada de lado, por conta disso os calculos estão incorretos e a acumulada não resulta em 1 quando somado todos os elementos da sigma algebra.

z4 <- c(0,1,6)
z1z2z3 <- x3

df4V1 <- data.frame("0" = px3xV2*4/6, "1" = px3xV2*1/6, "6" = px3xV2 * 1/6)
rownames(df4V1) <- x3
colnames(df4V1) <- z4

x4 <- c(0,1,2,3,4,6,7,8,9,12,13,14,18,19,24)
px4x <- c("256","256","96","16","1","256","192","48","4","96","24","3","16","1","1")
bazinga <- rep("/1296", 15)
px4x <- paste0(px4x, bazinga)
px4xV2 <- c("256","256","96","16","1","256","192","48","4","96","24","3","16","1","1")
px4xV2<- as.numeric(px4xV2)/1296

px4mx <- c(0.1975, 0.3950,0.4691,0.4814,0.4822,0.6797,0.8279,0.8640,0.8680,0.9421,0.9605,0.9629,0.9452,0.9760,0.9767)

df4V2 <- data.frame(x4, px4x, px4xV2, px4mx)
colnames(df3V2) <- c("x","P[X4 = x]","P[X4 = x] calculado","P[X4 <= x]")


# Resposta
# Utilizando a mesma lógica que as questôes anteriores o X crítico é de 12, com uma probabilidade de ruína de (1-0,94) 0.06, tendo como custo unitário para cada jogador algo em torno de 12/4 = 3 unidades monetárias 


plot(px4mx,
     main = "Probabilidade acumulada de P[X4 < x]",
     ylab = "Probabilidade",
     xlab = "Valor de x",
     type = "l")
# 
# Para o calculo da esperança e da variância continuarei a utilizar teoria de probabilidade e fare apartir do primeiro e do segundo momento:

Ex4 <- sum(df4V2$x * px4xV2)

SegMomento4 <- sum((df4V2$x^2)*px4xV2)

Varx4 <- SegMomento4 - (Ex4^2)

ft <- qflextable(head(cars))
nrow_part(ft, part = "body")
