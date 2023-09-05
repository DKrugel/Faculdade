set.seed(156)
a)
s = c(rep(0,100), 10*exp(-(1:100)/20)*cos(2*pi*(101:200)/4))
x = s + rnorm(200)
plot.ts(x) 

b)
sb = c(rep(0,100), 10*exp(-(1:100)/200)*cos(2*pi*(101:200)/4))
xb = s + rnorm(200)
plot.ts(xb) 

a1 <- plot.ts(x, col = "red", axes = F)
b1 <- plot.ts(xb, col = "black", axes = F)

par(new = TRUE)
par(new = F)

# Após o início da parcela exclusivamente aleatória, a alteração do modulador não alterou tanto a amplitude do sinal.

sc = c(rep(0,100), 10*exp((1:100)/200)*cos(2*pi*(101:200)/4))
xc = s + rnorm(200)
plot.ts(xc, col = "blue") 
par(new = TRUE)
plot.ts(xb)
# 
# A alteração do sinal não alterou a informação da série, algo que cogitei que poderia acontecer seria a inversão da série, porém acredito que isso só aconteceria caso opuvesse mudança na função coseno para a seno

