dat04 <- read.table("http://www.leg.ufpr.br/~paulojus/CE092/data04.txt", head=TRUE)
df <- data.frame(dat04)
pd <- data.frame(x = seq(0,3.5, length = 100))

fit01 <- lm(y~x, data = df)
pd$y1 <- predict(fit01, newdata =  pd)
with(pd,lines(y1 ~ x))

## Ajuste com uma Slop
plot(dat04)

c <- 2.5
pd <- transform(pd, x2 = ifelse(x < c, 0, x-c))
df <- transform(df, x2 = ifelse(x < c, 0, x-c))



fit02 <- lm(y ~ x + x2, data=df)

pd$y2 <- predict(fit02, pd)
with(pd, lines (y2 ~ x))


# Ajuste separado
df <- transform(df,b3 = ifelse(x < c, 0, x))
pd <- transform(pd,b3 = ifelse(x < c, 0, x))
fit03 <- lm(y~x + b3, data = df)


pd$y3 <- predict(fit03, pd)
with(df,plot(y~x))
subset(df$x<c, with(pd, lines(y3 ~x)))

