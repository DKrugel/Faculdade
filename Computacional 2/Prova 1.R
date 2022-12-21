y <- 
  
  c(163.89, 146.98, 157.82, 140.4, 120.6, 169.3, 134.72, 163.85, 
    134.7, 165.64, 139.54, 136.61, 141.66, 153.12, 155.47, 132.85, 
    171.64, 160.79, 187.64, 161.54)

y_limiar <- 
  
  170


# Ajusta a distribuição Normal por máxima verossimilhança.
my_fit <- MASS::fitdistr(y, densfun = "normal")
my_fit$estimate

##      mean        sd 
## 151.93800  16.19844

plot(ecdf(y))
curve(pnorm(x, 
            mean = my_fit$estimate[["mean"]],
            sd = my_fit$estimate[["sd"]]),
      add = TRUE, col = "red")

# X ~ Normal(média = 151.938, sd = 16.1984)
set.seed(123)

1 - pnorm(170, mean = 151.9380, sd = 16.19844)

sd = 16.19844

x <- rnorm(10000, 151.9380, 16.19844)

prob170 <-sum(x >= 170)/length(x)

prob170 <- as.data.frame(prob170)

library(tidyverse)

prob170 %>% 
  mutate(upper = prob170 + (1.96*sd)/sqrt(length(x)), lower = prob170 - 1.96*sd/sqrt(length(x)))

