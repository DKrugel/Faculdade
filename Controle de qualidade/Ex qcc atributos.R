#-----------------------------------------------------------------------
# CE 219 - Controle Estatístico de Qualidade
#
#              Prof. Dr. Walmes M. Zeviani & Prof. Dr. Cesar A. Taconeli
#                                     Departamento de Estatística · UFPR
#-----------------------------------------------------------------------

library(qcc)

#-----------------------------------------------------------------------
# Grafico para a fração de não-conformes.

# Exemplo 4.1.
def <- c(6, 15, 0, 9, 5, 1, 4, 5, 7, 12, 7, 4, 1, 3, 6, 8, 10, 5, 2, 7)

# Com probabilidade de itens não conformes estimada.
qcc(data = def,
    type = "p",
    sizes = 100,
    xlab = "Número da amostra",
    ylab = "Fração de defeituosas",
    title = "")

# Probabilidade probabilidade de itens não conformes especificada (p =
# 0.05).
qcc(data = def,
    type = "p",
    sizes = 100,
    xlab = "Número da amostra",
    ylab = "Fração de defeituosas",
    title = "",
    center = 0.05)

# Vamos construir novos gráficos excluindo a segunda amostra.
qcc(data = def[-2],
    type = "p",
    sizes = 100,
    xlab = "Número da amostra",
    ylab = "Fração de defeituosas",
    title = "")

qcc(data = def[-2],
    type = "p",
    sizes = 100,
    xlab = "Número da amostra",
    ylab = "Fração de defeituosas",
    title = "",
    center = 0.05)

#-----------------------------------------------------------------------
# Gráfico de controle feito à mão.

n <- 100
p <- def/n
pm <- mean(p)
n_sigmas <- 3

lim <- pm + n_sigmas * c(-1, 1) * sqrt(pm * (1 - pm)/n)
lim[1] <- max(0, lim[1])

plot(p, type = "o", pch = 19, ylim = extendrange(c(p, lim)))
abline(h = c(pm, lim), lty = 2)

#-----------------------------------------------------------------------
# Grafico para a fração de não-conformes com tamanhos de amostras
# variáveis.

# Números de itens não conformes em m = 25 amostras (fase I).
x <- c(5, 7, 10, 8, 6, 8, 4, 8, 6, 12, 4, 5, 5, 8, 10, 12, 9, 7, 4, 8,
       12, 12, 10, 9, 12)

# Tamanhos das m = 25 amostras - fase I.
n <- c(rep(100, 5), rep(80, 10), rep(125, 10))

# Gráfico de controle.
qcc(x, sizes = n, type = "p")

# Tamanhos de novas amostras - fase II.
novo_n <- rep(60, 5)
novo_x <- c(5, 8, 12, 6, 13)

# Números de itens não conformes nas novas amostras - fase II.
qcc(x,
    sizes = n,
    type = "p",
    newdata = novo_x,
    newsizes = novo_n)

#-----------------------------------------------------------------------
# Gráfico de controle feito à mão.

p <- x/n
pm <- weighted.mean(p, w = n)
n_sigmas <- 3

me <- n_sigmas * sqrt(pm * (1 - pm)/n)
lim <- cbind(pm - me, pm + me)
lim[, 1] <- pmax(0, lim[, 1])

plot(p, type = "o", pch = 19, ylim = extendrange(c(p, lim)))
matlines(seq_along(p), lim, lty = 2, col = 1, type = "s")
abline(h = pm, lty = 2)

#-----------------------------------------------------------------------
# Curvas características de operação para o exemplo 1.

# Para n = 100.
# Com probabilidade estimada.
o1 <- oc.curves.p(qcc(data = def[-2],
                      type = "p",
                      sizes = 100,
                      xlab = "Número da amostra",
                      ylab = "Fração de defeituosas",
                      title = ""))

data.frame(round(o1, 3))

# Para n = 50.
# Com probabilidade estimada.
o2 <- oc.curves.p(qcc(data = def[-2],
                      type = "p",
                      sizes = 50,
                      xlab = "Número da amostra",
                      ylab = "Fração de defeituosas",
                      title = "",
                      center = 0.0537))

data.frame(round(o2, 3))

#-----------------------------------------------------------------------
# Curva característica de operação feita à mão.

erro_tipo_II <- function(p, n, p0, n_sigmas = 3) {
  # Limites baseados na Normal.
  sd_p0 <- sqrt(p0 * (1 - p0)/n)
  lim <- p0 + c(-1, 1) * n_sigmas * sd_p0
  # Probabilidades baseadas na binomial.
  X <- n * lim
  1 - (pbinom(X[1] + 1, size = n, prob = p, lower.tail = TRUE) +
         pbinom(X[2], size = n, prob = p, lower.tail = FALSE))
}
erro_tipo_II <- Vectorize(FUN = erro_tipo_II,
                          vectorize.args = c("p", "n", "p0"))

grid <- expand.grid(n = 50,
                    p = as.numeric(names(o2)),
                    p0 = 0.0537)
grid$err <- with(grid,
                 erro_tipo_II(p = p,
                              n = n,
                              p0 = p0,
                              n_sigmas = 3))

plot(err ~ p, data = grid, type = "l")

# Comparando os resultados.
round(cbind(grid$err, o2), 5)

#-----------------------------------------------------------------------
# Códigos - Tempo entre homicídios sucessivos.

tempo <- c(34, 2, 8, 5, 25, 33, 17, 4, 10, 9.25, 0.5, 5.25, 3, 11, 2, 1,
           17, 45, 13, 2, 7, 3, 4, 11, 14, 23, 33, 1)

# Distribuição claramente assimétrica, remete à distribuição binomial.
plot(density(tempo))
rug(tempo)

plot(ecdf(tempo))
rug(tempo)

qqnorm(tempo)
qqline(tempo)

MASS::boxcox(lm(tempo ~ 1))
abline(v = 0.277, col = "red")

# Vamos aplicar a transformação recomendada em aula.
tempo_t <- tempo^0.277

# Boa aproximação com a distribuição Normal.
plot(density(tempo_t))
rug(tempo_t)

plot(ecdf(tempo_t))
rug(tempo_t)

qqnorm(tempo_t)
qqline(tempo_t)

# Vamos ajustar gráficos de controle para observações individuais.
# Base usada para o gráfico das amplitudes móveis (MR).
dataMR <- cbind(tempo_t[2:length(tempo_t)],
                tempo_t[1:(length(tempo_t) - 1)])

qcc(dataMR, type = "R")
qcc(tempot, type = "xbar.one", std.dev = "MR")

# Não há indicativo de alteração no tempo médio entre homicídios no
# período analisado.

# Agora, gráficos de controle CUSUM e MMEP.
cusum(tempo_t, decision.interval = 4.7, se.shift = 1)
ewma(tempo_t, lambda = 0.1)

# Nenhum dos gráficos fornece evidência de alteração na média do
# processo.

#-----------------------------------------------------------------------
# Exercício - não conformidades em placas de circuitos impressos.

# Dados da fase 1.
amostraf1 <- c(21, 24, 16, 12, 15, 5, 28, 20, 31, 25, 20, 24, 16, 19, 10,
               17, 13, 22, 18, 39, 30, 24, 16, 19, 17, 15)

# Dados da fase 2.
amostraf2 <- c(16, 18, 12, 15, 24, 21, 28, 20, 25, 19, 18, 21, 16, 22,
               19, 12, 14, 9, 16, 21)

# Gráfico para o número de não conformidades.
qcc(amostraf1, type = "c")

# Dois pontos fora dos limites de controle. Vamos assumir que as causas
# tenham sido identificadas, eliminadas, e não tenham afetado qualquer
# outro resultado amostral. Assim, revisamos os limites eliminando os
# pontos.

qcc(amostraf1[-c(6, 20)], type = "c")

# Agora, parece que temos um conjunto de amostras adequadas para
# representar o processo sob controle. Vamos adicionar os dados da fase
# 2.

qcc(amostraf1[-c(6, 20)], type = "c", newdata = amostraf2)

#-----------------------------------------------------------------------
# Gráfico de controle feito à mão.

cm <- mean(amostraf1)
n_sigmas <- 3

lim <- cm + n_sigmas * c(-1, 1) * sqrt(cm)
lim[1] <- max(0, lim[1])

plot(amostraf1, type = "o", pch = 19,
     ylim = extendrange(c(amostraf1, lim)))
abline(h = c(cm, lim), lty = 2)

#-----------------------------------------------------------------------
# Curva característica de operação.

oc <- oc.curves.c(qcc(data = amostraf1,
                      type = "c",
                      sizes = 1,
                      center = cm))
oc

#-----------------------------------------------------------------------
# Curva característica de operação feita à mão.

erro_tipo_II <- function(c, n, c0, n_sigmas = 3) {
  # Limites baseados na Normal.
  sd_c0 <- sqrt(c0/n)
  lim <- c0 + c(-1, 1) * n_sigmas * sd_c0
  # Probabilidades baseadas na Poisson.
  ppois(floor(n * lim[2]), lambda = c) -
    ppois(floor(n * lim[1]) - 1, lambda = c)
}
erro_tipo_II <- Vectorize(FUN = erro_tipo_II,
                          vectorize.args = c("c", "n", "c0"))

grid <- expand.grid(n = 1,
                    c = as.numeric(names(oc)),
                    c0 = cm)
grid$err <- with(grid,
                 erro_tipo_II(c = c,
                              n = n,
                              c0 = c0,
                              n_sigmas = 3))

plot(err ~ c, data = grid, type = "l")

# Comparando os resultados.
round(cbind(grid$err, oc), 5)

#-----------------------------------------------------------------------
# Exercício - não conformidades em cabos elétricos (gráficos de controle
# para não conformidades com tamanhos de amostra variáveis)

# Tamanhos das amostras (dimensão dos cabos).
n <- c(8000, 11000, 9000, 10000, 10000, 7000, 10000, 12000, 14000,
       10500, 6500, 12000, 10000, 10000, 10000, 7500, 11000, 8000,
       11000, 10000)
nu <- n/10000

# Números de não conformidades em cada amostra.
x <- c(7, 9, 5, 7, 9, 11, 6, 10, 12, 11, 7, 11, 8, 13, 6, 8, 9, 10, 8,
       13)

# Gráfico para o número de não conformidades por unidade.
qcc(data = x, sizes = nu, type = "u")

# A divisão por 10000 serve apenas para mudar a escala dos valores (de 1
# m para 10000 m).

# Agora, vamos adicionar ao gráfico as novas amostras.
n2 <- rep(2500, 5)
x2 <- c(2, 4, 5, 3, 2)

qcc(data = x,
    sizes = nu,
    newdata = x2,
    newsizes = n2/10000,
    type = "u")

#-----------------------------------------------------------------------
# Gráfico de controle feito à mão.

u <- x/nu
um <- weighted.mean(u, w = nu)
n_sigmas <- 3

me <- n_sigmas * sqrt(um/nu)
lim <- cbind(um - me, um + me)
lim[, 1] <- pmax(0, lim[, 1])

# x11()
plot(u, type = "o", pch = 19, ylim = extendrange(c(u, lim)))
matlines(seq_along(u), lim, lty = 2, col = 1, type = "s")
abline(h = um, lty = 2)

#-----------------------------------------------------------------------
# Gráfico de Pareto para não conformidades.

# Não conformidades para o serviço de entrega de pizza.
x <- c("Atraso na entrega",
       "Endereço não encontrado",
       "Pizza fria",
       "Pedido incompleto",
       "Embalagem avariada",
       "Problema no pagamento")

# Reamostrando das reclamações com pesos.
dados <- sample(x,
                size = 500,
                prob = c(0.45, 0.05, 0.2, 0.15, 0.05, 0.1),
                replace = TRUE)

# Frequência amostral.
tabela <- table(dados)
tabela

# Gráfico de pareto.
pc <- pareto.chart(tabela)
pc

#-----------------------------------------------------------------------
# Gráfico de Pareto feito em ggplot2.

library(ggplot2)

# Coloca os dados em tabela.
da <- data.frame(cause = rownames(pc),
                 freq = pc[, "Frequency"],
                 cfreq = pc[, "Cum.Freq."],
                 crel = pc[, "Cum.Percent."])
rownames(da) <- NULL
da

# Quebra o texto longo.
da$cause_lines <- sapply(da$cause,
                         FUN = function(x) {
                           paste(strwrap(x, w = 15),
                                 collapse = "\n")
                         })

# Gráfico de pareto com ggplot2.
ggplot(da) +
  aes(x = reorder(cause_lines, -freq)) +
  geom_col(aes(y = freq), fill = "orange", color = "black") +
  geom_line(aes(y = cfreq, group = 1)) +
  geom_point(aes(y = cfreq)) +
  expand_limits(y = c(0, sum(da$freq))) +
  labs(x = "Causa") +
  scale_y_continuous(name = "Frequência",
                     sec.axis = sec_axis(~100 * ./max(da$cfreq),
                                         name = "Frequência relativa"))

#-----------------------------------------------------------------------
# Diagrama de causa e efeito.

cause.and.effect(cause = list(
  Produto = c("Ingredientes",
              "Preço baixo",
              "Pizza queimada",
              "Sabores corretos"),
  Entrega = c("Atraso",
              "Taxa de entrega",
              "Confusão de pedidos",
              "Temperatura"),
  Atendimento = c("Aceita whatsapp",
                  "Telefone ocupado",
                  "Atendente hostil"),
  Pagamento = c("Aceita cartão",
                "Possui troco"),
  Embalagem = c("Caixa suja",
                "Caixa amassada",
                "Lacre rompido")),
  effect = "Freguês\ninsatisfeito")

#-----------------------------------------------------------------------