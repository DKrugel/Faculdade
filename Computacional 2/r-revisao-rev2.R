##======================================================================
## CE089 - Estatística Computacional 2
## Estatística - UFPR
## Prof. Fernando Mayer
##======================================================================

##======================================================================
## Revisão de conceitos fundamentais da linguagem R ====================
##======================================================================

##======================================================================
## Tipos e classes de objetos
##======================================================================

## Vetores atômicos ----------------------------------------------------

## double
x <- c(2, 4, 6)
typeof(x)
class(x)
## integer
x <- as.integer(x)
x <- c(2L, 4L, 6L)
typeof(x)
class(x)

x <- c(2, "a")
x
typeof(x)

## character
x <- c("a", "b", "c")
typeof(x)
class(x)
## logical
x <- c(TRUE, FALSE, TRUE)
typeof(x)
class(x)
## complex
x <- c(2 + 1i, 4 + 1i, 6 + 1i)
typeof(x)
class(x)
## raw
x <- raw(3)
typeof(x)
class(x)

## Lista ---------------------------------------------------------------
x <- list(1:10, letters[1:5])
typeof(x)
class(x)

## OBJETO -> CLASSE -> METODOS

##----------------------------------------------------------------------
## Representação numérica dentro do R

set.seed(123)
(x <- runif(10))

getOption("digits")

set.seed(12)
(y <- runif(10))

## Erro de ponto flutuante
sqrt(2)^2 - 2

print(sqrt(2)^2, digits = 22)

0.3 + 0.6 - 0.9
print(c(0.3, 0.6, 0.9), digits = 22)

print(x, digits = 1)
print(x, digits = 7) # padrão
print(x, digits = 22)

format(x, scientific = TRUE)

x <- 1/100
x
(x <- 1/100)
format(x)

x <- 1/1000
x
(x <- 1/1000)

format(x, sci=TRUE)
format(x, sci=FALSE)

1/1000
format(1/1000)
format(1/10000)

5/2
format(5/2)
format(5/2, digits=3)
format(5/2, nsmall=3)


pi
print(pi, dig=5)
format(pi, dig=5)
sprintf("%.4f", pi)

format(0.0300, dig=5)
sprintf("%.4f", 0.0300)

format(0.0300, dig=5)
formatC(0.0300)
formatC(0.0300, dig=5)
formatC(0.0300, dig=5, flag=0)


## muitas opões em format(), formatC(), sprintf(), prettyNum()

## outras opções em pacotes como stringi
## x <- c(471, 3645, 1123, 10236)

## > stringi::stri_pad_left(x, 5, 0)
## [1] "00471" "03645" "01123" "10236"
## > stringi::stri_pad_left(x, 5, 2)
## [1] "22471" "23645" "21123" "10236"
## > stringi::stri_pad_left(x, 6, 0)
## [1] "000471" "003645" "001123" "010236"
## > stringi::stri_pad_left(x/100, 6, 0)
## [1] "004.71" "036.45" "011.23" "102.36"
## > 
## > stringi::stri_pad_right(x, 6, 0)
## [1] "471000" "364500" "112300" "102360"
## > stringi::stri_pad_right(x/100, 6, 0)
## [1] "4.7100" "36.450" "11.230" "102.36"
## > stringi::stri_pad_right(x/100, 7, 0)
## [1] "4.71000" "36.4500" "11.2300" "102.360"

##----------------------------------------------------------------------
## Operações com vetores

num <- 1:6
num
num * 2
num * num
num + c(2, 4, 1) # como se chama este "mecanismo"?
num + c(2, 4, 1, 3)

##======================================================================
## Indexação e seleção condicional
##======================================================================

##======================================================================
## Indexação

## Vetores -------------------------------------------------------------

cont <- c(8, 4, NA, 9, 6, 1, 7, 9)
cont

cont[]
cont[4]
cont[c(1, 4, 8)]
ind <- c(1, 4, 8)
cont[ind]
cont[-4]
cont[-c(1, 4, 8)]
cont[1:5]
cont[seq(1, 8, by = 2)]
cont[-1:5]
cont[-(1:5)]

is.na(cont)
cont[is.na(cont)]

!is.na(cont)

c(TRUE, FALSE)
!c(TRUE, FALSE)

cont[!is.na(cont)]

cont[is.na(cont)] <- 0
cont
cont[3] <- NA
cont

names(cont) <- letters[1:length(cont)]
cont
cont["d"]
cont[c("f", "a")]

## Matrizes ------------------------------------------------------------

mat <- matrix(1:9, nrow = 3)
mat
mat[2, 3]
mat[, 1]
mat[, 1, drop = FALSE]
mat[1, ]
mat[1, , drop = FALSE]

mat
dim(mat)

mat[ , 1]
class(mat[ , 1])
mat[ , 1, drop = FALSE]
dim(mat[ , 1, drop = FALSE])

2 + 2
"+"(2, 2)

mat[3, 2]
mat[3, 2, drop = FALSE]
mat[1, ]
mat[1, , drop = FALSE]

mat[c(1, 3), c(2, 3)]

colnames(mat) <- LETTERS[1:3]
mat[, "B"]
mat[, "B", drop = FALSE]
mat[1, "C"]

rownames(mat) <- LETTERS[24:26]
mat["X", ]
mat["X", , drop = FALSE]
mat["Y", "A"]

## Listas --------------------------------------------------------------

lis <- list(c(3, 8, 7, 4), mat, 5:0, "O R é legal")
lis

length(lis)

lis[1]
class(lis[1])

lis[[1]]
class(lis[[1]])

mean(lis[1])
mean(lis[[1]])

lis[[1]][3]
lis[[2]]
lis[[2]][2, 3]

lis <- list(vetor1 = c(3, 8, 7, 4), mat = mat,
            vetor2 = 5:0, mensagem = "O R é legal")
## names(lis) <- c("vetor1", "mat", "vetor2", mensagem)
lis

lis$mensagem

lis$mat
lis$mat[2, 3]
lis$vetor1[3]

lis[["mat"]]
lis[["vetor1"]][3]

## data-frame ----------------------------------------------------------

da <- data.frame(A = 4:1, B = c(2, NA, 5, 8))
da

da[2, 1]
da[, 2]

da[,"B"]
da[1, ]

da["1", ]

rownames(da)
rownames(da) <- c("D", "U", "T", "K")
rownames(da)
da

da["U", ]

da[, "B"]
mean(da[, "B"], na.rm = TRUE)
class(da[, "B"])
da["U", "B"]
da[, "B", drop = FALSE]
mean(da[, "B", drop = FALSE])
class(da[, "B", drop = FALSE])

da$A
da$B[3]
da$B[c(2, 4)]

da[1]
class(da[1])

da[[1]]
class(da[[1]])
da[["A"]]
da[["A"]][2:3]

da

is.na(da)
da[is.na(da), ]

da[is.na(da$A), ]
da[is.na(da$B), ]

is.na(da)
is.na(da$A)
is.na(da$B)

da[!is.na(da$B), ]

is.na(da)
complete.cases(da)
!complete.cases(da)
da[complete.cases(da), ]
da[!complete.cases(da), ]

summary(da)

complete.cases(da)
!complete.cases(da)
sum(!complete.cases(da))

as.logical(0:10)

airquality
head(airquality)
dim(airquality)
colMeans(airquality)
complete.cases(airquality)
sum(complete.cases(airquality))
aqc <- airquality[complete.cases(airquality), ]

colMeans(airquality)
colMeans(aqc)
colMeans(airquality, na.rm = TRUE)

aqcc <- na.omit(airquality)

head(aqc)
head(aqcc)
all(aqcc == aqc)
all.equal(aqcc, aqc)
str(aqc)
str(aqcc)

##======================================================================
## Seleção condicional
##======================================================================

## Vetores -------------------------------------------------------------

dados <- c(5, 15, 42, 28, 79, 4, 7, 14)

dados > 15
dados[dados > 15]

dados[dados > 15 & dados <= 35]
dados[dados > 15 | dados <= 35]

dados > 15 & dados <= 35
dados > 15 | dados <= 35

cara <- letters[1:length(dados)]
cara

cara == "c"
dados[cara == "c"]

cara == "a" & cara == "c" # porque não funciona?
cara == "a" | cara == "c"
dados[cara == "a" | cara == "c"]
dados[cara == "a" | cara == "c" | cara == "f"]

cara == c("a", "c")
dados[cara == c("a", "c")] # errado
dados[cara %in% c("a", "c")]
cara %in% c("a", "c")

dados[cara %in% c("a", "c", "f")]

cara == c("a", "c")
cara %in% c("a", "c")

cara[dados == 15]
cara[dados > 30]
cara[dados %in% c(4, 14)]

dados[dados > 15]
which(dados > 15)

dados[dados > 15 & dados <= 35]
which(dados > 15 & dados <= 35)

dados[cara == "c"]
which(cara == "c")

dados[cara %in% c("a", "c")]
which(cara %in% c("a", "c"))

## Data frame ----------------------------------------------------------
dados <- data.frame(ano = c(2001, 2002, 2003, 2004, 2005),
                    captura = c(26, 18, 25, 32, NA),
                    porto = c("SP", "RS", "SC", "SC", "RN"))

dados[dados$ano == 2004, ]
dados[dados$porto == "SC", ]
dados[dados$captura > 20, "captura"]

dados[dados$captura > 20 & !is.na(dados$captura), ]
dados[dados$captura > 20 & complete.cases(dados), ]

dados[dados$captura > 25 & dados$porto == "SP", ]

dados[dados$porto == "SC", ]
subset(dados, porto == "SC")
dados[dados$captura > 20, ]
subset(dados, captura > 20)
dados[dados$captura > 20 & !is.na(dados$captura), ]
dados[dados$captura > 20, "captura"]
subset(dados, captura > 20, select = captura)
subset(dados, captura > 20, select = captura,  drop = TRUE)

##======================================================================
## Programação

## Estruturas de repetição ---------------------------------------------

for(i in 1:10){
    print(i)
}

x <- 100:200
for(j in 1:10){
    print(x[j])
}

for(i in c(2, 9, 4, 6)){
    print(i^2)
}

for(veiculos in c("carro", "ônibus", "trem", "bicicleta")){
    print(veiculos)
}

for(veiculos in c("carro", "ônibus", "trem", "bicicleta")){
    Sys.sleep(3)
    print(veiculos)
}

## Outras estruturas: while e repeat

## Calcule a soma em 1,2,3... até que a soma seja maior do que 1000
n <- 0
soma <- 0
while(soma <= 1000){
    n <- n + 1
    soma <- soma + n
}
soma

## DESAFIO
## Como fazer a soma dos 1000 primeiros numeros primos?

qnt <- 1000 #quantidade de numeros primos a ser somada
num <- 3 #comeca a testar a partir do numero 3
aux <- 0
soma <- 2 #ja considera o numero 2 na soma
cont <- 1

while(cont < qnt){
    for(i in 2:(num-1)){
        if(num %% i == 0){
            aux <- aux + 1
        }
    }
    if(aux == 0){
        soma <- soma + num
        cont <- cont + 1
        print(num) #printa os numeros primos, caso necessario
    }
    aux <- 0
    num <- num+1
}
soma

n <- 0
soma <- 0
repeat{
    n <- n + 1
    soma <- soma + n
    if(soma > 1000) break
}
soma

## Estrutura de seleção ------------------------------------------------

x <- 100:200
for(j in 1:10){
    if(x[j] <= 105){
        print("Menor ou igual a 105")
    }
}

x <- 100:200
for(j in 1:10){
    if(x[j] <= 105){
        print("Menor ou igual a 105")
    } else{
        print("Maior do que 105")
    }
}


##----------------------------------------------------------------------
## **Exemplo**: cálculo de notas de uma disciplina

url <- "http://leg.ufpr.br/~fernandomayer/data/notas.csv"
notas <- read.table(url, header = TRUE, sep = ";", dec = ",")
str(notas)
head(notas)
summary(notas)

notas[1, c("prova1", "prova2", "prova3")]
mean(notas[1, c("prova1", "prova2", "prova3")])
class(notas[1, c("prova1", "prova2", "prova3")])
mean(as.numeric(notas[1, c("prova1", "prova2", "prova3")]))

## Antes de seguir adiante, veja o resultado de
for(i in 1:30){
    print(notas[i, c("prova1", "prova2", "prova3")])
}

notas[1, c("prova1", "prova2", "prova3")]
class(notas[1, c("prova1", "prova2", "prova3")])
as.numeric(notas[1, c("prova1", "prova2", "prova3")])
class(as.numeric(notas[1, c("prova1", "prova2", "prova3")]))

## Calcula a média das 3 provas (para cada aluno)
notas$media <- 0
nlinhas <- nrow(notas)
provas <- c("prova1", "prova2", "prova3")
for(i in 1:nlinhas){
    notas$media[i] <- mean(as.numeric(notas[i, provas]))
}

head(notas)

notas$media <- rowMeans(notas[, provas])

## apenas comentando rapidamente
ap <- apply(notas[, provas], 1, mean)
all.equal(notas$media, ap)
rm(ap)

## e isto importa?
notas1000 <- notas[replicate(1000, seq_len(nrow(notas))), provas]
system.time({
    notas1000$media <- 0;
    for(i in 1:nrow(notas1000)){
        notas1000$media[i] <- mean(as.numeric(notas1000[i, provas]))
    }
    })
system.time(apply(notas1000, 1, mean))
system.time(rowMeans(notas1000))
## .. mais detalhes sobre apply e system.time mais adiante!

## E para calcular o CV?
cv <- function(x){
    desv.pad <- sd(x)
    med <- mean(x)
    cv <- desv.pad/med
    return(cv)
}

## Cria uma nova coluna para o CV
notas$CV <- 0
for(i in 1:nlinhas){
    notas$CV[i] <- cv(as.numeric(notas[i, provas]))
}

head(notas)

## Nova coluna para armazenar a situacao
notas$situacao <- NA
for(i in 1:nlinhas){
    if(notas$media[i] >= 7){
        notas$situacao[i] <- "aprovado"
    } else{
        notas$situacao[i] <- "reprovado"
    }
}

head(notas)

##======================================================================
##  O modo R: vetorização

x <- 1:1e6
## Calcula o quadrado de cada número da sequência em x usando for()
y1 <- numeric(length(x)) # vetor de mesmo comprimento de x que vai
                         # receber os resultados
for(i in 1:length(x)){
    y1[i] <- x[i]^2
}

## Modo R
y2 <- x^2
## Confere os resultados
identical(y1, y2)

## Tempo de execução usando for()
y1 <- numeric(length(x))
st1 <- system.time(
    for(i in 1:length(x)){
        y1[i] <- x[i]^2
    }
)
st1

## Tempo de execução usando a regra da reciclagem
st2 <- system.time(
    y2 <- x^2
)
st2

st1[3]/st2[3]

##----------------------------------------------------------------------
## Comparações usando apenas variações do for()

x <- 1:1e6

## Cria um objeto de armazenamento com o mesmo tamanho do resultado
st1 <- system.time({
    out1 <- numeric(length(x))
    for(i in 1:length(x)){
        out1[i] <- x[i]^2
    }
})
st1

## Cria um objeto de tamanho "zero" e vai "crescendo" esse vetor
st2 <- system.time({
    out2 <- numeric(0)
    for(i in 1:length(x)){
        out2[i] <- x[i]^2
    }
})
st2

## Cria um objeto de tamanho "zero" e cresce o vetor usando a função c()
## NUNCA faça isso!!
st3 <- system.time({
    out3 <- numeric(0)
    for(i in 1:length(x)){
        out3 <- c(out3, x[i]^2)
    }
})
st3
identical(out1, out2, out3)

## knitr::include_graphics("img/R_club.jpg")
##----------------------------------------------------------------------

##----------------------------------------------------------------------
## A família de funções *apply()

notas$media.apply <- apply(X = notas[, provas], MARGIN = 1, FUN = mean)
head(notas)
notas$CV.apply <- apply(X = notas[, provas], MARGIN = 1, FUN = cv)
head(notas)

notas$situacao.apply <- ifelse(notas$media.apply >= 7,
                               "aprovado", "reprovado")
head(notas)

## Médias por LINHA: média das 3 provas para cada aluno
apply(X = notas[, provas], MARGIN = 1, FUN = mean)
## Médias por COLUNA: média de cada uma das 3 provas para todos os
## alunos
apply(X = notas[, provas], MARGIN = 2, FUN = mean)

mean(notas$prova1)
mean(notas$prova2)
mean(notas$prova3)

## sapply simpilifica o resultado para um vetor
sapply(notas[, provas],  mean)
## lapply retorna o resultado em formato de lista
lapply(notas[, provas],  mean)

sapply(notas[, provas],  summary)
lapply(notas[, provas],  summary)


## Média da prova 1 por situação
tapply(notas$prova1,  notas$situacao,  mean)
## Média da prova 2 por situação
tapply(notas$prova2,  notas$situacao,  mean)
## Média da prova 3 por situação
tapply(notas$prova3,  notas$situacao,  mean)

## Mesmo resultado da tapply, mas agora em formato de data frame
aggregate(prova1 ~ situacao, data = notas, FUN = mean)
aggregate(prova2 ~ situacao, data = notas, FUN = mean)
aggregate(prova3 ~ situacao, data = notas, FUN = mean)
## Mas aqui podemos passar as 3 colunas de uma vez
aggregate(cbind(prova1, prova2, prova3) ~ situacao,
          data = notas, FUN = mean)

##======================================================================
## Funções

##----------------------------------------------------------------------
## Funções e argumentos

runif(10, 1, 100)
runif(n = 10, min = 1, max = 100)
args(sample)
args(plot)

##----------------------------------------------------------------------
## Funções for dummies

ola.mundo <- function(){
    writeLines("Olá mundo")
}

ola.mundo()


ola.mundo <- function(texto){
    writeLines(texto)
}

ola.mundo()

ola.mundo("Funções são legais")

##----------------------------------------------------------------------
## Função para fazer a soma e a média de dois vetores
rm(list = ls())

eu <- 50
tu <- 100

soma <- function(x, y) {
    ss <- sum(x, y)
    mm <- mean(c(x, y))
    return(c(ss, mm))
}

a <- c(2, 5, 9)
b <- c(3, 8, 4)
soma(a, b)

sum(a, b)
sum(c(a, b))
mean(a, b)
mean(c(a, b))
mean(c(a, b), trim = 1)

a <- c(2, 5, NA)
b <- c(3, 8, NA)
soma(a, b)

soma <- function(x, y, ...) {
    ss <- sum(x, y, ...)
    mm <- mean(c(x, y), ...)
    return(c(ss, mm))
}

a <- c(2, 5, 9)
b <- c(3, 8, 4)
soma(a, b)

a <- c(2, 5, NA)
b <- c(3, 8, NA)
soma(a, b)
soma(a, b, na.rm = TRUE)

## Veja que
mean(a, b, na.rm = TRUE)
mean(c(a, b), na.rm = TRUE)
mean(c(a, b), na.rm = TRUE, 0.5)
mean(c(a, b), na.rm = TRUE, trim = 0.5)

sum(c(a, b), na.rm = TRUE)
sum(a, b, na.rm = TRUE)

## MUITO CUIDADO!!!

soma <- function(x, y, ...) {
    ss <- sum(x, y, ...)
    mm <- mean(c(eu, tu), ...)
    return(c(ss, mm))
}

a <- c(2, 5, 9)
b <- c(3, 8, 4)
soma(a, b)

a <- c(2, 5, NA)
b <- c(3, 8, NA)
soma(a, b, na.rm = TRUE)

## O que aconteceu aqui?
ls()
search()
ls(".GlobalEnv")
ls("package:datasets")

## Como evitar: testando se existem no .GlobalEnv
soma <- function(x, y, ...) {
    if(exists("eu")) {eu <- x}
    if(exists("tu")) {tu <- y}
    ss <- sum(x, y, ...)
    mm <- mean(c(eu, tu), ...)
    return(c(ss, mm))
}

a <- c(2, 5, 9)
b <- c(3, 8, 4)
soma(a, b)

a <- c(2, 5, NA)
b <- c(3, 8, NA)
soma(a, b, na.rm = TRUE)

## Como evitar: usando environments
soma <- function(x, y, ...) {
    e <- rlang::env(x = x, y = y)
    ss <- sum(e$x, e$y, ...)
    mm <- mean(c(e$x, e$y), ...)
    return(c(ss, mm))
}

a <- c(2, 5, 9)
b <- c(3, 8, 4)
soma(a, b)

a <- c(2, 5, NA)
b <- c(3, 8, NA)
soma(a, b, na.rm = TRUE)

##======================================================================
## Programação Orientada a Objetos

#'
#' Como vimos anteriormente, o R é uma linguagem de programação
#' orientada à objetos. Dois conceitos fundamentais desse tipo de
#' linguagem são os de **classe** e **método**. Já vimos também que todo
#' objeto no R possui uma classe (que define sua estrutura) e analisamos
#' algumas delas. O que seria então um método? Para responder essa
#' pergunta precisamos entender inicialmente os tipos de orientação a
#' objetos que o R possui.
#'
#' O R possui 3 sitemas de orientação a objetos: **S3**, **S4**, e
#' **RC**:
#'
#' - **S3**: implementa um estilo de programação orientada a objeto
#'   chamada de *generic-function*. Esse é o estilo mais básico de
#'   programação em R (e também o mais utilizado). A ideia é que existam
#'   **funções genéricas** que decidem qual método aplicar de acordo com
#'   a classe do objeto. Os métodos são definidos da mesma forma que
#'   qualquer função, mas chamados de maneira diferente. É um estilo de
#'   programação mais "informal", mas possibilita uma grande liberdade
#'   para o programador.
#'
#' - **S4**: é um estilo mais formal, no sentido que que as funções
#'   genéricas devem possuir uma classe formal definida. Além disso, é
#'   possível também fazer o **despacho múltiplo de métodos**, ao
#'   contrário da classe S3.
#'
#' - **RC**: (*Reference Classes*, antes chamado de R5) é o sistema mais
#'   novo implementado no R. A principal diferença com os sistemas S3 e
#'   S4 é que métodos pertencem à objetos, não à funções. Isso faz com
#'   que objetos da classe RC se comportem mais como objetos da maioria
#'   das linguagens de programação, como Python, Java, e C#.
#'
#' Nesta sessão vamos abordar como funcionam os métodos como definidos
#' pelo sistema S3, por ser o mais utilizado na prática para se criar
#' novas funções no R. Para saber mais sobre os outros métodos, consulte
#' o livro [Advanced R](http://adv-r.had.co.nz/OO-essentials.html).
#'
#' Vamos entender como uma função genérica pode ser criada através de um
#' exemplo. Usando a função `methods()`, podemos verificar quais métodos
#' estão disponíveis para uma determinada função, por exemplo, para a
#' função `mean()`:
#'
methods(mean)
#'
#' O resultado são expressões do tipo `mean.<classe>`, onde `<classe>` é
#' uma classe de objeto como aquelas vistas anteriormente. Isso
#' significa que a função `mean()`, quando aplicada à um objeto da
#' classe `Date`, por exemplo, pode ter um comportamento diferente
#' quando a mesma função for aplicada à um objeto de outra classe
#' (numérica).
#'
#' Suponha que temos o seguinte vetor numérico:
#'
set.seed(1)
vec <- rnorm(100)
class(vec)
#'
#' e queremos calcular sua média. Basta aplicar a função `mean()` nesse
#' objeto para obtermos o resultado esperado
#'
mean(vec)
#'
#' Mas isso só é possível porque existe um método definido
#' espcificamente para um vetor da classe `numeric`, que nesse caso é a
#' função `mean.default`. A função genérica nesse caso é a `mean()`, e a
#' função método é a `mean.default`. Veja que não precisamos escrever
#' onome inteiro da função genérica para que ela seja utilizada, como
#' por exemplo,
#'
mean.default(vec)
#'
#' Uma vez passado um objeto para uma função, é a classe do objeto que
#' irá definir qual método utilizar, de acordo com os métodos
#' disponíveis. Veja o que acontece se forcarmos o uso da função
#' `mean.Date()` nesse vetor
mean.Date(vec)
#'
#' O resultado não faz sentido pois ele é específico para um objeto da
#' classe `Date`.
#'
#' Tudo isso acontece por causa de um mecanismo chamado de **despacho de
#' métodos** (*method dispatch*), que é responsável por identificar a
#' classe do objeto e utilizar ("despachar") a função método correta
#' para aquela classe. Toda função genérica possui a mesma forma: uma
#' chamada para a função `UseMethod()`, que especifica o nome genérico e
#' o objeto a ser despachado. Por exemplo, veja o código fonte da função
#' `mean()`
#'
mean
#'
#' Agora veja o código fonte da função `mean.default`, que é o método
#' específico para vetores numéricos
#'
mean.default
#'
#' Agora suponha que você ddeseja criar uma função que calcule a média
#' para um objeto de uma classe diferente daquelas previamente
#' definidas. Por exemplo, suponha que você quer que a função `mean()`
#' retorne a média das linhas de uma matriz.
#'
set.seed(1)
mat <- matrix(rnorm(50), nrow = 5)
mean(mat)
rowMeans(mat)
#'
#' O resultado é a média de todos os elementos, e não de cada linha.
#' Nesse caso, podemos definir nossa própria função método para fazer o
#' cálculo que precisamos. Por exemplo:
#'
mean.matrix <- function(x, ...) rowMeans(x, ...)
#'
#' Uma função método é sempre definida dessa forma:
#' `<funçãogenérica>.<classe>`. Agora podemos ver novamente os métodos
#' disponíveis para a função `mean()`
#'
methods(mean)

#'
#' e simplesmente aplicar a função genérica `mean()` à um objeto da classe
#' `matrix` para obter o resultado que desejamos
#'
class(mat)
mean(mat)
#'
#' Esse exemplo ilustra como é simples criar funções método para
#' diferentes classes de objetos. Poderíamos fazer o mesmo para objetos
#' das classes `data.frame` e `list`
#'
mean.data.frame <- function(x, ...) sapply(x, mean, ...)
mean.list <- function(x, ...) lapply(x, mean)
#'
#' Aplicando em objetos dessas classes específicas, obtemos:
#'
## Data frame
set.seed(1)
da <- data.frame(c1 = rnorm(10),
                 c2 = runif(10))
class(da)
mean(da)
## Lista
set.seed(1)
dl <- list(rnorm(10), runif(50))
class(dl)
mean(dl)



#'
#' Obviamente esse processo todo é extremamente importante ao se criar
#' novas funções no R. Podemos tanto criar uma função genérica (como a
#' `mean()`) e diversos métodos para ela usando classes de objetos
#' existentes, quanto (inclusive) criar novas classes e funções método
#' para elas. Essa é uma das grandes liberdades que o método S3 de
#' orientação à objetos permite, e possivelmente um dos motivos pelos
#' quais é relativamente simples criar pacotes inteiros no R.
#'
#' # Referências
#'
#' Para mais detalhes e exemplos dos assuntos abordados aqui, veja
#' @Grolemund2014. Uma abordagem mais avançada e detalhada sobre
#' programação orientada a objetos no R pode ser consultada em
#' @Wickham2015.
