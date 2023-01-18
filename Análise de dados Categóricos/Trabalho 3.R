#Carregando dados
data <- read.csv(file = "http://leg.ufpr.br/~lucambio/ADC/healthcare_worker.csv")

c.table <- array(data = c(data[,2],(data[,3]-data[,2])),
                 dim = c(5,2),
                 dimnames = list(occupation = data[,1], Hepatite = c("+","-")))

c.table

chisq.test(c.table)

library(vcd)
assocstats(c.table)


####################
### Exercício 20 ###
####################


data <- read.csv(file = "http://www.ncbi.nlm.nih.gov/pubmedhealth/PMH0001187")
