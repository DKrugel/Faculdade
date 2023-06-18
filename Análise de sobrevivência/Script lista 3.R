library(tidyverse)
library(survival)
library(survminer)
library(ComparisonSurv)

# Se vc estiver lendo esse sript, saiba que eu surtei e to fazendo ele na louca
#Não aguento mais, só quero um período normalizado com umas férias decentes...

Grande_  <- c("28"," 89"," 175"," 195"," 309"," 377 a"," 393 a"," 421 a"," 447 a"," 462"," 709 a"," 744 a"," 770 a"," 1106 a","1206")

Grande_censura <- ifelse(str_detect(Grande_, pattern = "a" ), 0,1)
Tumor_Grande <- as.numeric(str_remove(Grande_, "a"))

Pequeno_ <- c("34"," 88"," 137"," 199"," 280"," 291"," 299 a"," 300 a"," 309"," 351"," 358"," 369"," 369"," 370"," 375"," 382"," 392"," 429 a","451"," 1119 a")
Pequeno_Censura <- ifelse(str_detect(Pequeno_, "a"),0,1)
Tumor_Pequeno <- as.numeric(str_remove(Pequeno_, "a"))

dados_pequeno <- data.frame(cbind(Tumor_Pequeno, Pequeno_Censura))
dados_grande <- data.frame(cbind(Tumor_Grande,Grande_censura))
set.seed(123)
  tempo <- c(Surv(Tumor_Pequeno,Pequeno_Censura), Surv(Tumor_Grande,Grande_censura))
  tipo <- c(rep("Pequeno", length(Pequeno_)), rep("Grande", length(Grande_)))
  tipo_ <- c(rep(0, length(Pequeno_)), rep(1, length(Grande_)))
  
   dados_1 <- data.frame(tempo, tipo) 

ekm_pequeno <- survfit(Surv(Tumor_Pequeno,Pequeno_Censura)~1, data = dados_pequeno)
ekm_grande <- survfit(Surv(Tumor_Grande,Grande_censura)~1, data = dados_grande)

ekm_comp <- survfit(tempo ~ tipo, data = dados_1)
ggsurvplot(ekm_comp, pval = T)


modelo_cox <- coxph(tempo ~ tipo, data = dados_1)
summary(modelo_cox)

zph <- cox.zph(modelo_cox)
ggcoxzph(zph)

ComparisonSurv::Overall.test(c(Tumor_Pequeno,Tumor_Grande),
                             status = c(Pequeno_Censura,Grande_censura),
                             group = tipo_)
#Teste Log Rank rejeita a hipotese alternativa de que as duas curvas diferem a um nivel de confiança de 95%
# Porem o teste Tarone-Ware não.

pvalor <- numeric(100000)
tipo_amostral <- c(rep(0, length(amostra_pequeno)), rep(1, length(amostra_grande)))

for(i in 1:length(pvalor)){
amostra_pequeno <- sample(Surv(Tumor_Pequeno,Pequeno_Censura), 20, replace = T)
amostra_grande <- sample(Surv(Tumor_Grande,Grande_censura), 15, replace = T)
tempo_amostral <- c(amostra_pequeno,amostra_grande)

dados_amostral <- data.frame(tempo_amostra, tipo_amostral) 

logrank_test <- survdiff(tempo_amostral ~ tipo_amostral, data = dados_amostral)
pvalor[i] <- 1-pchisq(logrank_test$chisq, 1)
}

hist(pvalor, 
     main = "bootstrap da analise de sobrevivencia",
     ylab = "frequencia")
mean(pvalor)

splots <- list() 

splots[[1]] <-  ggsurvplot(ekm_pequeno)
splots[[2]] <-  ggsurvplot(ekm_grande)

ekm_2<- survfit(Surv(Tumor_Pequeno,Pequeno_Censura)[-sample(1:20,5)] ~ Surv(Tumor_Grande,Grande_censura), data = dados)
ggsurvplot(ekm_2)

arrange_ggsurvplots(splots)


coxph()
       
setwd("~/Faculdade/Análise de sobrevivência")             
dados <- read.table("ovario.txt", sep = " ", dec = ".",header = T)

View(dados)
splots <- list()

ekm_1 <- survfit(Surv(dados$tempo, dados$cens)~1, data = dados)
ekm_trat <-  survfit(Surv(dados$tempo, dados$cens)~ trat , data = dados)
splots[[1]] <- ggsurvplot(ekm_trat, pval = T)

#Falta clusterizar
ekm_idade <- survfit(Surv(dados$tempo, dados$cens) ~ idade , data = dados)
splots[[2]]<- ggsurvplot(ekm_idade, pval = T)

ekm_res <- survfit(Surv(dados$tempo, dados$cens)~ res , data = dados)
splots[[3]] <- ggsurvplot(ekm_res, pval = T)

ekm_status <- survfit(Surv(dados$tempo, dados$cens)~ status , data = dados)
splots[[4]] <- ggsurvplot(ekm_status, pval = T)

arrange_ggsurvplots(splots)



