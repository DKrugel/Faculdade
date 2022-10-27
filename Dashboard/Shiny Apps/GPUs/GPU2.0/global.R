library(flexdashboard)
library(tidyverse)
library(shiny)
library(plotly)
library(data.table)

gpu.comp <- read_csv("H:/Faculdade/Dashboard/Shiny Apps/GPUs/GPU2.0/gpu_db.csv")

gpu <- gpu.comp %>% 
  filter(!is.na(releaseYear))

total_manuf <- dados_selecionados4 %>% 
  group_by(manufacturer) %>% 
  summarise(Total=n())

total_ano <- gpu %>% 
  group_by(releaseYear) %>% 
  summarise(Total=n())

clock_ano <- gpu %>% 
  group_by(releaseYear) %>% 
  summarise(Media=mean(gpuClock))

memoria_ano <- gpu %>% 
  group_by(releaseYear) %>% 
  summarise(Media=mean(memSize,na.rm=TRUE))


