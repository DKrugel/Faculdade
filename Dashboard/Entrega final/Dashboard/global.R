library(shiny)
library(shinyWidgets)
library(plotly)
library(dplyr)
library(markdown)
library(tidyverse)
library(shinythemes)

gpu.comp <- read.csv("gpu_db.csv")

gpu <- gpu.comp %>% 
  filter(!is.na(releaseYear))

  total_manuf <- gpu %>% 
    group_by(releaseYear, manufacturer) %>% 
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
