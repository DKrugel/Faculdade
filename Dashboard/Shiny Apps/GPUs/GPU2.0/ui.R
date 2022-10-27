#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
library(dplyr)

#Criando Placeholder ## Remover depois

div_placeholder <- function(content = "PLACEHOLDER", 
                           color = "#d2d2d2", 
                           height = "3em") {
  css <- c(sprintf("background-color: %s;", color),
           "padding: 5px;",
           "margin: 5px;",
           "border-radius: 5px;",
           sprintf("height: %s;", height))
  div(content,
      style = paste0(css, colapse = " "))

}
  
# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Título
    titlePanel("Dashboard do Walminho"),

    # Header
    verticalLayout(fluidRow(
      sliderInput("Slider","Selecione a Faixa Temporal",
                  min=min(clock_ano$releaseYear),
                  max=max(clock_ano$releaseYear),
                  value = c(round(mean(clock_ano$releaseYear)-5),
                  round(mean(clock_ano$releaseYear))+5))
    ),
          checkboxGroupButtons(
            inputId = "BOTAO",
            label = "Quais chips serão exibidos?", 
            choices = c("Onboard", "Dedicada"),
            status = "danger")
        ,
    
        #Outputs
        mainPanel(
          verticalLayout(
            fluidRow(
              splitLayout(
                plotlyOutput("clock"),
                plotlyOutput("dispers"),
           ))),
          fluidRow(
            column(4,selectInput(inputId = "var1",
                        label = "Selecione a primeira variável:",
                        choices=names(gpu)[4:12])),
            column(4,
                   pickerInput(
                    inputId = "pickerManu",
                    label = "Manufacturer",
                    choices = total_manuf$manufacturer,
                    multiple = TRUE
            ))),
            fluidRow(
           splitLayout(
             plotlyOutput("hist1"),
             plotlyOutput("hist2"),
          ),
          
          div_placeholder("Tablela")
            )
    )
)))
