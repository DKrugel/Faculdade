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
opções <- c(1985:2022)  

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Título
    titlePanel("Old Faithful Geyser Data"),

    # Header
    verticalLayout(fluidRow(
       column(2,
              pickerInput(
                inputId = "LIANO",
                label = "Escolha o ano inicial:", 
                choices = opções)
        ),
       column(2,
          pickerInput(
            inputId = "LSANO",
            label = "Escolha o ano final:", 
            choices = opções)
       )),
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
                div_placeholder("plot1"),
                div_placeholder("plot2"),
           )),
          fluidRow(
           splitLayout(
            div_placeholder("plot3"),
            div_placeholder("plot4"),
          )),
          
          div_placeholder("Tablela")
    )
))
))
