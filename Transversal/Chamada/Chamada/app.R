#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ptwikiwords)
# library(shinyTime)
# library(tidyverse)



#Código da UI

ui <- navbarPage("Transversal", position = "static-top",
                 tabPanel("home",

  
    titlePanel("Chamadas aleatórias"),

    
    verticalLayout(
        sidebarPanel(
        width = 12,
           fluidRow(
         
              column(6,
           numericInput(
            "HoraInicial", label = "Começo da aula:", 
            value = 0,
            min = 0,
            max = 23)),
       
           column(3,  
        numericInput(
          "MinInicial", label = "Minutos",
          value = 0,
          min = 0,
          max = 59))
        ),
        
        fluidRow(
          column(6,
            numericInput(
              "HoraFinal", label = "final da aula:",
              value = 23,
              min = 0,
              max = 59)
            ),
            
            column(3,
                numericInput(
                  "MinFinal", label = "(HH:MM)",
                  value = 59,
                  min = 0,
                  max = 59)
            )
          ),
        fluidRow(
          sliderInput(
            "qtd", label = "Quantidade de palavras geradas",
            min = 0,
            max = 100,
            value = 4
          )
        )
          
        ),


        mainPanel(
           verbatimTextOutput("Horarios"),
           verbatimTextOutput("words"), fluid = TRUE
         )
    )
    )#tabpanel Home
    ,tabPanel(
      "HELP",
      includeMarkdown("help.Rmd")
    )
    )#NavbarPage


#servidor com os Outputs
server <- function(input, output) {

    
    output$Horarios <- renderText({
      
      if(input$HoraFinal - input$HoraInicial > 4){
      hora <- sort(sample(input$HoraInicial:input$HoraFinal, size = 4), decreasing = FALSE)
      }else {hora <- sample(input$HoraInicial:input$HoraFinal, size = 4, replace = TRUE)}
      
      min <- sample(input$MinInicial:input$MinFinal, size = 4, replace = TRUE)
      paste(hora,min, sep = ":")
    
      })
    
   output$words <- renderText({
    # palavra <- read.csv2("ptBR.csv", sep = ",", encoding = "UTF-8")
    
      w <- sample(palavra$Word, size = input$qtd)
    
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
