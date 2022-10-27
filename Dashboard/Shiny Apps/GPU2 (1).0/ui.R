#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

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

  
shinyUI(navbarPage("CE 062B",
                   position = "static-top",
  tabPanel("Home",
    # Header
    verticalLayout(fluidRow(
      sliderInput("Slider","Selecione a Faixa Temporal",
                  min=min(clock_ano$releaseYear),
                  max=max(clock_ano$releaseYear),
                  value = c(round(mean(clock_ano$releaseYear)-5),
                  round(mean(clock_ano$releaseYear))+5),
                  ticks = FALSE)
    ),
        
    
        #Outputs
        mainPanel(
          verticalLayout(
            fluidRow(
              splitLayout(
                plotlyOutput("clock"),
                plotlyOutput("dispers"),
           ))),
          fluidRow(
            column(7,selectInput(inputId = "var1",
                        label = "Selecione a primeira variável:",
                        choices=names(gpu)[4:12])),
            column(5,
                   pickerInput(
                    inputId = "pickerManu",
                    label = "Manufacturer",
                    choices = unique(total_manuf$manufacturer),
                    multiple = TRUE
            ))),
            fluidRow(
           splitLayout(
             plotlyOutput("hist1"),
             plotlyOutput("hist2"),
          )
            )
          )
    )#Vertical Layout
),#Tab Panel Home
 tabPanel(
   "Dados Utilizados",
   dataTableOutput("Tabela")
 )

)#NavbarPage
)#ShinyUI
