
shinyUI(navbarPage(
                   title = "CE 062B",
                   position = "static-top",
                   theme = shinytheme("slate"),
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
                    label = "Escolha os fabricantes",
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
 ), #TabPanel Dados
tabPanel(
  "Sobre",
  includeMarkdown("Sobre.Rmd")
)#TabPanel Sobre

)#NavbarPage
)#ShinyUI



#Criando Placeholder ## Remover depois

# div_placeholder <- function(content = "PLACEHOLDER", 
#                            color = "#d2d2d2", 
#                            height = "3em") {
#   css <- c(sprintf("background-color: %s;", color),
#            "padding: 5px;",
#            "margin: 5px;",
#            "border-radius: 5px;",
#            sprintf("height: %s;", height))
#   div(content,
#       style = paste0(css, colapse = " "))
# 
# }
# 
#   
