library(tidyverse)
library(shiny)
library(plotly)

ui <- fluidPage(
  
  # Título
  titlePanel("GPUs"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "slider",
                  label = "Faixa de Anos:",
                  min = min(gpu$releaseYear),
                  max = max(gpu$releaseYear),
                  value = c(round(mean(gpu$releaseYear))-5,round(mean(gpu$releaseYear))+5)),
      
      selectInput(inputId = "var1",
                  label = "Selecione a primeira variável:",
                  choices=names(gpu)[4:12]),
      
      selectInput(inputId = "var2",
                  label = "Selecione a segunda variável:",
                  choices=names(gpu)[4:12],
                  selected=names(gpu)[5])
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Barplot ----
      plotlyOutput(outputId = "plot1"),
      plotlyOutput(outputId = "plot2")
      
    )
  )
)

server <- function(input, output) {
  library(tidyverse)
  library(plotly)
  
  gpu.comp <- read_csv("gpu_db.csv")
  
  gpu <- gpu.comp %>% 
    filter(!is.na(releaseYear))
  
  output$plot1 <- renderPlotly({
    f <- gpu[,c("releaseYear",input$var1)]
    f <- f %>% 
      group_by(releaseYear) %>% 
      summarise(Media=mean(.data[[input$var1]],na.rm=TRUE))
    
    dados_selecionados1 <- filter(f,between(f$releaseYear,input$slider[1],input$slider[2]))
    
    ggplotly(ggplot(dados_selecionados1,aes(x=releaseYear,y=Media))+
               geom_bar(stat="identity"))
  })
  
  output$plot2 <- renderPlotly({
    g <- gpu[,c("releaseYear",input$var2)]
    g <- g %>% 
      group_by(releaseYear) %>% 
      summarise(Media=mean(.data[[input$var2]],na.rm=TRUE))
    
    dados_selecionados2 <- filter(g,between(g$releaseYear,input$slider[1],input$slider[2]))
    
    ggplotly(ggplot(dados_selecionados2,aes(x=releaseYear,y=Media))+
               geom_bar(stat="identity"))
  })
}

shinyApp(ui, server)
