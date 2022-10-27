#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(flexdashboard)
library(tidyverse)
library(plotly)
library(data.table)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$distPlot <- renderPlot({

        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')

    })
    
    output$clock <- renderPlotly({
      dados_selecionados <- filter(clock_ano,between(clock_ano$releaseYear,input$Slider[1],input$Slider[2]))
      
      g <- dados_selecionados %>% 
        ggplot(aes(x=releaseYear,y=Media))+
        geom_point()+
        geom_line()+
        xlab("Anos")+
        ylab("Clock Médio")
      
    })
    
    output$memory <- renderPlotly({
      dados_selecionados2 <- filter(memoria_ano,between(memoria_ano$releaseYear,input$Slider[1],input$Slider[2]))
      
      h <- dados_selecionados2 %>% 
        ggplot(aes(x=releaseYear,y=Media))+
        geom_bar(stat="identity")
      
      ggplotly(h)
    })
  
    output$dispers <- renderPlotly({
      
      dados_selecionados3 <- filter(gpu, between(gpu$releaseYear, input$Slider[1], input$Slider[2]))
      
      plot.dispercao <- ggplot(dados_selecionados3, aes(x=gpuClock, y=memClock, color = releaseYear)) + 
        geom_point() + 
        labs(x = "Clock da GPU", y = "Clock da memória", color = "Ano de lançamento")
      
      ggplotly(plot.dispercao)
    })
    
    output$hist1 <- renderPlotly({
      f <- gpu[,c("releaseYear",input$var1)]
      f <- f %>% 
        group_by(releaseYear) %>% 
        summarise(Media=mean(.data[[input$var1]],na.rm=TRUE))
      
      dados_selecionados1 <- filter(f,between(f$releaseYear,input$Slider[1],input$Slider[2]))
      
      ggplotly(ggplot(dados_selecionados1,aes(x=releaseYear,y=Media))+
                 geom_bar(stat="identity"))
    })
    
    output$hist2 <- renderPlotly({
      dados_selecionados4 <- filter(gpu,between(gpu$releaseYear, input$Slider[1], input$Slider[2]))  
      dados_selecionados4 <- subset(dados_selecionados4, manufacturer %in% input$pickerManu)
       
       ggplotly(
            ggplot(total_manuf,
              aes(x = manufacturer, y = Total))+
              geom_bar(stat = "identity")
  )
})
    
})





