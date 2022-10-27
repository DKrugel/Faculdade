
shinyServer(function(input, output) {
  
  # Valor.botao <- c()
  # if(length(observe({input$BOTAO})) == 1){
  #   if("Onboard" %in% observe({input$BOTAO})){
  #     Valor.botao <- c(1)
  #   }else{
  #     Valor.botao <- c(2)
  #   }
  # }else{
  #   Valor.botao <- c(3)
  # }
  # 
  # if(Valor.botao == 1){
  #   gpu <- filter(gpu, is.na(memSize))
  # }
  # if(Valor.botao == 2){
  #   gpu <- filter(gpu, !is.na(memSize))
  # }
  # 
    
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
       dados_selecionados4 <- filter(total_manuf,between(releaseYear, input$Slider[1], input$Slider[2]))  
       dados_selecionados4 <- subset(dados_selecionados4, manufacturer %in% input$pickerManu)
       
      
       ggplotly(
            ggplot(dados_selecionados4,
              aes(x = manufacturer, y = Total))+
              geom_bar(stat = "identity", position = "stack")
  )
})
    output$Tabela <- renderDataTable(
      gpua
    )
})





