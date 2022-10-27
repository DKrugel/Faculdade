library(shiny)
library(shinyjs)
library(wordcloud2)
library(stringr)
library(tidyverse)

#make sure this directory exists
#uncomment and run if there is no 'Screens' folder in your 'Documents'
# if(!file.exists("~/Documents/Screens")){
#   dir.create("~/Documents/Screens")
# }

ui <- fluidPage(
  useShinyjs(), #make sure to drop this useShinyJs line somewhere in UI
  actionButton("go", label = "Começar!"),
  actionButton("stop", label = "Parar!"),
  textOutput('status'),
  wordcloud2Output("nuvem"),
  plotOutput("dens")
)

server <- function(input, output, session) {
  
  #this reactive value rv$loop will serve as our loop starter/stopper
  rv <- reactiveValues(loop = 0)
  
  #if input go is clicked the loop is started 
  #because the observe event only continues if rv$loop is > 0
  observeEvent(input$go,{
    rv$loop <- 1
  })
  
  #if input stop is clicked loop is stopped
  #because this makes rv$loop -100, which is < 0 
  #so observe event is not triggered again for the screenshot loop
  
  observeEvent(input$stop,{
    rv$loop <- -100
  })
  
  #observe changes in the rv$loop variable
  #only do anything if the rv$loop value is greater than 0
  #if it is triggered the last line adds 1 to rv$loop
  #which re-validates the observeEvent to trigger it again
  #it only stops if input$stop is pushed to make rv$loop -100
  observeEvent(rv$loop, {
    if(rv$loop > 0){
      texto <- data.frame(letras=sample(letters,size=100000,replace=TRUE)) %>% 
                  group_by(letras) %>% 
                  summarise(Total=n())
      output$nuvem <- renderWordcloud2({
        wordcloud2(texto)
      })
      output$dens <- renderPlot({
        ggplot(texto,aes(x=letras,y=Total,fill=letras))+
          geom_bar(stat="identity")
      })
      shinyjs::delay(5000, rv$loop <- rv$loop + 1)
      
    }
  })
}

shinyApp(ui, server)
