

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

 # df <- read.csv2("gpu_db.csv", header = TRUE, sep = ",")
 # LSANO1 <- c(reactive({input$LSANO}))
 # LIANO1 <- c(reactive({input$LSANO}))
  
  #df.timestamped <- df[between(df$releaseYear, reactive({input$LSANO}), reactive({input$LIANO})),  ]  
  df.timestamped <- filter(df$releaseYear, input$LSANO,input$LIANO)
  
  
     output$distPlot <- renderPlot({

        # generate bins based on input$bins from ui.R
       x    <- faithful[, 2]
       
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')

    })
    
    

})

