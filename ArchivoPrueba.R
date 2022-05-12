
#### PROYECTO 3 FByB 

# shiny test

library(shiny)
library(shinythemes)
library(TTR)
library(quantmod)

#definimos el ui para la app

 ui<-fluidPage(
  
   titlePanel("Análisis Técnico en R"),
   
   sidebarLayout(position = "left", 
     sidebarPanel("Elegir acción e indicadores a visualizar",
     
     selectInput(inputId = "stock", 
                 label = "Elegir acción:",
                 choices = c("NKE","F","TSLA", "WOOF"), selected = "NKE"),
     
     checkboxGroupInput(inputId = "ind", label = "Escoger indicadores a visualizar", choices = c("SMA","EMA","RSI","MACD","ATR","BB") )
     ),
     
     mainPanel("Mostramos gráficas",
      
      textOutput("selected_stock")
                        
               )
     )
   )
  
# hacemos parte tecnica de server
  
 server<-function(input,output){
   
   output$selected_stock <- renderText({ 
     paste("Gráfico de ", input$stock)
   })
   
   
   
 }
 
 #corre la app
 
 shinyApp(ui = ui, server = server)
 
