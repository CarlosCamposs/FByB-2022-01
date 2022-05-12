# shiny test

library(shiny)
library(shinythemes)
library(xts)
library(TTR)
library(quantmod)
#------------------------------------------------------------------------------
#definimos el ui para la app

ui<-fluidPage( theme = shinytheme("flatly"),
               
               titlePanel("Análisis Técnico en R"),
               
               sidebarLayout(position = "left", 
                             sidebarPanel(
                               
                               selectInput(inputId = "Stock", 
                                           label = h2("Elegir acción:"),
                                           choices = c("NKE","F","TSLA", "WOOF"), selected = "NKE"),#selectinput
                               
                               radioButtons(inputId="period", label=h4("Periodicidad"), 
                                            choices=c("Diario","Semanal","Mensual") ,selected = "Diario"),#radiobutton1
                               
                               radioButtons(inputId="timeframe", label=h4("Periodo de tiempo"), 
                                            choices=c("Este año","Últimos 3 meses","Últimos 6 meses",
                                                      "Último año","Últimos 3 años","Últimos 5 años",
                                                      "Últimos 10 años"),selected = "Este año"),#radiobutton2
                               
                               checkboxGroupInput(inputId = "ind", label = h4("Escoger indicadores a visualizar"), choices = c("VOL","SMA","EMA","RSI","MACD","ATR","BB") )
                             ), #sidebar 
                             
                             mainPanel(
                               
                               h1( textOutput("selected_Stock") ), #header interactua con server
                               plotOutput(outputId = "grafico") #grafica que interactua con server (grafico velas)
                               
                             ) #mainpanel
               )#sidebarlayout
)#fluid page
#-------------------------------------------------------------------------------------
# hacemos parte tecnica de server

server<-function(input,output){
   
  output$selected_Stock <- renderText({ 
    paste("Gráfico ",input$period," de $", input$Stock) #cambia el header
  })
  
   dataInput <- reactive({
     
     
     if(input$period == "Diario"){
       pe<-"daily"
     }else if(input$period == "Semanal"){
       pe<-"weekly"
     }else if(input$period == "Mensual"){
       pe<-"monthly"
     }else{
       pe<-NA
     } # if para determinar el formato de la serie de datos por periodicidad
     
    getSymbols(input$Stock, src = "yahoo",periodicity=pe,
               auto.assign = FALSE)
  })
  
 
  
  output$grafico <- renderPlot({
  
    if(input$timeframe == "Este año"){
      tf<-"last 1 years"
    }else if(input$timeframe == "Últimos 3 años"){
      tf<-"last 3 years"
    }else if(input$timeframe == "Últimos 5 años"){
      tf<-"last 5 years"
    }else if(input$timeframe == "Últimos 3 meses"){
      tf<-"last 3 months"
    }else if(input$timeframe == "Últimos 6 meses"){
      tf<-"last 6 months"
    }else if(input$timeframe == "Últimos 10 años"){
      tf<-"last 10 years"
    }else{ tf<- "2022"}
    
    chartSeries( dataInput() ,typle="candle",
                 subset = tf ,theme=chartTheme("white",up.col="Green",dn.col="Red"),name = input$Stock ,TA=NULL) #la madre del env stock jala los datos
    
  },width = 1200 ,height = 800 ) 
  
}

#corre la app

shinyApp(ui = ui, server = server)
