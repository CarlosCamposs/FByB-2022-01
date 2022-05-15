# shiny test

library(shiny)
library(shinythemes)
library(xts)
library(TTR)
library(quantmod)
library(dplyr)
#------------------------------------------------------------------------------
#definimos el ui para la app

ui<-fluidPage( theme = shinytheme("darkly"),
               
               titlePanel("Análisis Técnico en R"),
               
               sidebarLayout(position = "left", 
                             sidebarPanel(
                               
                               selectInput(inputId = "Stock", 
                                           label = h2("Elegir acción:"),
                                           choices = c("NKE","MDLZ","MU", "KMB","T","AMZN"), selected = "NKE"),#selectinput
                               
                               radioButtons(inputId="period", label=h4("Periodicidad"), 
                                            choices=c("Diario","Semanal","Mensual") ,selected = "Diario"),#radiobutton1
                               
                               radioButtons(inputId="timeframe", label=h4("Periodo de tiempo"), 
                                            choices=c("Este año","Últimos 3 meses","Últimos 6 meses",
                                                      "Último año","Últimos 3 años","Últimos 5 años",
                                                      "Últimos 10 años"),selected = "Este año"),#radiobutton2
                               numericInput(inputId = "tsma",label = h5("Periodos SMA") , value = 14 , min = 8, max = 200 ),#num input
                               
                               numericInput(inputId = "tema",label = h5("Periodos EMA") , value = 14 , min = 8, max = 200 ),#num input
                               
                               checkboxGroupInput(inputId = "ind", label = h4("Escoger indicadores a visualizar"),
                                                  choices = c("Volumen","SMA","EMA","ROC","RSI","MACD","ATR","BBands"),selected = NA ),#checkbox
                           width = 2  ), #sidebar 
                             
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
                 subset = tf ,theme=chartTheme("white",up.col="Green",dn.col="Red"),name = input$Stock ,TA=NULL )  #la madre del env stock jala los datos
    
    # if para ir metiendo indicadores
    
    if( "Volumen" %in% input$ind ){
       
    print( addVo() )
    
    }#if

    if("SMA" %in% input$ind){
      print(addSMA(n=input$tsma,col = "Blue"))
    } # los if solo funcionan para el ultimo ciclo wtf
    
    if( "EMA" %in% input$ind ){
      
      print( addEMA(n=input$tema, col="coral4") )
      
    }
    
    if( "ROC" %in% input$ind ){
      
      print( addROC(col = "deeppink") )
      
    }
    
    if( "RSI" %in% input$ind ){
      
      print( addRSI() )
      
    }
    
    if( "MACD" %in% input$ind ){
      
      print( addMACD( col = c("green","red","blue", "black") ) )
      
    }
    
    if( "ATR" %in% input$ind ){
      
      print( addATR() )
      
    }
    
    if( "BBands" %in% input$ind ){
      
      print( addBBands() )
      
    }
    
    
        
    #indicadores<- paste(input$ind, collapse = ", ")
    
    #case_when(
    #  as.logical("Vo" %in% input$ind) ~ print( addVo()),
    #  as.logical("SMA" %in% input$ind)~ print (addSMA())
    #)
    
    
    
    
    },width = 1550 ,height = 900 ) #renderplot 
    
  
}

#corre la app

shinyApp(ui = ui, server = server)
