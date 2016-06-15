
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
#setwd("D:/Shinny/Fishgrowth/")
library(shiny)
source("script.R")
source("scriptindividual.R")


shinyServer(function(input, output) {

  
  output$contents <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    
    file1 <- input$file
    
    if (is.null(file1))
      return(NULL)
    
  head(read.csv(file1$datapath, header=input$header, sep=input$sep))
  })
  
  
  
  
  data<-reactive({
    file1<-input$file
    if(is.null(file1)){return()}
    read.csv(file1$datapath, header=input$header, sep=input$sep)
  })
  
    
  
  #observeEvent(input$startPoblacional, { 
  #  print(data)
  # f1(data())})
  pobparams <- reactive({
    withProgress(message = 'LOADING...',
                 detail = 'This may take a while...',value = NULL, {
                   f1(data(),input$iterationNumber,input$linfrange)
                  })
    
  })
  
  output$pobparams <- renderTable({
     if(is.null(data())){return ()}
     pobparams()[c(1:2,19),]

    })
  

  
   output$pob_plot <- renderPlot({
     f3(pobparams(),data())
    })
    
      
 # observeEvent(input$startIndividual, { 
#    print(data)
#    f2(data())
#  })

indparams <- reactive({
  withProgress(message = 'Calculation in progress',
               detail = 'This may take a while...',value = 0, {
                 f2(data(),input$iterationNumber,input$linfrange)
               })

})

output$indparams <- renderTable({
  if(is.null(data())){return ()}

  indparams()#[c(1:2,19),]
  
})



output$ind_plot <- renderPlot({
  f4(indparams(),data())
})



  # the following renderUI is used to dynamically generate the tabsets when the file is loaded. Until the file is loaded, app will not show the tabset.
output$activeTab <- reactive({
  return(input$tab)
})
outputOptions(output, 'activeTab', suspendWhenHidden=FALSE) 
output$tb <- renderUI({
    if(is.null(data()))
      h5("Powered by", tags$img(src="logo.png", heigth=300, width=300))
    else
      tabsetPanel(tabPanel("Importing file", tableOutput("contents")),
                  tabPanel("Population Parameters", #h4("Patience this may take a while... do not switch tabs!"),

                           tableOutput("pobparams")),tabPanel("Individual Parameters", tableOutput("indparams")),
                  tabPanel("Population Plot",plotOutput(outputId = "pob_plot", height = "500px")),tabPanel("Individual Plot",plotOutput(outputId = "ind_plot", height = "1000px")))
    
    })

})

