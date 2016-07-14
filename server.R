
# This is the server logic for the Bayesian von Bertalanffy Growth Model
# Shiny web application.
# 
# Loading library and source code
library(shiny)
source("scriptpopulation.R")
source("scriptindividual.R")
source("fsummary.R")

# Server

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
          head(read.csv(file1$datapath, header=TRUE, sep=input$sep))
    
    })
  
  
  
  
    data<-reactive({
      file1<-input$file
      if(is.null(file1)){return()}
      read.csv(file1$datapath, header=TRUE, sep=input$sep)
    })
  
  
  output$summary <- renderTable({
    
                fsummary(data())
                   
                 })
  
  
  #observeEvent(input$startPoblacional, { 
  #  print(data)
  # f1(data())})
    pobparams <- reactive({
      withProgress(message = 'Running Population Model',
                 detail = 'This may take a while...',value = NULL, {
                   
                   f1(data(),input$iterationNumber,input$linfrange)
                   
                })
  })
  
  output$pobparams <- renderTable({
     if(is.null(data())){return ()}
     pobparams()

    })
  
  # downloadHandler() takes two arguments, both functions.
  # The content function is passed a filename as an argument, and
  #   it should write out data to that filename.
  output$downloadDataPopulation <- downloadHandler(
    
    # This function returns a string which tells the client
    # browser what name to use when saving the file.
    filename = function() {
      file1<-input$file
      if(is.null(file1)){return()}
      paste("population","results",file1$name, sep="_")
      
    },
    
    # This function should write data to a file given to it by
    # the argument 'file'.
    content = function(file) {
      sep <- ";"
      
      
      
      # Write to a file specified by the 'file' argument
      write.table(pobparams(), file, sep = sep, col.names=TRUE,
                  row.names = FALSE)
    })
  
  
 
   output$pob_plot <- renderPlot({
     f3(pobparams(),data())
    })
    


indparams <- reactive({
  withProgress(message = 'Running Individual Model',
               detail = 'This may take a while...',value = 0, {
                
                 f2(data(),input$iterationNumber,input$linfrange)
                 
               })

})

output$indparams <- renderTable({
  if(is.null(data())){return ()}

  indparams()
  
})

output$downloadDataIndividual <- downloadHandler(
  
  # This function returns a string which tells the client
  # browser what name to use when saving the file.
  filename = function() {
    file1<-input$file
    if(is.null(file1)){return()}
    paste("individual","results",file1$name, sep="_")
  },
  
  # This function should write data to a file given to it by
  # the argument 'file'.
  content = function(file) {
    sep <- ";"
    
    # Write to a file specified by the 'file' argument
    write.table(indparams(), file, sep = sep, col.names=TRUE,
                row.names = FALSE)
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
      tabsetPanel(tabPanel("Importing file",h4("Check your data!"),h3("REMEMBER!!! SELECT A OPTIMUM LINF RANGE ACCORDING TO YOUR DATA"), tableOutput("contents"),h4("Summary"),tableOutput("summary")),
                  tabPanel("Population Parameters", tableOutput("pobparams"),   downloadButton('downloadDataPopulation', 'Download Population Results')),tabPanel("Individual Parameters", tableOutput("indparams"), downloadButton('downloadDataIndividual', 'Download Individual Results')),
                  tabPanel("Population Plot",plotOutput(outputId = "pob_plot", height = "500px")),tabPanel("Individual Plot",plotOutput(outputId = "ind_plot", height = "500px")))
    
    })

})

#------------------------------------------------------------
# "Life-history growth-mortality tradeoffs as revealed by 
#  otolith geochemistry in a sedentary coastal fish"

# I.A. Catalan, J. Alos, C. Diaz-Gil, S. Perez, 
# G. Basterretxea, B. Morales-Nin and M. Palmer

# Bayesian von Bertalanffy Growth Model (B-VBGM)
# Model designed and implemented by Miquel Palmer
# Shiny app designed and constructed by Carlos Diaz-Gil and
# Roc Itxart Alba
# 
#
# Mediterranean Institute for Advanced Studies (IMEDEA UIB-CSIC)
# Laboratory of Marine Research and Aquaculture (LIMIA)
#
#------------------------------------------------------------