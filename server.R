require(shiny)
source('helpers.R')

shinyServer(function(input, output) {
  
  output$choice <- renderUI({
    if(input$radio == "Manual"){
      return(list(numericInput("nrow", "Number of Rows", 3),
      numericInput("ncol", "Number of Columns", 3)))
    }
    else{
      fileInput(
      'file1', 'Choose Excel File')
    }
  })
  
  output$value <- renderPrint({ input$radio })
  dataInput <- reactive({
    if(input$radio == "File"){
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    read.xls(
      inFile$datapath, sheet = 1, header = T, sep = ",",row.names = 1
    ) #gdata read.xls needed for data input because read.xlsx (xlsx package) reads blanck cells. But xlsx needed for writing xlsx files
      }
    })
  
  output$contents <- renderTable({
    dataInput()
  })
  
  output$results <- renderTable({
    if(input$radio == "Manual"){
      calculate(input$tbl)
    }
    else{
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    calculate(dataInput())
    }
  }
    , include.rownames = FALSE)
  
  output$inmatrix <- renderUI({
    if(is.null(input$nrow)) return(NULL)
    else{
    if(input$radio == "Manual"){
    matrixInput("tbl", "Enter Data", as.data.frame(matrix(
      0,nrow = input$nrow,ncol = input$ncol
    )))
    }
    else{
      return(NULL)
    }
    }
  })
  
})