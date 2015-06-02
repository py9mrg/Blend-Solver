require(shiny)
source('helpers.R')

shinyServer(function(input, output) {
 # This section sets up whether the user inputs data by Excel file upload or manually 
  output$choice <- renderUI({
    if(input$radio == "Manual"){
      return(list(helpText("Say something useful here"),numericInput("nrow", "Number of Rows", 3),
      numericInput("ncol", "Number of Columns", 3)))
    }
    else{
      list(helpText("Say something useful here"),fileInput(
      'file1', 'Choose Excel File'))
    }
  })
  
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
  
  # This section sets up whether the user inputs totals (E, F matrices) by Excel file upload or manually 
  
  tmp2 <- reactive({
    if(input$radio == "Manual") return(ncol(input$tbl))
       else return(ncol(dataInput()))
  })
  
  output$choice2 <- renderUI({
    if(input$radio2 == "Manual"){
      return(list(helpText("Say something useful here"),numericInput("nrow2", "Number of Rows", 1)))
    }
    else{
      list(helpText("Say something useful here"),fileInput(
        'file2', 'Choose Excel File'))
    }
  })
  
  output$totals <- renderUI({
    if(is.null(tmp2())) return(NULL)
    else{
      if(input$radio2 == "Manual"){
        matrixInput("totals", "Totals Input", as.data.frame(matrix(
          0,nrow = input$nrow2,ncol = tmp2()-1
        )))
      }
      else{
        return(NULL)
      }
    }
  })
  
  
  output$totals2 <- renderUI({
    if(is.null(tmp2())) return(NULL)
    else{
      if(input$radio2 == "Manual"){
        matrixInput("totals2", "Totals Input 2", as.data.frame(matrix(
          0,ncol = 1,nrow = input$nrow2
        )))
      }
      else{
        return(NULL)
      }
    }
  })
  
  # THis section does the calculation and outputs the results
  
  results <- eventReactive(input$calculateButton,{ 
    if(input$radio == "Manual"){
      if(is.null(input$tbl)){
        return(NULL)
      } else if(is.null(input$totals)){
        return(NULL)
      } else{
        calculate(input$tbl,E=input$totals,F=input$totals2)
      }
    } else{
    inFile <- input$file1
    if (is.null(inFile)) 
      {
      return(NULL)
    } else if(is.null(input$totals)){
      return(NULL)
    } else{
      calculate(dataInput(),E=input$totals,F=input$totals2)
    }
    }
  })
  
  output$results <- renderTable({results()} , include.rownames = FALSE)
  
})