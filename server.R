require(shiny)
source('helpers.R')

shinyServer(function(input, output) {
  output$contents <- renderTable({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    read.xls(
      inFile$datapath, sheet = 1, header = T, sep = ",",row.names=1
    ) #gdata read.xls needed for data input because read.xlsx (xlsx package) reads blanck cells. But xlsx needed for writing xlsx files
    
  })
  output$results <- renderTable({
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    raw_data <- read.xls(
      inFile$datapath, sheet = 1, header = T, sep = ",",row.names=1
    )
    calculate(raw_data)
  })
})