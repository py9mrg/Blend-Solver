require(shiny)
source('helpers.R')

shinyServer(function(input, output) {
 # This section sets up whether the user inputs data by Excel file upload or manually
  output$choice <- renderUI({ # sets up the input data method based on user choice 
    if(input$radio == "Manual"){
      return(list(helpText("Say something useful here"),numericInput("nrow", "Number of Rows", 3),
      numericInput("ncol", "Number of Columns", 3)))
    }
    else{
      list(helpText("Say something useful here"),fileInput(
      'file1', 'Choose Excel File'))
    }
  })
  
  dataInput <- reactive({ # reads in data file if that's what user choise
    if(input$radio == "File"){
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    read.xls(
      inFile$datapath, sheet = 1, header = T, sep = ",",row.names = 1
    ) #gdata read.xls needed for data input because read.xlsx (xlsx package) reads blanck cells. But xlsx needed for writing xlsx files
      }
    })
  
  output$contents <- renderTable({ # show the data table if user imported by file
    dataInput()
  })
  
  output$inmatrix <- renderUI({ # manual data input
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
  
  tmp2 <- reactive({ # sets up the required columns for the manual totals entry
    if(input$radio == "Manual") return(ncol(input$tbl))
       else return(ncol(dataInput()))
  })
  
  output$choice2 <- renderUI({ # sets up whether to read by file or manually the totals, based on user choice 2
    if(input$radio2 == "Manual"){
      return(list(helpText("Say something useful here"),numericInput("nrow2", "Number of Rows", 1)))
    }
    else{
      list(helpText("Say something useful here"),fileInput(
        'file2', 'Choose Excel File'))
    }
  })
  
  dataInput2 <- reactive({ # read totals in from file if that is chosen
    if(input$radio2 == "File"){
      inFile <- input$file2
      if (is.null(inFile))
        return(NULL)
      read.xls(
        inFile$datapath, sheet = 1, header = T, sep = ","#,row.names = 1
      ) #gdata read.xls needed for data input because read.xlsx (xlsx package) reads blanck cells. But xlsx needed for writing xlsx files
    }
  })
  
  output$contents2 <- renderTable({ # show the read totals
    dataInput2()
  })
  
  output$totals <- renderUI({ # do totals manually
    if(is.null(tmp2())) return(NULL)
    else{
      if(input$radio2 == "Manual"){
        matrixInput("totals", "Totals Input", as.data.frame(matrix(
          0,nrow = input$nrow2,ncol = tmp2()
        )))
        
      }
      else{
        return(NULL)
      }
    }
  })
  

  # THis section does the calculation and outputs the results - will need to rewrite this to separate out the options
  
  results<-eventReactive(input$calculateButton,{
      if(is.null(dataInput2())){
        totals<-input$totals
        print(totals)
      } else{
        totals<-dataInput2()
        print(totals)
      }
      if(is.null(dataInput())){
        data<-input$tbl
      } else{
        data<-dataInput()
      }
    if(is.null(data) | is.null(totals)){
      return(NULL)
    } else{
      calculate(data,totals)
    }
  })
        
  
  
#   results <- eventReactive(input$calculateButton,{
#     if(input$radio == "Manual"){
#       if(is.null(input$tbl)){
#         return(NULL)
#       } else if(is.null(input$totals)){
#         return(NULL)
#       } else{
#         calculate(input$tbl,totals=input$totals)
#       }
#     } else{
#     inFile <- input$file1
#     if (is.null(inFile)) 
#       {
#       return(NULL)
#     } else if(is.null(input$totals)){
#       return(NULL)
#     } else{
#       calculate(dataInput(),totals=dataInput2())
#     }
#     }
#   })
  
  output$results <- renderTable({results()} , include.rownames = FALSE)
  
})