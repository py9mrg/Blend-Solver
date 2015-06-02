require(shiny)
source('helpers.R')

shinyServer(function(input, output) {
  #This section initiates the example matrices used in description sections
  
  output$example1<-renderTable({matrix(1,nrow=2,ncol=2)})
  
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
  
  # This section sets up whether the user inputs totals (E, F matrices) by Excel file upload or manually. Really this is just setting equalities, but I can't be bothered to rename
  
  tmp2 <- reactive({ # sets up the required columns for the manual totals entry
    if(input$radio == "Manual") return(ncol(input$tbl))
       else return(ncol(dataInput()))
  })
  
  output$choice2 <- renderUI({ # sets up whether to read by file or manually the totals, based on user choice 2
    if(input$radio2 == "Manual"){
      return(list(helpText("Say something useful here"),numericInput("nrow2", "Number of Equalities", 1)))
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
          1,nrow = input$nrow2,ncol = tmp2()
        )))
        
      }
      else{
        return(NULL)
      }
    }
  })
  
  # This section sets up the various constraints that may be entered into the solver.
  
  output$choice3 <- renderUI({ # sets up whether to read by file or manually the constraints, based on user choice 2
    if(input$radio3 == "Manual"){
      return(list(helpText("Say something useful here"),numericInput("nrow3", "Number of Extra Constraints", 0))) # need new number of rows
    }
    else{
      list(helpText("Say something useful here"),fileInput(
        'file3', 'Choose Excel File'))
    }
  })
  
  dataInput3 <- reactive({ # read constraints in from file if that is chosen
    if(input$radio3 == "File"){
      inFile <- input$file3
      if (is.null(inFile))
        return(NULL)
      read.xls(
        inFile$datapath, sheet = 1, header = T, sep = ","#,row.names = 1
      ) #gdata read.xls needed for data input because read.xlsx (xlsx package) reads blanck cells. But xlsx needed for writing xlsx files
    }
  })
  
  output$contents3 <- renderTable({ # show the read constraints
    dataInput3()
  })
  
  output$constraints <- renderUI({ # do constraints manually
    if(is.null(tmp2())) return(NULL)
    else{
      if(input$radio3 == "Manual"){
        if(input$radio == "File"){
        matrixInput("constraints", "Constraints Input", as.data.frame(
          cbind(rbind(diag(ncol(dataInput())-1),matrix(0,nrow=(input$nrow3),ncol=ncol(dataInput())-1)),seq(0,0,length=(ncol(dataInput())-1+input$nrow3))) # will have same number of columns and rows as input data, but different rows
        ))
        } else{
          matrixInput("constraints", "Constraints Input", as.data.frame(
            cbind(rbind(diag(ncol(input$tbl)-1),matrix(0,nrow=(input$nrow3),ncol=ncol(input$tbl)-1)),seq(0,0,length=(ncol(input$tbl)-1+input$nrow3))) # will have same number of columns and rows as input data, but different rows
          ))
        }
      } else{
        return(NULL)
      }
    }
  })
  
  # This section sets up any weights the user might desire.
  
  output$choice4 <- renderUI({ # sets up whether to read by file or manually the constraints, based on user choice 2
    if(input$radio4 == "Manual"){
      return(helpText("Say something useful here")) # Don't need any rows defined here, weights are fixed at 1 row each for Wx, Wa
    }
    else{
      list(helpText("Say something useful here x"),fileInput(
        'file_x', 'Choose Excel File'),helpText("Say something useful here a"),fileInput(
          'file_a', 'Choose Excel File'))
    }
  })

  dataInput4_x <- reactive({ # read constraints in from file if that is chosen
    if(input$radio4 == "File"){
      inFile_x <- input$file_x
      if (is.null(inFile_x))
        return(NULL)
      read.xls(
        inFile_x$datapath, sheet = 1, header = T, sep = ",",row.names = 1
      ) #gdata read.xls needed for data input because read.xlsx (xlsx package) reads blanck cells. But xlsx needed for writing xlsx files
    }
  })
  
  dataInput4_a <- reactive({ # read constraints in from file if that is chosen
    if(input$radio4 == "File"){
      inFile_a <- input$file_a
      if (is.null(inFile_a))
        return(NULL)
      read.xls(
        inFile_a$datapath, sheet = 1, header = T, sep = ",",row.names = 1
      ) #gdata read.xls needed for data input because read.xlsx (xlsx package) reads blanck cells. But xlsx needed for writing xlsx files
    }
  })
  
  output$contents4_x <- renderTable({ # show the read constraints
    dataInput4_x()
  })
  
  output$contents4_a <- renderTable({ # show the read constraints
    dataInput4_a()
  })
  
#   output$weights <- renderUI({ # do totals manually
#     if(is.null(tmp2())) return(NULL)
#     else{
#       if(input$radio4 == "Manual"){
#         matrixInput("weights", "Weights Input", as.data.frame(matrix(
#           1,nrow = 2,ncol = tmp2()-1 # number of components only required for this table - no equalities/targets used.
#         )))
#         
#       }
#       else{
#         return(NULL)
#       }
#     }
#   })
  
  # This section does all the calculations and renders the results.
  
  results<-eventReactive(input$calculateButton,{
      if(is.null(dataInput2())){ # if you decide to add in a default option instead of explicitly showing the default, then "switch" will be useful here.
        totals<-input$totals
      } else{
        totals<-dataInput2()
      }
      if(is.null(dataInput())){
        data<-input$tbl
      } else{
        data<-dataInput()
      }
      if(is.null(dataInput3())){
        constraints<-input$constraints
      } else{
        constraints<-dataInput3()
      }
      if(is.null(dataInput4_x())){
        Wx<-input$Wx
      } else{
        Wx<-dataInput4_x()
      }
      if(is.null(dataInput4_a())){
        Wa<-input$Wa
      } else{
        Wa<-dataInput4_a()
      }
    if(is.null(data) | is.null(totals) | is.null(constraints) | is.null(Wx) | is.null(Wa)){
      return(NULL)
    } else{
      calculate(data,totals,constraints,Wx,Wa)
    }
  })
        
  
  

  output$results <- renderTable({results()} , include.rownames = FALSE)
  
})