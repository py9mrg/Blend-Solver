require(shiny)
source('helpers.R')

shinyServer(function(input, output) {
  #This section initiates the example matrices used in description sections
  
  output$Excel_Example<-renderTable({
    tmp<-matrix(c(1,2,4,2,3,5,1.5,2.5,4.5),nrow=3,ncol=3)
    colnames(tmp)<-c('Comp.1','Comp.2','Target')
    rownames(tmp)<-c('Param.1','Param.2','Param.3')
    return(tmp)
    })
  output$Manual_Example<-renderTable({
    tmp<-matrix(c(1,2,4,2,3,5,1.5,2.5,4.5),nrow=3,ncol=3)
    return(tmp)
    },include.rownames=F,include.colnames=F)
  
  output$Example_Equalities<-renderTable({
    matrix(c(1,0,1,0,0,1,0.55,0.45),nrow=2,ncol=4)
    },include.rownames=F,include.colnames=F)
  
  output$Example_Equalities2<-renderTable({
    tmp<-matrix(c(1,0,1,0,0,1,0.55,0.45),nrow=2,ncol=4)
    colnames(tmp)<-c('Comp.1','Comp.2','Comp.3','Proportion')
    return(tmp)
    },include.rownames=F)
  
  output$Example_Constraints<-renderTable({
    tmp<-matrix(c(1,0,0,1,-1,0,1,0,1,0,0,0,1,0,-1,0,0,0,0.2,-0.8),nrow=5,ncol=4)
    colnames(tmp)<-c('Comp.1','Comp.2','Comp.3','Proportion')
    return(tmp)
  },include.rownames=F)
  
  output$Example_Wx<-renderTable({
    tmp<-matrix(c(0.25,0.25,0.25,0.25),nrow=1,ncol=4)
    colnames(tmp)<-c('Comp.1','Comp.2','Comp.3','Comp.4')
    return(tmp)
  },include.rownames=F)
  
  output$Example_Wa<-renderTable({
    tmp<-matrix(c(0.333,0.333,0.333),nrow=3,ncol=1)
    rownames(tmp)<-c('Param.1','Param.2','Param.3')
    return(tmp)
  },include.rownames=T,include.colnames=F)
  
  output$Example_Blend<-renderTable({
    tmp<-matrix(c(0.25,0.25,0.5),nrow=1,ncol=3)
    colnames(tmp)<-c('Comp.1','Comp.2','Comp.3')
    rownames(tmp)<-c('Component Proportions')
    return(tmp)
  })
  
  output$Example_Properties<-renderTable({
    tmp<-matrix(c(2.5,1.5,4.5),nrow=3,ncol=1)
    colnames(tmp)<-c('Resulting Properties')
    rownames(tmp)<-c('Param.1','Param.2','Param.3')
    return(tmp)
  })
  
  
 # This section sets up whether the user inputs data by Excel file upload or manually
  output$choice <- renderUI({ # sets up the input data method based on user choice 
    if(input$radio == "Manual"){
      return(list(numericInput("nrow", "Number of Rows", 3),
      numericInput("ncol", "Number of Columns", 3)))
    }
    else{
      fileInput(
      'file1', 'Choose Excel File')
    }
  })
  
  dataInput <- reactive({ # reads in data file if that's what user choise
    if(input$radio == "File"){
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    read.xls(
      inFile$datapath, sheet = 1, header = T, sep = ",",row.names = 1
    ) #gdata read.xls needed for data input because read.xlsx (xlsx package) reads blank cells. But xlsx needed for writing xlsx files
      }
    })
  
  output$contents <- renderTable({ # show the data table if user imported by file
    dataInput()
  }, rownames=T)
  
  output$inmatrix <- renderUI({ # manual data input
    if(is.null(input$nrow)) return(NULL)
    else{
      if(input$radio == "Manual"){
        matrixInput("tbl", "Enter Data in table below. See left panel (and Introduction) for explanation.", as.data.frame(matrix(
          0,nrow = input$nrow,ncol = input$ncol
        )))
      }
      else{
        return(NULL)
      }
    }
  })
  
  # This section sets up whether the user inputs totals (E, F matrices) by Excel file upload or manually. Really this is just setting equalities, but I can't be bothered to rename
  
  tmp <- reactive({ # sets up the required rows for the manual totals entry
    if(input$radio == "Manual") return(nrow(input$tbl))
    else return(nrow(dataInput()))
  })
  
  tmp2 <- reactive({ # sets up the required columns for the manual totals entry
    if(input$radio == "Manual") return(ncol(input$tbl))
       else return(ncol(dataInput()))
  })
  
  output$choice2 <- renderUI({ # sets up whether to read by file or manually the totals, based on user choice 2
    if(input$radio2 == "Manual"){
      return(numericInput("nrow2", "Number of Equalities", 1))
    }
    else{
      fileInput(
        'file2', 'Choose Excel File')
    }
  })
  
  dataInput2 <- reactive({ # read totals in from file if that is chosen
    if(input$radio2 == "File"){
      inFile <- input$file2
      if (is.null(inFile))
        return(NULL)
      read.xls(
        inFile$datapath, sheet = 1, header = T, sep = ","#,row.names = 1
      ) #gdata read.xls needed for data input because read.xlsx (xlsx package) reads blank cells. But xlsx needed for writing xlsx files
    }
  })
  
  output$contents2 <- renderTable({ # show the read totals
    dataInput2()
  },include.rownames=F)
  
  output$totals <- renderUI({ # do totals manually
    if(is.null(tmp2())) return(NULL)
    else{
      if(input$radio2 == "Manual"){
        matrixInput("totals", "Equalities Input", as.data.frame(matrix(
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
      return(numericInput("nrow3", "Number of Extra Constraints", 0)) # need new number of rows
    }
    else{
      fileInput(
        'file3', 'Choose Excel File')
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
  },include.rownames=F)
  
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
      return(NULL) # Don't need any rows defined here, weights are fixed at 1 row each for Wx, Wa
    }
    else{
      list(helpText("Upload Component Weightings:"),fileInput(
        'file_x', 'Choose Excel File'),helpText("Upload Parameter Weightings:"),fileInput(
          'file_a', 'Choose Excel File'))
    }
  })

  dataInput4_x <- reactive({ # read constraints in from file if that is chosen
    if(input$radio4 == "File"){
      inFile_x <- input$file_x
      if (is.null(inFile_x))
        return(NULL)
      read.xls(
        inFile_x$datapath, sheet = 1, header = T, sep = ","#,row.names = 1
      ) #gdata read.xls needed for data input because read.xlsx (xlsx package) reads blanck cells. But xlsx needed for writing xlsx files
    }
  })
  
  dataInput4_a <- reactive({ # read constraints in from file if that is chosen
    if(input$radio4 == "File"){
      inFile_a <- input$file_a
      if (is.null(inFile_a))
        return(NULL)
      read.xls(
        inFile_a$datapath, sheet = 1, header = F, sep = ",",row.names = 1
      ) #gdata read.xls needed for data input because read.xlsx (xlsx package) reads blanck cells. But xlsx needed for writing xlsx files
    }
  })
  
  output$contents4_x <- renderTable({ # show the read constraints
    dataInput4_x()
  },include.rownames=F)
  
  output$contents4_a <- renderTable({ # show the read constraints
    dataInput4_a()
  },include.rownames=T,include.colnames=F)
  
  output$weights_x <- renderUI({ # do totals manually
    if(is.null(tmp2())) return(NULL)
    else{
      if(input$radio4 == "Manual"){
        matrixInput("weights_x", "Component Weights Input", as.data.frame(matrix(
          format(round(1/(tmp2()-1),digits=3),nsmall=3),nrow = 1,ncol = tmp2()-1
        )))
        
      }
      else{
        return(NULL)
      }
    }
  })
  
  output$weights_a <- renderUI({ # do totals manually
    if(is.null(tmp2())) return(NULL)
    else{
      if(input$radio4 == "Manual"){
        matrixInput("weights_a", "Parameter Weights Input", as.data.frame(matrix(
          format(round(1/(tmp()),digits=3),nsmall=3),ncol = 1,nrow = tmp()
        )))
        
      }
      else{
        return(NULL)
      }
    }
  })

  
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
        Wx<-input$weights_x
      } else{
        Wx<-dataInput4_x()
      }
      if(is.null(dataInput4_a())){
        Wa<-input$weights_a
      } else{
        Wa<-dataInput4_a()
      }
    if(is.null(data) | is.null(totals) | is.null(constraints) | is.null(Wx) | is.null(Wa)){
      return(NULL)
    } else{
      calculate(data,totals,constraints,Wx,Wa)
    }
  })
        
  
  

  output$results <- renderUI({results()})# , include.rownames = FALSE)
  
})