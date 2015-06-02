require(shiny)

shinyUI(fluidPage(
  titlePanel("Blend Solver"),
  sidebarLayout(
    sidebarPanel(h3("Data and Target Input"),
                 helpText("Manual or File - mention about deleting rows/columns if blanks show up in imported data"),
      radioButtons("radio", label = "Input Data",
                   choices = list("Manual" = "Manual", "File" = "File"), 
                   selected = "File"),
            uiOutput('choice'),
                hr(),
                h3("Define Totals"),
                helpText('Help in here about defining totals, e.g. all UK = 55 % and all local = 45 %. That\'s very strict though. Usually just want sum of everything = 1 and use constraints section to set =< values. Leave in Manual for default'),
      radioButtons("radio2", label = "Input Totals",
                   choices = list("Manual" = "Manual", "File" = "File"), 
                   selected = "Manual"),
            uiOutput('choice2'),
      hr(),
      h3("Define Constraints"),
      helpText("Help here about defining constraints - going to be tricky as may need example matrices. Leave in Manual for default"),
      radioButtons("radio3", label = "Input Constraints",
                   choices = list("Manual" = "Manual", "File" = "File"), 
                   selected = "Manual"),
            uiOutput('choice3'),
      hr(),
      h3("Define Weights"),
      helpText("Help here about defining weights on either the parameters, components, or both. That Wx = number of components, Wa number of params"),
      radioButtons("radio4", label = "Input Weights",
                   choices = list("Manual" = "Manual", "File" = "File"), 
                   selected = "Manual"),
      uiOutput('choice4'),
      actionButton("calculateButton", "Calculate")
    ),
    mainPanel(
      uiOutput('inmatrix'),
      tableOutput('contents'),
      hr(),
      uiOutput('totals'),
      tableOutput('contents2'),
      hr(),
      uiOutput('constraints'),
      tableOutput('contents3'),
      hr(),
      #uiOutput('weights'),
      tableOutput('contents4_x'),
      tableOutput('contents4_a'),
      
      hr(),
      h3('Results'),
      tableOutput('results')
    )
  )
))
