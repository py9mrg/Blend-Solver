require(shiny)

shinyUI(fluidPage(
  titlePanel("Blend Solver"),
  sidebarLayout(
    sidebarPanel(h3("Data and Target Input"),
                 helpText("Manual or File"),
      radioButtons("radio", label = "Input Data",
                   choices = list("Manual" = "Manual", "File" = "File"), 
                   selected = "File"),
            uiOutput('choice'),
                hr(),
                h3("Define Total"),
                helpText('Help in here about defining totals, e.g. all UK = 55 % and all local = 45 %. That\'s very strict though. Usually just want sum of everything = 1 and use constraints section to set =< values.'),
      radioButtons("radio2", label = "Input Data",
                   choices = list("Manual" = "Manual", "File" = "File"), 
                   selected = "Manual"),
            uiOutput('choice2'),
      actionButton("calculateButton", "Calculate")
    ),
    mainPanel(
      uiOutput('inmatrix'),
      tableOutput('contents'),
      hr(),
      uiOutput('totals'),
      tableOutput('contents2'),
      hr(),
      h3('Results'),
      tableOutput('results')
    )
  )
))
