require(shiny)

shinyUI(fluidPage(
  titlePanel("Uploading Files"),
  sidebarLayout(
    sidebarPanel(
      fileInput('file1', 'Choose Excel File',
                accept=c('text/csv', 
                         'text/comma-separated-values,text/plain', 
                         '.csv')),
      numericInput("nrow", "Number of Rows", 2),
      numericInput("ncol", "Number of Columns", 2)
      ),
    mainPanel(
      tableOutput('contents'),
      tableOutput('results'),
      uiOutput("inmatrix")
    )
    )
  )
)
