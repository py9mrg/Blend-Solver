require(shiny)

shinyUI(fluidPage(
  titlePanel("Uploading Files"),
  sidebarLayout(
    sidebarPanel(
      fileInput('file1', 'Choose Excel File',
                accept=c('text/csv', 
                         'text/comma-separated-values,text/plain', 
                         '.csv'))
      ),
    mainPanel(
      tableOutput('contents'),
      tableOutput('results')
    )
  )
))
