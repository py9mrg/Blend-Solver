require(shiny)

shinyUI(fluidPage(
  titlePanel("Uploading Files"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("radio", label = h3("Radio buttons"),
                   choices = list("Manual" = "Manual", "File" = "File"), 
                   selected = "File"),
      uiOutput('choice')
    ),
    mainPanel(
      tableOutput('contents'),
      tableOutput('results'),
      uiOutput("inmatrix")
    )
  )
))
