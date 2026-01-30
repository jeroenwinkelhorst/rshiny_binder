library(shiny)

ui <- fluidPage(
  titlePanel("Hello from Shiny on JupyterHub"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins", "Number of bins:", min = 5, max = 50, value = 20),
      selectInput("var", "Variable:", choices = names(mtcars), selected = "mpg")
    ),
    mainPanel(
      plotOutput("hist"),
      verbatimTextOutput("summary")
    )
  )
)

server <- function(input, output, session) {
  output$hist <- renderPlot({
    x <- mtcars[[input$var]]
    hist(x, breaks = input$bins, main = paste("Histogram of", input$var), xlab = input$var)
  })
  output$summary <- renderPrint({
    summary(mtcars[[input$var]])
  })
}

shinyApp(ui, server)