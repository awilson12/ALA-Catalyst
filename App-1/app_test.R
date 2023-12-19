require(shiny)
require(shinydashboard)

ui <- fluidPage(
  fluidRow(
    column(12, actionButton("update", "Update"))
  ),
  fluidRow(
    column(12, echarts4rOutput("plot"))
  )
)

server <- function(input, output){
  data <- eventReactive(input$update, {
    Sys.sleep(1) # sleep one second to show loading
    data.frame(
      x = 1:10,
      y = rnorm(10)
    )
  })
  
  output$plot <- renderEcharts4r({
    data() |> 
      e_charts(x) |> 
      e_bar(y) |> 
      e_show_loading()
  })
}

shinyApp(ui, server)