library(shiny)
library(DT)

rowCallback <- c(
  "function(row, dat, displayNum, index){",
  "  if(dat[1] < 5){",
  "    $('td:eq(1)', row).addClass('red');",
  "  }",
  "}"
)

css <- "
.red {
  background-color: #e34755;
}
table.dataTable tr.selected td.red {
  background-color: #e34755 !important;
}
"

ui <- fluidPage(
  
  tags$head(
    tags$style(HTML(css))
  ),
  
  title = 'Select Table Rows',
  
  fluidRow(
    column(6, DTOutput('x1')),
    column(6, plotOutput('x2', height = 500))
  )
)

server <- function(input, output) {
  
  output$x1 <- renderDT({
    datatable(cars,
              options = list(
                columnDefs = list(list(targets = 3,visible = FALSE)),
                rowCallback = JS(rowCallback)
              )
    )
  })
  
  # highlight selected rows in the scatterplot
  output$x2 <- renderPlot({
    s <- input$x1_rows_selected
    par(mar = c(4, 4, 1, .1))
    plot(cars[ ,-3])
    if (length(s)) points(cars[s, , drop = FALSE], pch = 19, cex = 2)
  })
}

shinyApp(ui, server)
