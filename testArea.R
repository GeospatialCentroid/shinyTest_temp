### specifically evaluating the select process in shinyt 



library(pacman)
pacman::p_load("sf", "dplyr", "leaflet","shinipsum", "shiny", "stringr",
               "plotly", "DT", "vroom")


# enviroscreen data 
counties <- sf::st_read("data/geometries/county/coloradoCounties.geojson")
countyScore <- as_tibble(vroom("data/scores/county.csv"))%>% 
  dplyr::mutate(geom = "County")


# connect datasets 
county1 <- dplyr::left_join(x = counties, y = countyScore, by ="GEOID")
# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Colorado Enviroscreen"),
  # content for the reactive elements of the map 
  fluidRow(
    # select geography 
    column(4,
           selectInput(
             inputId = "geom",
             label = "Select Geographic Scale", 
             choices = c("County","Census Tract", "Census Block Group"),
             selected = "County",
             width = '90%'
           )
    ),
    # select indicator 
    column(4,
           selectInput(
             inputId = "Indicator",
             label = "Select Layer for Map",
             choices = indicators,
             selected = "envExp",
             width = '90%'
           )
    ),
    # toggle between measured and percentile 
    column(2, 
           selectInput(
             inputId = "Percentile",
             label = "Measure or Percentile",
             choices = c("Measured Value", "Percentile Rank"),
             selected = "Percentile Rank"
           )    
           
    )
  ),
  fluidRow(
    column(10,
           dataTableOutput("data_table")      
    )
  )
)


server <- function(input, output) {
  # get df from spatial object 
  df1 <- reactive({
    countyScore %>%
      filter(`geom` == input$geom)
    })
  
  # output for datatable
  output$data_table <- renderDataTable({
    df1()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

