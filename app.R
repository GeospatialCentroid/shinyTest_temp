#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(pacman)
pacman::p_load(
  "sf", "dplyr", "leaflet", "shinipsum", "shiny", "stringr",
  "plotly", "DT", "vroom", "feather", "sfarrow"
)
# enviroscreen data
envoData <- sfarrow::st_read_feather("data/scores/allScores.feather")

# purple high
colorRamp <- c(
  "#00441b", "#1b7837", "#5aae61", "#a6dba0", "#d9f0d3", "#e7d4e8",
  "#c2a5cf", "#9970ab", "#762a83", "#40004b"
)

# palMap <- colorNumeric(
#   palette = colorRamp,
#   domain = vals,
#   reverse = FALSE
# )

# unique Indicators
indicators <- sf::st_drop_geometry(envoData) %>%
  dplyr::select(-c(area, GEOID)) %>%
  dplyr::select(!ends_with("_Pctl")) %>%
  dplyr::select(!ends_with("_pcntl")) %>%
  names()
## currently the period is in the name, due to the file type.... need


# Define UI for application that draws a histogram
ui <- fluidPage(

  # Title -------------------------------------------------------------------
  # Application title
  titlePanel("Colorado Enviroscreen"),
  # Sidebar with a slider input for number of bins
  fluidRow(
    h2("landing page image with text overlay"),
    # Image
    img(src = "rstudio.png", height = 140, width = 400),
    # text describing the process
    h2("text describing the process"),
    p(random_text(nwords = 400))
  ),


  # description of use ------------------------------------------------------
  fluidRow(
    column(
      6,
      h2("image showing examples of how to use the resource "),
      plotOutput("image", height = "300px"),
    ),
    column(
      6,
      h2("supporting text for the image "),
      p(random_text(nwords = 400))
    )
  ),


  # Select Reactive Elements ------------------------------------------------
  # content for the reactive elements of the map
  fluidRow(
    # select geography
    column(
      4,
      selectInput(
        inputId = "Geom",
        label = "Select Geographic Scale",
        choices = c("County", "Census Tract", "Census Block Group"),
        selected = "County",
        width = "90%"
      )
    ),
    # select indicator
    column(
      4,
      selectInput(
        inputId = "Indicator",
        label = "Select Layer for Map",
        choices = indicators,
        selected = "envExp",
        width = "90%"
      )
    ),
    # toggle between measured and percentile
    column(
      2,
      selectInput(
        inputId = "Percentile",
        label = "Measure or Percentile",
        choices = c("Measured Value", "Percentile Rank"),
        selected = "Measured Value"
      )
    )
  ),


  # display map -------------------------------------------------------------
  fluidRow(
    leafletOutput("mymap")
  ),

  # describe indicators -----------------------------------------------------
  # sentence explaining the indicators
  h2("two sentences that change based on idicator selection"),
  textOutput("indicatorDesc"),
  br(),

  # show plots --------------------------------------------------------------
  # plot of the datasets
  fluidRow(
    h2("Histograph of 5 component scores. Reactive on the map selection"),
    plotlyOutput("graph")
  ),
  br(),

  # Describe plots  --------------------------------------------------------
  # paragraphy explaining the plot
  h2("text describing the plot"),
  p(random_text(nwords = 80)),


  # show reactive table -----------------------------------------------------
  # table showing the results
  fluidRow(
    h2("Reactive table based on geography selection"),
    column(
      10,
      dataTableOutput("data_table")
    )
  ),

  # print statement for trouble shooting
  fluidRow(
    column(
      12,
      textOutput("test1")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  # intro image -------------------------------------------------------------
  # image output
  output$image <- renderImage({
    random_image()
  })

  # reactive geometry selection --------------------------------------------------
  # select table
  df1 <- reactive({
    envoData %>%
      dplyr::filter(area == input$Geom)
  })


  # reactive indicator selection --------------------------------------------
  # grab options for indicator
  indicator1 <- reactive({
    input$Indicator
  })
  indicator2 <- reactive({
    paste0(indicator1(), "_pcntl")
  })
  # # grab specific element
  selection <- reactive({
    ifelse(
      test = input$Percentile == "Measured Value",
      yes = indicator1(),
      no = indicator2()
    )
  })


  # troubleshooting print statement -----------------------------------------
  # output$test1 <- renderPrint(selection())

  # filter for map visualization
  # join with spatial data based on the geom column
  spatData <- reactive({
    df1() %>%
      dplyr::select(selection())
  })


  # create palettes for map --------------------------------------------------------
  # generate a palette with the indicator
  values2 <- reactive({
    spatData() %>%
      sf::st_drop_geometry() %>%
      pull()
  })
  # palette for the map
  palMap <- reactive({
    leaflet::colorNumeric(
      palette = colorRamp,
      domain = values2(),
      reverse = FALSE
    )
  })
  # palette for the legend
  palLen <- reactive({
    colorNumeric(
      palette = colorRamp,
      domain = values2(),
      reverse = TRUE
    )
  })
  

# custom popup ------------------------------------------------------------
  # spatData2 <- reactive({
  #   spatData() %>% 
  #   dplyr::mutate(
  #     popup = paste0(
  #       # "<b>", paste0(GEOID," County"),"</b>",
  #       "<br/><i>", as.character(selection()),"</i>", # needs to be text
  #       # "<br/><b>Measured:</b> ", !!as.symbol(indicator1),
  #       # "<br/><b>Percentile:</b> ", !!as.symbol(indicator2)
  #     )
  #   )})
  # 
  # generate map ------------------------------------------------------------
  output$mymap <- renderLeaflet(
    leaflet() %>%
      addTiles() %>%
      addPolygons(
        data = spatData(),
        color = "#454547",
        weight = 1,
        smoothFactor = 0.5,
        opacity = 1.0, fillOpacity = 0.5,
        fillColor = palMap()(values2()), # https://stackoverflow.com/questions/48953149/dynamic-color-fill-for-polygon-using-leaflet-in-shiny-not-working
        highlightOptions = highlightOptions(
          color = "white",
          weight = 2,
          bringToFront = TRUE
        ),
         popup = "need to make reactive"
      ) %>%
      leaflet.extras::addSearchOSM()
  )

# indicator summary -------------------------------------------------------
  # output for indicator summary
  output$indicatorDesc <- renderText({
    paste0(input$Indicator, " : ", shinipsum::random_text(nwords = 40))
  })


# histogram plots ---------------------------------------------------------
  # output for ployly
  output$graph <- renderPlotly({
    random_ggplotly()
  })

# table output ------------------------------------------------------------   
  # output for datatable
  output$data_table <- renderDataTable(
    df1() %>% sf::st_drop_geometry(), 
    options = list(fillContainer = TRUE,)
  )
}

# Run the application
shinyApp(ui = ui, server = server)





### this works but lets try something different
# create the map to start visualization with stock features define in code
# before the ui
# output$mymap <- renderLeaflet(
#   leaflet() %>%
#     addTiles()%>%
#     addPolygons(
#       data = county1,
#       color = "#454547",
#       weight = 1,
#       smoothFactor = 0.5,
#       opacity = 1.0, fillOpacity = 0.5,
#       fillColor =  ~palMap(vals),
#       highlightOptions = highlightOptions(color = "white",
#                                           weight = 2,
#                                           bringToFront = TRUE),
#       popup = "county1"
#       #group = "Indicator Score",
#       #options = pathOptions(pane = "index"))
#     )%>%
#     leaflet.extras::addSearchOSM()
# )


# proxy for chaging visualization
# set reactive features based on
# observe({
#   # determine Indicator
#   indicator <- reactive({
#     input$Indicator
#   })
# })
# output$test1 <- renderText(names(geom1))
# if(geom1 == "County"){
#   data1 <- counties
#
# elseif(geom1 == "Census Tracts"){
#   data1 <- censusTracts
# }else{
#   data1 <- censusBlockGroups
# }


# indicator1 <- indicator
# indicator2 <- paste0(indicator1,"_pcntl")
#
# # determine percentile rank or measured
# indicatorType <- reactive({
#   input$Percentile
# })
# selection <- ifelse(
#   test = indicatorType == "Measured Value",
#   yes = indicator1,
#   no = indicator2
# )
#
# values2 <- data1 %>%
#   dplyr::select(selection)%>%
#   sf::st_drop_geometry()%>%
#   pull()
#
# # palette for the map
# palMap <- colorNumeric(
#   palette = colorRamp,
#   domain = values2,
#   reverse = FALSE
# )
# #palette for the legend
# palLen <- colorNumeric(
#   palette = colorRamp,
#   domain = values2,
#   reverse = TRUE
# )

# leafletProxy("mymap", data = data1) %>%
#   clearShapes() %>%
#   addPolygons(
#     color = "#454547",
#     weight = 1,
#     smoothFactor = 0.5,
#     opacity = 1.0, fillOpacity = 0.5,
#     fillColor =  ~palMap(values2),
#     highlightOptions = highlightOptions(color = "white",
#                                         weight = 2,
#                                         bringToFront = TRUE),
#     #popup = ~popup,
#     #group = "Indicator Score",
#     #options = pathOptions(pane = "index"))
#   )
#
#     })
#
