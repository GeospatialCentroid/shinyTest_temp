#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(sf)
library(dplyr)
library(leaflet)
library(leaflet.extras)
library(shinipsum)
library(shiny)
library(stringr)
library(plotly)
library(DT)
library(vroom)
library(shinythemes)

# addCircleMarkers(
#   data = weather2(),
#   layerId = ~Site,
#   lng = ~ long,
#   lat = ~ lat,
#   radius = if(input$variable %in% c("Snowfall",
#                                     "Snow_depth")) {~sqrt(variable)} else {~variable},
#   color = "black",
#   weight = 5,
#   stroke = TRUE,
#   fillOpacity = 1,
#   fillColor = "black",
#   popup = paste("Station:", weather2()$Site, "<br>",
#                 paste0(input$variable, ":"), weather2()$variable,
#                 if(input$variable %in% c("Precipitation", "Snowfall",
#                                          "Snow_depth")) {"mm"} else {"degrees Celcius"}
#   ),

# https://github.com/ccmothes/poudrePortal



#### feedback 
# 
# nested structure for the indicator selector. 
# 
# add link in the introduction text for the website and the method documentation 
# 
# multiple features showcase links to exteral references 
# 


# source helpers ----------------------------------------------------------
lapply(list.files(path = "src",recursive = TRUE, full.names = TRUE), source)


# enviroscreen data
envoData <- readRDS("data/scores/allScores.rda")

# addational Data 
oil <- readRDS("data/scores/oil.rda")
coal <- readRDS("data/scores/coal.rda")
rural <- readRDS("data/scores/rural.rda")

# di community 
di <- getDI()

# purple high
colorRamp <- c(
  "#fcfbfd",
  "#efedf5",
  "#dadaeb",
  "#bcbddc",
  "#9e9ac8",
  "#807dba",
  "#6a51a3",
  "#54278f",
  "#3f007d")


# create initial dataset for map  -----------------------------------------
mapData <- initialMapData(envoData)
# palette for the map
palMap <- leaflet::colorNumeric(palette = colorRamp,
    domain = mapData$`Colorado Enviroscreen Score_pcntl`,
    reverse = TRUE
  )

# unique Indicators
indicators <- sf::st_drop_geometry(envoData) %>%
  dplyr::select(-c(area, GEOID)) %>%
  dplyr::select(!ends_with("_Pctl")) %>%
  dplyr::select(!ends_with("_pcntl")) %>%
  names()

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("sandstone"),

  # nav panel ---------------------------------------------------------------
  # not sure if it's needed but probably... 
  
  # Title ------------------------------------------------------------------
  fluidRow(
    includeCSS("www/banner.css"),
    HTML(
      '<header id="showcase">
    <h1>Colorado Enviroscreen</h1>
    <p>Mapping Health Equity in Colorado</p>
    </header>')
  ),

  fluidRow(
    h2("text describing the project"),
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
  fluidRow(style = {"border-style: solid; borderColor=:#4d3a7d;"},
    # action button 
    column(
      2,
      actionButton("button", "Update Map")
    ),
    # select geography
    column(
      2,
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
      3,
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
    ),
    # add DI Communities 
    column(
      3,
      selectInput(
        inputId = "addDI",
        label = "Disproportionally Impacted Communities",
        choices = c("Add to Map", "Remove from Map"),
        selected = "Remove from Map"
      )
    )
  ),


  # display map -------------------------------------------------------------
  fluidRow(style = "background-color:#4d3a7d;",
           tags$style(type = "text/css", "#mymap {height: calc(100vh - 80px) !important;}"),
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
server <- function(input, output,session) {

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

  # generate map ------------------------------------------------------------
  ### tie all this into an external function just to clean up the server script. I want the 
  ### server to be focused on reactive coded not the static stuff. 
  output$mymap <- renderLeaflet({
    createMap(mapData = mapData, pal = colorRamp, palMap = palMap,oil=oil, rural = rural, coal = coal)
  
    })

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
  

# proxy map elements  -----------------------------------------------------
  filteredData <- eventReactive(input$button,{
    d1 <- envoData %>%
      filter(area %in% input$Geom)
  })

  observe({
    leafletProxy("mymap", data = filteredData()) %>%
      removeShape(layerId = mapData$GEOID) %>%
      addPolygons(
        data = filteredData(),
        group = "Indicator Score",
        color = "#454547"
        )
  })
}

# Run the application
shinyApp(ui = ui, server = server)




# # update map
# indicator <- eventReactive(input$button,{
#   # contruct indicator name 
#   
#   if(input$Percentile == "Measure Value"){
#     indicate <- input$Indicator 
#   }else{
#     indicate <- paste0(input$Indicator,"_pcntl")
#   }
# }
# )
# 
# filteredData <- eventReactive(input$button,{
#   d1 <- envoData %>%
#     filter(area %in% input$Geom)%>%
#     dplyr::select(GEOID, indicator())
# })
# 
# 
# # generate palette 
# palette1 <- reactive({
#   palMap <- leaflet::colorNumeric(palette = colorRamp,
#                                   domain = filteredData()[,indicator()],
#                                   reverse = TRUE
#   )
# })


# observe({
#   add_di <- input$addDI
#   proxy <- leafletProxy("mymap")
#   if(add_di == "Add to Map"){
#     proxy %>% addPolygons(
#       data = di,
#       layerId =  "Disproportionally Impacted Community",
#       stroke = TRUE,
#       color = "#51F0CD",
#       weight = 1,
#       group =  "Disproportionally Impacted Community"
#     )
#   }else{
#     proxy %>% removeMarker(layerId="Disproportionally Impacted Community")
#   }
# })
# 


# diCommunity <- readRDS("data/scores/diCommunities.rda")%>%
#   mutate(
#     Mn_FLAG = case_when(
#       Mn_FLAG == 1 ~ "Yes",
#       Mn_FLAG == 0 ~ "No"
#     ),
#     FLP_FLA = case_when(
#       FLP_FLA == 1 ~ "Yes",
#       FLP_FLA == 0 ~ "No"
#     ),
#     Br_FLAG = case_when(
#       Br_FLAG == 1 ~ "Yes",
#       Br_FLAG == 0 ~ "No"
#     )
#     
#   )%>%
#   mutate(popup =
#            paste0(
#              "<br/><h3>Disproportionally Impacted Community: </h3>",
#              "<br/><b>Census Block Group: </b>", GEOID,
#              "<br/>",
#              "<br/><b>40% of Households are Low Income: </b>", FLP_FLA,
#              "<br/><b>Percent Low Income: </b>", Pov_PCT,
#              "<br/>",
#              "<br/><b>40% of Households are Minority : </b>", Mn_FLAG,
#              "<br/><b>Percent Minority: </b>", Min_PCT,
#              "<br/>",
#              "<br/><b>40% of Households are Housing Burdened : </b>", Br_FLAG,
#              "<br/><b>Percent Housing Burdened: </b>", HH_Br_P
#            )
#            )
