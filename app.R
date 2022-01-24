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
library(bslib)

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
envoData <- readRDS("data/scores/allScores.rda")%>%
  dplyr::mutate(visParam = `Colorado Enviroscreen Score_pcntl`)

# addational Data 
oil <- readRDS("data/scores/oil.rda")
coal <- readRDS("data/scores/coal.rda")
rural <- readRDS("data/scores/rural.rda")

# di community 
di <- getDI()

# purple high
colorRamp <- c("#f2f0f7"  ,"#cbc9e2"  ,"#9e9ac8"  ,"#756bb1"  ,"#54278f")

# create initial dataset for map  -----------------------------------------
mapData <- initialMapData(envoData)
# palette for the map
palMap <- leaflet::colorNumeric(palette = colorRamp,
                                domain = mapData$visParam,
                                reverse = TRUE)

# unique Indicators
indicators <- sf::st_drop_geometry(envoData) %>%
  dplyr::select(-c(area, GEOID)) %>%
  dplyr::select(!ends_with("_Pctl")) %>%
  dplyr::select(!ends_with("_pcntl")) %>%
  names()

# Define UI for application that draws a histogram
ui <- fluidPage( 
  theme = bslib::bs_theme(
  bootswatch = "flatly",
  #bg = "#FFFFFF",
  #fg = "#000",
  primary = "#186D03",
  secondary = "#DD5B27",
  success = "#f28e35",
  base_font = "Trebuchet MS,Helvetica,sans-serif",
  heading_font = "museo-sans,sans-serif"
  )%>%
  bslib::bs_add_rules(sass::sass_file("www/style.scss")),
  # # nav panel ---------------------------------------------------------------
  # not sure if it's needed but probably... 
  
  # Title ------------------------------------------------------------------
  shiny::titlePanel(h1("Colorado Enviroscreen")),
  
  fluidRow(
  
  p("The Colorado Department of Public Health and Environment and a team at Colorado State University are working on an enhanced environmental health screening tool for Colorado. This interactive mapping tool is called CO EnviroScreen.
      The tool will enable users to identify disproportionately impacted (DI) communities based on the definition in Colorado’s Environmental Justice Act (HB21-1266). CO EnviroScreen will be one way Colorado addresses current and historic inequities.
      The mapping tool aims to:"),
  p(
    tags$ol(
      tags$li("Pinpoint areas that have a disproportionate burden of health and/or environmental harm."),
      tags$li("Help users maximize funding and resources for policy changes and other interventions to avoid, minimize, and mitigate environmental health risks."),
      tags$li("Advance a healthy and sustainable Colorado where everyone has the same degree of protection from environmental and health hazards.")
    )
  ),
  p("Scroll down for addation information on how to use this resource and the current results of the Colorado Enviroscreen."),
  ),


  # description of use ------------------------------------------------------
  tabsetPanel(
        tabPanel("Enviroscreen Score",
                 h3("What is the Enviroscreen Score"),
                 fluidRow(
                   column(6,
                           h2("example Images ")
                   ),
                   column(6,
                     h2("supporting text")
                  ),
                 )),
        tabPanel("Using the Map",
                 h3("How to use the map"),
                 p(
                   tags$ol(
                     tags$li("Geography scale selector"),
                     tags$li("Indicator Selector"),
                     tags$li("Measured vs percentile score"),
                     tags$li("Map Elements (basemap, search, reset"),
                     tags$li("interaction with tables and figures")
                   )
                 ),
                 fluidRow(
                   column(6,
                           h2("example Images "),
                   ),
                   column(
                     6,
                     h2("supporting text")
                     )
                 )),
        tabPanel("Understanding the Data",
                 h3("What to do with the data."),
                 fluidRow(column(6,
                                  h2("example Images "),
                 ),
                 column(6,
                   h2("supporting text"),
                 )
                 )),
        tabPanel("Addational Ideas",
                 h3("These tabs can continue for other discussion points"),
                 fluidRow(
                   column(align = "center", 6,
                           h2("example Images "),
                          random_text(nchars = 1000 )
                   ),
                   column(6,
                     h2("supporting text"),
                   )
                 ))
      ),


  # Select Reactive Elements ------------------------------------------------
  # content for the reactive elements of the map
  fluidRow(id = "map", style = {"border-style: solid; borderColor=:#4d3a7d; padding-left:100px;padding-right:100px;"},
           # select geography
           column(
             2,
             offset = 1,
             tags$div(title="Click here to select area to display on the map",
                      selectInput(
                        inputId = "Geom",
                        label = "Select Geographic Scale",
                        choices = c("County", "Census Tract", "Census Block Group"),
                        selected = "County",
                        width = "90%"
                      )
             )
           ),
           # select indicator
           column(
             3,
             tags$div(title="Click here to select variable for map",
                      selectInput(
                        inputId = "Indicator",
                        label = "Select Layer for Map",
                        choices = list(
                          "EnviroScreen Score" = "Colorado Enviroscreen Score",
                          "Group Component Scores" = c("Pollution and Climate Burden", "Population Burden"),
                          "Individual Component Scores" =c("Environmental Exposures",
                                                           "Environmental Effects",
                                                           "Climate",
                                                           "Sensitive Populations",
                                                           "Socioeconomic"),
                          "Environmental Exposures" = c("Ozone"                                                  
                                                        ,"Particulate Matter 2.5"                                 
                                                        ,"Lead Paint in Homes"                                    
                                                        ,"Diesel Particulate Matter"                              
                                                        ,"Traffic Density"                                        
                                                        ,"Hazardous Air Emission" 
                          ),
                          "Environmental Effects" = c(
                            "Waste Water Discharge"                                  
                            ,"Proximity to Superfund Sites"                           
                            ,"Proximity to Risk Management Plan Sites"                
                            ,"Proximity to Treatment, Storage and Disposal Facilities"
                          ),
                          "Climate" = c(
                            "Wildfire Risk"                                          
                            ,"Flood Plain Area" 
                          ),
                          "Sensitive Populations" = c(
                            "Population Under Five"                                  
                            ,"Population Over Sixity Four"                            
                            ,"Heart Disease"                                          
                            ,"Asthma"                                                 
                            ,"Life Expectancy"                                        
                            ,"Low Birth Weight" 
                          ),
                          "Socioeconomic" = c(
                            "People of Color"                                        
                            ,"Educational Attainment"                                 
                            ,"Low Income"                                             
                            ,"Linguistic Isolation"                                   
                            ,"Disability" 
                          )
                        ),
                        selected = "envExp",
                        width = "90%"
                      )
             )
           ),
           # toggle between measured and percentile
           column(
             3,
             tags$div(title="Click here to show measure value or rank of the variable",
                      selectInput(
                        inputId = "Percentile",
                        label = "Measure or Percentile",
                        choices = c("Measured Value", "Percentile Rank"),
                        selected = "Measured Value"
                      )
             ),
           ),
           # action button 
           column(
             2,
             tags$div(title="Click here to update map display",
                      actionButton("button", "Update Map")
             )
           )
  ),
  


  # display map -------------------------------------------------------------
  fluidRow(tags$style(type = "text/css", "#mymap {height: calc(100vh - 80px) !important;}"),style = {"background-color:#4d3a7d;"},
           column(1),
           column(10, leafletOutput("mymap")),
           column(1)
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
      12,
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
    df1() %>% sf::st_drop_geometry()
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


