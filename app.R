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
library(shiny)
library(stringr)
library(plotly)
library(DT)
library(vroom)
library(bslib)
# library(rmapshaper)


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
di <- getDI()# %>%
  # I'd like to use rmapshaper::ms_simplify but it is not installing properly on my computer. 
  # sf::st_simplify(preserveTopology = TRUE, dTolerance = 100)

colorRamp <- c(  "#f2f0f7"  ,"#cbc9e2"  ,"#9e9ac8"  ,"#756bb1"  ,"#54278f")

# selector for tables 
p <- "temp"

# create initial dataset for map  -----------------------------------------
mapData <- initialMapData(envoData)
# palette for the map
palMap <- leaflet::colorNumeric(palette = colorRamp,
    domain = mapData$visParam,
    reverse = TRUE
  )

# unique Indicators
indicators <- sf::st_drop_geometry(envoData) %>%
  dplyr::select(-c(name, area, GEOID)) %>%
  dplyr::select(!ends_with("_Pctl")) %>%
  dplyr::select(!ends_with("_pcntl")) %>%
  names()



# UI  ---------------------------------------------------------------------
ui <- fluidPage(
  theme = bslib::bs_theme(
  bootswatch = "flatly",
  #bg = "#FFFFFF",
  #fg = "#000",
  primary = "#245d38",# green 
  secondary = "#001970", #blue 
  success = "#245d38", # green
  base_font = "Trebuchet MS,Helvetica,sans-serif",
  heading_font = "museo-sans,sans-serif"
    )%>%
    bslib::bs_add_rules(sass::sass_file("www/style.scss")),
  # # nav panel ---------------------------------------------------------------
  # # not sure if it's needed but probably... 
  # 
  
  # HTML('<header class="main-header" role="banner">
  #   <img src="MountainsToPlains.png" alt="Banner Image"/>
  # </header>'
  # ),
  # 
  # Title ------------------------------------------------------------------
  shiny::titlePanel(h1("Colorado Enviroscreen")),
  
  
  fluidRow(style={"padding-left:100px;padding-right:100px;"},
    p(HTML("</br><a href='#map'>Jump to Map</a>")),
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
    p("Scroll down for addation information on how to use this resource and the current results of the Colorado Enviroscreen.")

  ),


  # description of use ------------------------------------------------------
  h2("Understanding the Enviroscreen Tool"),
  tabsetPanel(
    tabPanel("Enviroscreen Score",
             h3("What is the Enviroscreen Score"),
             fluidRow(
               column( align = "center",
                       6,
                       h2("example Images "),
               ),
               column(
                 6,
                 h2("supporting text"),
               )
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
               column( align = "center",
                       6,
                       h2("example Images "),
               ),
               column(
                 6,
                 h2("supporting text"),
               )
             )),
    tabPanel("Understanding the Data",
             h3("What to do with the data."),
             fluidRow(column( align = "center",
                              6,
                              h2("example Images "),
             ),
             column(
               6,
               h2("supporting text"),
             )
             )),
    tabPanel("Addational Ideas",
             h3("These tabs can continue for other discussion points"),
             fluidRow(
               column( align = "center",
                       6,
                       h2("example Images "),
               ),
               column(
                 6,
                 h2("supporting text"),
               )
             ))
  ),



  # Select Reactive Elements ------------------------------------------------
  # content for the reactive elements of the map
  br(), 
  h2("Select Elements to display on the map"),
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
  h2("indicator description"),
  textOutput("indicatorDesc"),
  br(),

  # show plots --------------------------------------------------------------
  # plot of the datasets
  h2("Graphs"),
  fluidRow(
    plotlyOutput("graph1")
  ),
  

  # Describe plots  --------------------------------------------------------
  # paragraphy explaining the plot
  h2("supporting Text"),
  p("Description of the plot elements"),


  # show reactive table -----------------------------------------------------
  # table showing the results
  h2("Enviroscreen Score Data"),
  # Output: Tabset w/ plot, summary, and table ----
  tabsetPanel(type = "tabs",
              tabPanel("Group Component Scores", dataTableOutput("gcomponentScore")),
              tabPanel("Component Score", dataTableOutput("componentScore")),
              tabPanel("Environmental Exposures", dataTableOutput("evnEx")),
              tabPanel("Environmental Effects", dataTableOutput("evnEf")),
              tabPanel("Climate", dataTableOutput("clim")),
              tabPanel("Sensitive Population", dataTableOutput("senPop")),
              tabPanel("Socioeconomic", dataTableOutput("socEco"))
    ),
  # download table option  --------------------------------------------------
  fluidRow(
    column(2,selectInput("download", "Choose a dataset:",
                  choices = c("All Data" 
                              # ,"Group Component Scores"
                              # ,"Component Score"
                              # ,"Environmental Exposures"
                              # ,"Environmental Effects"
                              # ,"Climate"
                              # ,"Sensitive Population"
                              # ,"Socioeconomic"
                              )
                  ),
    ),
    column(2,downloadButton("downloadData", "Download"))
  ),
  
  h3("Addational Resources"),
  p("We can utilize this space for sharing any relivent reference information."),

  # # print statement for trouble shooting
  # fluidRow(
  #   column(
  #     12,
  #     textOutput("test1")
  #   )
  # )
)

# Define server logic required to draw a histogram
server <- function(input, output,session) {

  # intro image -------------------------------------------------------------
  # image output
  # output$image <- renderImage({
  #   random_image()
  # })

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
    createMap(mapData = mapData, di = di,
              pal = colorRamp, palMap = palMap,
              oil=oil, rural = rural, coal = coal)
    })

# # indicator summary -------------------------------------------------------
  # output for indicator summary
  output$indicatorDesc <- renderText({
    # paste0(input$Indicator, " : ", shinipsum::random_text(nwords = 40))
  })


# histogram plots ---------------------------------------------------------
  
  # output for ployly
  output$graph1 <- renderPlotly({
    p1<- plot_ly(iris)
    p1
  })

# table output ------------------------------------------------------------   
  # output for datatable
  tableData <- reactive({
    df1() %>% sf::st_drop_geometry()
    }
  )
  
  # group component scores 
  output$gcomponentScore <- renderDataTable(tableData() %>% dplyr::select(
    "GEOID"
    ,"name"
    ,"Colorado Enviroscreen Score"                                  
    ,"Colorado Enviroscreen Score_pcntl"                            
    ,"Pollution and Climate Burden"                                 
    ,"Pollution and Climate Burden_pcntl"                           
    ,"Population Burden"                                            
    ,"Population Burden_pcntl"
    ))
  # component Score
  output$componentScore <- renderDataTable(tableData() %>% dplyr::select(
    "GEOID",
    "name"
    ,"Environmental Exposures",
    "Environmental Effects",
    "Climate",
    "Sensitive Populations",
    "Socioeconomic"
  ))
  # enviromental exposures Score
  output$evnEx <- renderDataTable(tableData() %>% dplyr::select(
    "GEOID"
    ,"name"
    ,"Ozone"
    ,"Ozone_pcntl"
    ,"Particulate Matter 2.5"
    ,"Particulate Matter 2.5_pcntl"
    ,"Lead Paint in Homes"
    ,"Lead Paint in Homes_pcntl"
    ,"Diesel Particulate Matter"
    ,"Diesel Particulate Matter_pcntl"
    ,"Traffic Density"
    ,"Traffic Density_pcntl"
    ,"Hazardous Air Emission"
    ,"Hazardous Air Emission_pcntl"
  ))
  # enviromental effects Score
  output$evnEf <- renderDataTable(tableData() %>% dplyr::select(
    "GEOID"
    ,"name"
    ,"Waste Water Discharge"
    ,"Waste Water Discharge_pcntl"
    ,"Proximity to Superfund Sites"
    ,"Proximity to Superfund Sites_pcntl"
    ,"Proximity to Risk Management Plan Sites_pcntl"
    ,"Proximity to Risk Management Plan Sites"
    ,"Proximity to Treatment, Storage and Disposal Facilities"
    ,"Proximity to Treatment, Storage and Disposal Facilities_pcntl"
  ))
  # component Score
  output$clim <- renderDataTable(tableData() %>% dplyr::select(
    "GEOID"
    ,"name"
    ,"Wildfire Risk"
    ,"Wildfire Risk_pcntl"
    ,"Flood Plain Area"
    ,"Flood Plain Area_pcntl"
  ))
  # component Score
  output$senPop <-  renderDataTable(tableData() %>% dplyr::select(
    "GEOID"
    ,"name"
    ,"Population Under Five"
    ,"Population Under Five_pcntl"
    ,"Population Over Sixity Four"
    ,"Population Over Sixity Four_pcntl"
    ,"Heart Disease"
    ,"Heart Disease_pcntl"
    ,"Asthma"
    ,"Asthma_pcntl"
    ,"Life Expectancy_pcntl"
    ,"Life Expectancy"
    ,"Low Birth Weight"
    ,"Low Birth Weight_pcntl"
  ))
  # component Score
  output$socEco <- renderDataTable(tableData() %>% dplyr::select(
    "GEOID"
    ,"name"
    ,"People of Color"
    ,"People of Color_pcntl"
    ,"Educational Attainment"
    ,"Educational Attainment_pcntl"
    ,"Low Income"
    ,"Low Income_pcntl"
    ,"Linguistic Isolation"
    ,"Linguistic Isolation_pcntl"
    ,"Disability"
    ,"Disability_pcntl"
  ))
  # download data -----------------------------------------------------------
  
  # Reactive value for selected dataset ----
  datasetInput <- reactive({
    switch(input$download,
           "All Data" = df1()
           # ,"Group Component Scores" = output$gcomponentScore 
           # ,"Component Score" = output$componentScore 
           # ,"Environmental Exposures" = output$evnEx
           # ,"Environmental Effects" =output$evnEf
           # ,"Climate" = output$clim 
           # ,"Sensitive Population" = output$senPop
           # ,"Socioeconomic" = socEcoTable()
           )
  })

  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$download,"_",input$Geom, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(datasetInput(), file, row.names = FALSE)
    }
  )
  

  
# proxy map elements  -----------------------------------------------------
  observeEvent(input$button, {
    ### helpful source https://stackoverflow.com/questions/37433569/changing-leaflet-map-according-to-input-without-redrawing
    # geography 
    geo <- input$Geom 
    
    # indicator 
    in1 <- input$Indicator
    indicator1 <- in1
    indicator2 <- paste0(in1,"_pcntl")
    
    if(input$Percentile == "Measured Value"){
      indicator <- indicator1 
    }
    if(input$Percentile == "Percentile Rank"){
      indicator <- indicator2
    }
    
    # filter and assign visparam 
    ed2 <- envoData[envoData$area == geo, ]
    ed2 <- ed2 %>%
      mutate(visParam = !!as.symbol(indicator))%>% # https://stackoverflow.com/questions/62862705/r-shiny-mutate-replace-how-to-mutate-specific-column-selected-from-selectinput
      st_cast("POLYGON")
    
    
    ed2 <- ed2 %>%
      dplyr::mutate(
        popup = paste0(
          "<br/><strong>", as.character(in1),"</strong>", # needs to be text
          paste0("<br/><strong>",name,"</strong>"),
          paste0(if(in1 %in% c("Colorado Enviroscreen Score",
                                    "Pollution Burden",
                                    "Climate Burden",
                                    "Environmental Exposures",
                                    "Environmental Effects",
                                    "Climate",
                                    "Sensitive Populations",
                                    "Socioeconomic"
          )){
            paste0("<br/><b>Percentile:</b> ", round(!!as.symbol(indicator2), digits =  0))
          }else{
            paste0("<br/><b>Measured:</b> ", round(!!as.symbol(indicator1), digits = 2),
                   "<br/><b>Percentile:</b> ", round(!!as.symbol(indicator2), digits =  0))
          })
      ))
    
    # palette 
    pal1 <- leaflet::colorNumeric(palette = colorRamp,
                                  domain = ed2$visParam,
                                  reverse = TRUE)
    
    leafletProxy("mymap") %>%
      clearGroup(group = "Indicator Score") %>%
      addPolygons(
        data = ed2,
        color = "#454547",
        weight = 1,
        smoothFactor = 0.5,
        opacity = 1.0,
        layerId = ed2$GEOID,
        fillOpacity = 0.5,
        fillColor =  ~pal1(ed2$visParam),
        popup = ed2$popup,
        highlightOptions = highlightOptions(
          color = "white",
          weight = 2,
          bringToFront = TRUE
        ),
        options = pathOptions(pane = "index"),
        group = "Indicator Score"
        
      )
  })

# # click observer event ----------------------------------------------------
#   observeEvent(input$mymap_shape_click, { 
#     p <- input$mymap_shape_click$id  # typo was on this line
#     print(p)
#   })
}

# Run the application
shinyApp(ui = ui, server = server)
## display in browser for styling elements 
# options = list(launch.browser = TRUE)
