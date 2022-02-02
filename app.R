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
# library(shinipsum)
library(rmapshaper)
library(readr)


# source helpers ----------------------------------------------------------
lapply(list.files(path = "src",recursive = TRUE, full.names = TRUE), source)


# enviroscreen data
envoData <- readRDS("data/scores/allScores.rda")%>%
  dplyr::mutate(visParam = `Colorado Enviroscreen Score_pcntl`)

# addational Data 
oil <- readRDS("data/scores/oil.rda")
coal <- readRDS("data/scores/coal.rda")
rural <- readRDS("data/scores/rural.rda")
descriptors <- read_csv("data/descriptions/indicatorDesc.csv")

# di community 
di <- getDI()# %>%
  # I'd like to use rmapshaper::ms_simplify but it is not installing properly on my computer. 
  # sf::st_simplify(preserveTopology = TRUE, dTolerance = 100)

colorRamp <- c(  "#f2f0f7"  ,"#cbc9e2"  ,"#9e9ac8"  ,"#756bb1"  ,"#54278f")

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

#hist data
histData <- envoData %>%
  sf::st_drop_geometry()%>%
  dplyr::filter(area == "County")%>%
  dplyr::select(
    "GEOID"                                                   
    ,"Colorado Enviroscreen Score"                            
    ,"Pollution and Climate Burden"                           
    ,"Population Burden"                                      
    ,"Environmental exposures"                                
    ,"Environmental effects"                                   
    ,"Climate Vulnerability"                                  
    ,"Sensitive population"                                   
    ,"Demographics"
  )

# set empty parameter for histogram funciton 
## set to non GEOID number for the histogram generate on loading. 
geoidMap <- "100"
geoids <- "08001"

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
  shiny::titlePanel(h1("Colorado Enviroscreen"),windowTitle = "Colorado Enviroscreen"),
  
  
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
             p(
               tags$ol(
                 tags$li("Definition"),
                 tags$li("How the score was developed"),
                 tags$li("Intended use"),
                 tags$li("limitations"),
               )
             ),
    ),
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
                 h2("supporting text"),
               )
             )),
    tabPanel("Understanding the Data",
             h3("What to do with the data."),
             p(
               tags$ol(
                 tags$li("Component scores"),
                 tags$li("temporality considerations "),
                 tags$li("Assumptions in the data"),
                 tags$li("limitations"),
                 tags$li("links to the background documentations")
               )
             ),
    ),
    tabPanel("Example",
      h3("Showcase how someone would go about answering a question"),
      p(
        tags$ol(
          tags$li("I care about climate, what does this resource tell me about the climate impacts present in my neighborhood"),
          tags$li("Select Climate indicator and update map"),
          tags$li("Use the serach tool "),
          tags$li("check out the table to see what goes into climate score "),
          tags$li("change geography to get more specific"),
          tags$li("download the data to view it elsewhere")
        )
      ),
    ),
    tabPanel("Addational Content",
             h3("These tabs can continue for other discussion points"),
             fluidRow(
               column(
                       6,
                       h2("example Images "),
               ),
               column(
                 6,
                 h2("supporting text"),
               )
             )
            )
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
          "Individual Component Scores" =c("Environmental exposures",
                                           "Environmental effects",
                                           "Climate Vulnerability",
                                           "Sensitive population",
                                           "Demographics"),
          "Environmental exposures" = c("Ozone"                                                  
                                        ,"Particles"                                 
                                        ,"Lead exposure risk"                                    
                                        ,"Diesel PM"                              
                                        ,"Traffic proximity and volume"                                        
                                        ,"Air Toxics Emissions" 
                                      ),
          "Environmental effects" = c(
            "Wastewater Discharge Indicator"                                  
            ,"Proximity to National Priorities List (NPL) sites"                           
            ,"Proximity to RMP Sites"                
            ,"Proximity to Hazardous Waste Facilities"
          ),
          "Climate" = c(
            "Wildfire Risk"                                          
            ,"Flood Plains" 
          ),
          "Sensitive population" = c(
            "Population under 5"                                  
            ,"Population over 64"                            
            ,"Heart disease in adults"                                          
            ,"Asthma hospitalization rate"                                                 
            ,"Life Expectancy"                                        
            ,"Low weight birth rate" 
          ),
          "Demographics" = c(
            "Percent people of color"                                        
            ,"Percent less than high school education"                                 
            ,"Percent low income"                                             
            ,"Percent linguistic isolation"                                   
            ,"Percent disability" 
          )
        ),
        selected = "Colorado Enviroscreen Score",
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
    # action button : update map elements
    column(
      1,
      tags$div(title="Click here to update map display",
               actionButton("button", "Update Map")
      )
    ),
    # action button 
    column(
      1,
      tags$div(title="Click to remove highilights features",
               actionButton("button_remove", "Remove Highlighted Areas")
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
  h2("Indicator Description"),
  fluidRow(style={"padding-left:100px;padding-right:100px;"},
    p(
      textOutput("indicatorDesc")  
    )
  ),
  br(),

  # show plots --------------------------------------------------------------
  # plot of the datasets
  h2("Graphs"),
  plotlyOutput("plot2",width = "100%"),
  

  # Describe plots  --------------------------------------------------------
  # paragraphy explaining the plot
  h2("supporting Text"),
  p("Description of the plot elements"),


  # show reactive table -----------------------------------------------------
  # table showing the results
  h2("Enviroscreen Score Data"),
  # add selection to map button 
  fluidRow(column(
    2, offset = 10,
    tags$div(title="Click here to add selections to map display",
             actionButton("button_table", "Highlight Selection to Map")
    )
  )),
  # Output: Tabset w/ plot, summary, and table ----
  tabsetPanel(type = "tabs",
              tabPanel("Group Component Scores", dataTableOutput("gcomponentScore")),
              tabPanel("Component Score", dataTableOutput("componentScore")),
              tabPanel("Environmental exposures", dataTableOutput("evnEx")),
              tabPanel("Environmental effects", dataTableOutput("evnEf")),
              tabPanel("Climate Vulnerability", dataTableOutput("clim")),
              tabPanel("Sensitive population", dataTableOutput("senPop")),
              tabPanel("Demographics", dataTableOutput("socEco"))
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

)

# Define server logic required to draw a histogram
server <- function(input, output,session) {

  # storing GEOIDs from table selection -------------------------------------
  RV<-reactiveValues(Clicks=list())

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
     # render leaflet object 
     createMap(mapData = mapData, di = di,
              pal = colorRamp, palMap = palMap,
              oil=oil, rural = rural, coal = coal)
    })

# # indicator summary -------------------------------------------------------
  # output for indicator summary'
  desc1 <- descriptors
  output$indicatorDesc <- renderText({
    ind1 <- input$Indicator
    desc1 <- descriptors %>% dplyr::filter(Indicator == ind1) %>% dplyr::select("Desc") %>% pull()
    paste0(input$Indicator, " : ", as.character(desc1))
  })


# histogram plots ---------------------------------------------------------
  # if input changes reset map value 
  plotGroup <- c( "Colorado Enviroscreen Score", "Environmental exposures","Environmental effects",
                  "Climate Vulnerability","Sensitive population","Demographics")
  # output for ployly
  output$plot2 <- renderPlotly({
    plots1 <- list()
    for(i in seq_along(plotGroup)){
      plots1[[i]] <- genPlots(dataframe = df1(),parameter = plotGroup[i], geoid = input$mymap_shape_click)
    }
    subplot(plots1, nrows = 1, shareY = TRUE, titleX = TRUE)
  })
  
  
# table output ------------------------------------------------------------   
  # output for datatable
  
  tableData <- reactive({
    geoid1 <- input$mymap_shape_click
    if(is.null(geoid1$id)){
      df1() %>% sf::st_drop_geometry()
    }else{
      genTable(tableData = df1(), geoid = input$mymap_shape_click)
    }
    })  
  
  
  ### coloring tables is something to come back too 
  # https://stackoverflow.com/questions/24736956/r-highlighting-shiny-data-table
  # https://rstudio.github.io/DT/010-style.html
  
  # group component scores 
  output$gcomponentScore <- renderDataTable(tableData()%>%
      dplyr::select(
    "GEOID"
    ,"name"
    ,"Colorado Enviroscreen Score"                                  
    ,"Colorado Enviroscreen Score_pcntl"                            
    ,"Pollution and Climate Burden"                                 
    ,"Pollution and Climate Burden_pcntl"                           
    ,"Population Burden"                                            
    ,"Population Burden_pcntl"
    ))
  
  
  # click observer event ----------------------------------------------------
  ### each table has it's own observer that assigned to the same variable. 
  ### this allows only selection from specific table to be added to map 
  observeEvent(input$gcomponentScore_rows_selected, {
    s1 <- input$gcomponentScore_rows_selected  # typo was on this line
    print(RV$Clicks )
    RV$Clicks <- tableData() %>% select("GEOID") %>%dplyr::slice(s1) %>% pull()
  })
  observeEvent(input$componentScore_rows_selected, {
    s1 <- input$componentScore_rows_selected 
    RV$Clicks <- tableData() %>% select("GEOID") %>%dplyr::slice(s1) %>% pull()
    print(RV$Clicks )
  })
  observeEvent(input$evnEx_rows_selected, {
    s1 <- input$evnEx_rows_selected 
    RV$Clicks <- tableData() %>% select("GEOID") %>%dplyr::slice(s1) %>% pull()
    print(s1)
  })
  observeEvent(input$evnEf_rows_selected, {
    s1 <- input$evnEf_rows_selected 
    RV$Clicks <- tableData() %>% select("GEOID") %>%dplyr::slice(s1) %>% pull()
    print(RV$Clicks )
  })
  observeEvent(input$clim_rows_selected, {
    s1 <- input$clim_rows_selected 
    RV$Clicks <- tableData() %>% select("GEOID") %>%dplyr::slice(s1) %>% pull()
    print(RV$Clicks )
  })
  observeEvent(input$senPop_rows_selected, {
    s1 <- input$senPop_rows_selected 
    RV$Clicks <- tableData() %>% select("GEOID") %>%dplyr::slice(s1) %>% pull()
    print(RV$Clicks )
  })
  observeEvent(input$socEco_rows_selected, {
    s1 <- input$socEco_rows_selected 
    RV$Clicks <- tableData() %>% select("GEOID") %>%dplyr::slice(s1) %>% pull()
    print(RV$Clicks )
  })

  # component Score
  output$componentScore <- renderDataTable(tableData() %>% dplyr::select(
    "GEOID",
    "name"
    ,"Environmental exposures",
    "Environmental effects",
    "Climate Vulnerability",
    "Sensitive population",
    "Demographics"
  ))
  # enviromental exposures Score
  output$evnEx <- renderDataTable(tableData() %>% dplyr::select(
    "GEOID"
    ,"name"
    ,"Ozone"
    ,"Ozone_pcntl"
    ,"Particles"                                              
    ,"Particles_pcntl"                                         
    ,"Lead exposure risk"                                     
    ,"Lead exposure risk_pcntl"                                
    ,"Diesel PM"                                              
    ,"Diesel PM_pcntl"                                         
    ,"Traffic proximity and volume"                           
    ,"Traffic proximity and volume_pcntl"                      
    ,"Air Toxics Emissions"                                   
    ,"Air Toxics Emissions_pcntl"
  ))
  # enviromental effects Score
  output$evnEf <- renderDataTable(tableData() %>% dplyr::select(
    "GEOID"
    ,"name"
    ,"Wastewater Discharge Indicator"                         
    ,"Wastewater Discharge Indicator_pcntl"                    
    ,"Proximity to National Priorities List (NPL) sites"      
    ,"Proximity to National Priorities List (NPL) sites_pcntl" 
    ,"Proximity to RMP Sites"                                 
    ,"Proximity to RMP Sites_pcntl"                            
    ,"Proximity to Hazardous Waste Facilities"                
    ,"Proximity to Hazardous Waste Facilities_pcntl"
  ))
  # component Score
  output$clim <- renderDataTable(tableData() %>% dplyr::select(
    "GEOID"
    ,"name"
    ,"Wildfire Risk"                                          
    ,"Wildfire Risk_pcntl"                                     
    ,"Flood Plains"                                           
    ,"Flood Plains_pcntl"
  ))
  # component Score
  output$senPop <-  renderDataTable(tableData() %>% dplyr::select(
    "GEOID"
    ,"name"
    ,"Population under 5"                                     
    ,"Population under 5_pcntl"                                
    ,"Population over 64"                                     
    ,"Population over 64_pcntl"                                
    ,"Heart disease in adults"                                
    ,"Heart disease in adults_pcntl"                           
    ,"Asthma hospitalization rate"                            
    ,"Asthma hospitalization rate_pcntl"                       
    ,"Life Expectancy"                                        
    ,"Life Expectancy_pcntl"                                   
    ,"Low weight birth rate"                                  
    ,"Low weight birth rate_pcntl"
  ))
  # component Score
  output$socEco <- renderDataTable(tableData() %>% dplyr::select(
    "GEOID"
    ,"name"
    ,"Percent people of color"                                
    ,"Percent people of color_pcntl"                           
    ,"Percent less than high school education"                
    ,"Percent less than high school education_pcntl"           
    ,"Percent low income"                                     
    ,"Percent low income_pcntl"                                
    ,"Percent linguistic isolation"                           
    ,"Percent linguistic isolation_pcntl"                      
    ,"Percent disability"                                     
    ,"Percent disability_pcntl"
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
  # add selected features to map 
  observeEvent(input$button_table, {
    print(RV$Clicks)
    mapFeatures <- envoData %>% select("GEOID") %>% dplyr::filter(GEOID %in% RV$Clicks)
      # add features to map 
      leafletProxy("mymap") %>%
        clearGroup(group = "Table Highlight") %>%
        addPolygons(
          data = mapFeatures,
          options = pathOptions(pane = "index"),
          group = "Table Highlight"
        )
    })
  
  # remove selected features from map 
  observeEvent(input$button_remove, {
    # add features to map 
    leafletProxy("mymap") %>%
      clearGroup(group = "Table Highlight") 
  })

  # click observer event ----------------------------------------------------
  observeEvent(input$mymap_shape_click, {
    geoidMap <- input$mymap_shape_click$id  # typo was on this line
    print(geoidMap)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
## display in browser for styling elements 
# options = list(launch.browser = TRUE)
