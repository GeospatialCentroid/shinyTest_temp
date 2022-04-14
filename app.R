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
library(bslib)
library(readr)
library(data.table)
library(leaflegend)
library(shinyBS)
library(shinyWidgets)


# source helpers ----------------------------------------------------------
lapply(list.files(path = "src",recursive = TRUE, full.names = TRUE), source)

version <- 3

# enviroscreen data
envoData <- readRDS(paste0("data/scores/allScores_",version,".rds"))%>%
  dplyr::mutate(visParam = `Colorado EnviroScreen Score Percentile`)%>%
  dplyr::select("County Name", "GEOID", everything())%>% 
  dplyr::select(-"GEOID3")


# Additional Data 
oil <- readRDS("data/scores/oilgasVis.rds") 
coal <- readRDS("data/scores/coalVis.rds")
rural <- readRDS("data/scores/ruralVis.rds")
descriptors <- read_csv("data/descriptions/indicatorDesc.csv")
justice40 <- readRDS("data/scores/justice40.rds") %>%
  dplyr::mutate(popup = paste0(
    "Census Tract ", GEOID ," in ", County_Name," County."    
    ,br()
    ,"A total of ", Total.threshold.criteria.exceeded," clauses defined this area as disadvantaged."))

# di community 
di <- getDI()
# palette for DI layer 
diPal <- colorFactor(palette = c(
  "#a6cee3", "#33a02c","#b2df8a","#1f78b4"), levels = c("Low Income", "People of Color",
                                                       "Housing Burden", "More then one category"), di$color
)


colorRamp <- c(  "#f2f0f7"  ,"#cbc9e2"  ,"#9e9ac8"  ,"#756bb1"  ,"#54278f")

# create initial dataset for map  -----------------------------------------
mapData <- initialMapData(envoData)
# palette for the map
palMap <- leaflet::colorNumeric(palette = colorRamp,
                                domain = mapData$visParam,
                                na.color = "#808080",
    reverse = TRUE
  )


# unique Indicators
indicators <- sf::st_drop_geometry(envoData) %>%
  dplyr::select(-c(`County Name`, area, GEOID)) %>%
  dplyr::select(!ends_with("_Pctl")) %>%
  dplyr::select(!ends_with(" Percentile")) %>%
  names()

#hist data
histData <- envoData %>%
  sf::st_drop_geometry()%>%
  dplyr::filter(area == "County")%>%
  dplyr::select(
    "GEOID"                                                   
    ,"Colorado EnviroScreen Score"                            
    ,"Pollution & Climate Burden"                           
    ,"Socioeconomics & Demographics"                                      
    ,"Environmental exposures"                                
    ,"Environmental effects"                                   
    ,"Climate vulnerability"                                  
    ,"Sensitive population"                                   
    ,"Demographics"
  )

# set empty parameter for histogram funciton 
## set to non GEOID number for the histogram generate on loading. 
geoidMap <- "100"



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

  # Title ------------------------------------------------------------------
  fluidRow(
    class = "titleElement",
    column(4, 
           tags$a(
             href = "https://cdphe.colorado.gov/enviroscreen",
             tags$img(
               src="EnviroScreen Logos/co_cdphe_pr_es_white_v.png",
               title = "Colorado Department of Public Health and Environment",
               width="90%",
               height="90%" 
             )
           )
          ),
    column(8, h1("Colorado EnviroScreen"), h4("Beta Test Version February 2022"))
  ),
  
  fluidRow(
    p(HTML("</br><a href='#map'>Jump to Map</a>")),
    p("The Colorado Department of Public Health and Environment and a team at Colorado State University are working on an enhanced environmental health screening tool for Colorado. This interactive mapping tool is called Colorado EnviroScreen. To learn more about the project, please visit CDPHE’s",
      tags$a(href = "https://cdphe.colorado.gov/enviroscreen", "Colorado EnviroScreen webpage.", target = "_blank"),
      "EnviroScreen will enable users to identify disproportionately impacted (DI) communities based on the definition in Colorado’s Environmental Justice Act (HB21-1266). Colorado EnviroScreen will be one way that Colorado addresses current and historic inequities."),
  ),
  fluidRow(
    p("The mapping tool aims to:")),
  fluidRow(
    p(
      tags$ul(
        tags$li("Pinpoint areas where DI communities also have a greater health burden and/or more environmental risks."),
        tags$li("Help users advocate for funding, interventions, and policy changes to avoid, minimize, and mitigate environmental health risks."),
        tags$li("Advance a healthy and sustainable Colorado where everyone has the same degree of protection from environmental and health hazards.")
      ))),
  fluidRow(
    p("Click here for more information about CDPHE’s work to ",
      tags$a(href = "https://cdphe.colorado.gov/environmental-justice", "advance environmental justice", target = "_blank"),
      ", ",
      tags$a(href = "https://cdphe.colorado.gov/environment/toxicology-and-environmental-epidemiology", "understand the connection between the environment and our health", target = "_blank"),
      " , and ",
      tags$a(href = "https://cdphe.colorado.gov/air-pollution/climate-change#equity", "advance climate equity.", target = "_blank"),
     )),
  fluidRow(
           p("Scroll down for additional information about how to use Colorado EnviroScreen.")
  ),


  # description of use ------------------------------------------------------
  fluidRow(class = "sectionTitle", 
           h2("Understanding the EnviroScreen Tool")
  ),
  tabsetPanel(
    tabPanel( "EnviroScreen Score",
             br(),
             tags$strong("EnviroScreen Score"),
             p(
               "Colorado EnviroScreen is an environmental justice mapping tool that uses population and environmental factors to calculate an EnviroScreen score.",
               br(),
               "A higher EnviroScreen score means the area is more likely to be affected by environmental injustice. There are scores for each county, census tract, and census block group in Colorado. CDPHE expects to make improvements and additions to the tool in response to feedback or as new data become available",
               br(),
               "Although EnviroScreen provides a robust measure of cumulative environmental burden, it is not a perfect tool. The tool uses limited environmental and sociodemographic data to calculate the EnviroScreen score.",
             ),
             p(
               "Colorado EnviroScreen does NOT:",
               tags$ul(
                 tags$li("Define all areas that might be affected by environmental injustice or specific environmental burdens."),
                 tags$li("Tell us about individuals who have health problems that make them more likely to experience negative effects from environmental exposures."),
                 tags$li("Take all Environmental exposures into account."),
                 tags$li("Tell us about smaller areas within a county, census tract, or census block group that may be more vulnerable to environmental exposures."),
              )
             ),
             p(
               "For more information about how to use environmental justice mapping tools, 
    please review the Climate Equity Data Viewer guide (available in", 
    tags$a(href = "https://drive.google.com/file/d/1iytdPG5iK2VBNpIy8k6oT6lU6-QKMLOa/view?usp=sharing", "English", target = "_blank"),
    "and ",
    tags$a(href = "https://drive.google.com/file/d/17rQ90fNt3DF-0PbySpGjo2tiy9AmDiCc/view?usp=sharing", "Spanish", target = "_blank"),
    ")."
             ),
    ),
    tabPanel("Using the Map",
             br(),
             tags$strong("Using the Map"),
             p("The default view of the map shows the state of Colorado. Individual counties, census tracts, or census block groups are color-coded, based on their climate equity score. The legend on the right-hand side of the map shows what these colors represent. The lighter the color, the worse the EnviroScreen score."),
             br(),
             p(
               "The map is interactive. You can zoom in and out, drag the map to a different location,  and click on a location to learn more about its EnviroScreen score. Explore these features:"
             ),
             p(
               tags$ul(
                 tags$li("Search for an address, city, ZIP code, or place name by using the magnifying glass icon, typing the location into the search bar, and selecting the location from the populated drop-down list."),
                 tags$li("Zoom in and out of the map by scrolling up and down with your mouse or trackpad. "),
                 tags$li("Drag the map to a new location by clicking, holding, and moving your cursor."),
                 tags$li("Click on a location to view more information for that location in the graph and table below the map.")
                 ),
             )
            ),
    tabPanel("Understanding the Data",
             br(),
             tags$strong("Understanding the Data"),
             p(
               "EnviroScreen displays a cumulative impact EnviroScreen score that is derived from relative rankings (percentiles) of individual data indicators. Individual data indicators are aggregated into topic-based scores. These topic-based scores are aggregated further into the Pollution & Climate Burden and Socioeconomics & Demographics scores. The Pollution & Climate Burden and Socioeconomics & Demographics scores are multiplied together to calculate the EnviroScreen score. The EnviroScreen score is the default data visualized on the map. Sub-component scores and individual datasets are also available for display by selecting from the “Select Layer for Map” dropdown above the map." 
             ),
    ),
    tabPanel("Example",
             br(),
             tags$strong("Example"),
      p("A non-profit group wants to apply for a grant it will use to install more air 
        pollution monitoring in its community. The grant rules say applicants
        have to show that their community needs the funds. The group knows science-based 
        information will strengthen its application. The group uses Colorado EnviroScreen
        to get more information about their community."
        ),
      p("Here’s how the community group would use the tools in Colorado EnviroScreen:",
        tags$ol(
          tags$li("They start by using the search feature (magnifying glass in the upper right of map) to find the census tract in which its community office is located."),
          tags$li("They click this area on the map and scroll below the map to view additional information."),
          tags$li("They compare their census tract’s overall and component EnviroScreen scores to compare their community with the rest of the state."),
          tags$li("They download the data for their census tract at the bottom of the page. "),
          tags$li("They use this information to write a compelling grant application for community air monitors."),
        )
      ), 
    ),
    tabPanel("Descriptions",
             br(),
             tags$em("EnviroScreen Score"),
             p("The EnviroScreen sScore combines population characteristics and with environmental burdens. The score goes from 0 to 100, with the highest score representing the highest burden."
               ,"The EnviroScreen score is a percentile, which is like a ranking. The number represents how many of the state’s counties, census tracts, or census block groups have a lower score than the geography in question."
               ,"Suppose a census block group has an EnviroScreen score of 50. Thisat means its EnviroScreen score is higher than 50% of all census block groups in Coloradothe state. A score of 90 means its the EnviroScreen score is higher than 90% of all census block groups in Colorado the state."
             ),
             br(),
    ),
    tabPanel("FAQ",
             br(),
             tags$em("Frequently Asked Questions"),
  
             tags$iframe(src = "https://docs.google.com/document/d/15XVihyI1_i7oMdQvoT0gbARBQvKhCaPI1FkU2Ua8U_k/edit?usp=sharing", seamless=NA,
                         width = "100%", height = 800), 
             br(),
    )
    ),




  # Select Reactive Elements ------------------------------------------------
  # content for the reactive elements of the map
  br(), 
  fluidRow(class = "sectionTitle",id = "map",
    # action button : update map elements
    column(2,
          tags$div(title = "Click here to update map display",
           actionButton(inputId = "updateMap", "Update Map"),
       ),
    ),
    # select geography  
    column(2,
      tags$div(title="Click here to select area to display on the map",
               selectInput(
                 inputId = "Geom",
                 label = "Geo Scale",
                 choices = c("County", "Census Tract", "Census Block Group"),
                 selected = "County",
                   width = "90%"
               )
      )
    ),
    # select indicator
    column(4,
      tags$div(title="Click here to select variable for map",
      selectInput(
        inputId = "Indicator",
        label = "Layer",
        choices = list(
          "EnviroScreen Score" = "Colorado EnviroScreen Score",
          "Group Component Scores" = c("Pollution & Climate Burden", "Socioeconomics & Demographics"),
          "Individual Component Scores" =c("Environmental exposures",
                                           "Environmental effects",
                                           "Climate vulnerability",
                                           "Sensitive population",
                                           "Demographics"),
          "Environmental exposures" = c("Air toxics emissions",
                                        "Diesel PM", 
                                        "Drinking Water Violations",
                                        "Lead exposure risk",
                                        "Noise",
                                        "Other Air Pollutants",
                                        "Ozone",
                                        "Particles" ,                                
                                       "Traffic proximity & volume"                                        
                                      ),
          "Environmental effects" = c(
            "Impaired Surface Water",
            "Proximity to hazardous waste facilities",
            "Proximiy to Mining and Smelting",
            "Proximity to National Priorities List (NPL) sites",
            "Proximiy to Oil and Gas",
            "Proximity to RMP sites",              
            "Wastewater discharge indicator" 
          ),
          "Climate vulnerability" = c(
            "Drought",
            "Extreme Heat Days",
            "Floodplains",
            "Wildfire risk"                                          
          ),
          "Sensitive population" = c(
            "Asthma hospitalization rate",
            "Cancer Incidence",
            "Diabetes Incidence",
            "Heart disease in adults",
            "Life expectancy",                                       
            "Low weight birth rate",
            "Mental Health Incidence",
            "Population over 64",
            "Population under 5"                                  
          ),
          "Demographics" = c(
            "Housing Cost Burdened",
            "Percent disability",
            "Percent less than high school education",                                
            "Percent linguistic isolation",
            "Percent low income", 
            "Percent people of color"
          )
        ),
        selected = "Colorado EnviroScreen Score",
        width = "90%"
      )
    )
    ),
    # toggle between measured and percentile
    column(2,
      tags$div(title="Click here to show measured value or rank of the variable",
      selectInput(
        inputId = "Percentile",
        label = "Measure or %",
        choices = c("Measured Value", "Percentile Rank"),
        selected = "Percentile Rank"
        )
      )
    ),
    column(
      2,
      tags$div(title="Click here to remove highlighted features",
               actionButton("removeHighlight", "Remove Highlighted Areas")
      )
    ),
    tags$blockquote(textOutput("indicatorDesc"))
    
  ),
  
  # display map -------------------------------------------------------------
  fluidRow(tags$style(type = "text/css", "#mymap {height: calc(100vh - 250px) !important;}"), #style = {"background-color:#4d3a7d;"},
           column(1),
           column(7, leafletOutput("mymap")),
           column(3, br(),br(),br(),br(),
                  plotlyOutput("histEnviroScreen" ,height = "80%", width = "100%")),
           column(1),

  ),

  # describe indicators -----------------------------------------------------
  # sentence explaining the indicators
  #    # add selection to map button 
  # tags$blockquote(textOutput("indicatorDesc")),
  # show plots --------------------------------------------------------------
  # plot of the datasets
  br(),
  fluidRow(class = "plotArea",
           column(1),
           column(2, br(), plotlyOutput("histExposure", height=300)),
           column(2, br(), plotlyOutput("histEffect", height=300)),
           column(2, br(), plotlyOutput("histClimate", height=300)),
           column(2, br(), plotlyOutput("histSocial", height=300)),
           column(2, br(), plotlyOutput("histDemo", height=300)),
           column(1),
    p("The EnviroScreen score combines five components: Environmental exposures, Environmental effects, Climate vulnerability, Sensitive population, and Demographics. When you click a location on the map, the orange bars in this chart show the score for that location. The orange bars show how the location compares to the rest of Colorado for each component score. Together, the charts show how the EnviroScreen score is calculated for the selected location.")
  ),
  br(),
  
  # show reactive table -----------------------------------------------------
  # table showing the results
  fluidRow(class = "sectionTitle",
           h2("EnviroScreen Score Data"),
           p("Use the tabs above  the table to filter through different elements of the
    Colorado EnviroScreen Score. You can select specific rows in the table, then hit the
    Blue `Highlight Selection on Map` button in the upper right to view the location on the map."),

  ),

  radioGroupButtons(inputId = "tableSelect", label = "",
                    choices = c("Group Component Scores", "Component Score",
                                "Environmental Exposures", "Environmental Effects",
                                "Climate Vulnerability", "Sensitive Population",
                                "Demographics", "Extra Evaluations"),
                    justified = TRUE),
  # data table output ----
  # changed to just single table
  DT::dataTableOutput("tableAll"),
  
  # tabsetPanel(type = "tabs",
  #             tabPanel("Group Component Scores", DT::dataTableOutput("gcomponentScore")),
  #             tabPanel("Component Score", DT::dataTableOutput("componentScore")),
  #             tabPanel("Environmental exposures", dataTableOutput("evnEx")),
  #             tabPanel("Environmental Effects", dataTableOutput("evnEf")),
  #             tabPanel("Climate vulnerability", dataTableOutput("clim")),
  #             tabPanel("Sensitive population", dataTableOutput("senPop")),
  #             tabPanel("Demographics", dataTableOutput("socEco")),
  #             tabPanel("Extra Evaluations", dataTableOutput("exEval"))),
  # download table option  --------------------------------------------------
  fluidRow(
    column(4,
           offset = 3, 
           tags$div(title="Click here to download content",
           downloadButton("downloadData", "Download Data for Current Geography")
           ),
    ),
    column(4, 
      tags$div(title="Click here to add selections to map display",
                 actionButton("button_table", "Highlight Selection on Map")
      ),
    )
  ),
  br(),
  
  fluidRow(    class = "titleElement", 
               column(8,   
                      h3("Additional Resources"),
                      p(class = "href2",
                        "For more information about how to use environmental justice mapping tools, 
    please review the Climate Equity Data Viewer guide (available in", 
    tags$a(href = "https://drive.google.com/file/d/1iytdPG5iK2VBNpIy8k6oT6lU6-QKMLOa/view?usp=sharing",
           tags$span(style="color:white","English"), target = "_blank"),
    "and ",
    tags$a(href = "https://drive.google.com/file/d/17rQ90fNt3DF-0PbySpGjo2tiy9AmDiCc/view?usp=sharing",
           tags$span(style="color:white","Spanish"), target = "_blank"),
    ")."
                      ), ),
    column(4,tags$a(
      href = "https://cdphe.colorado.gov/enviroscreen",
      tags$img(
        src="EnviroScreen Logos/co_cdphe_pr_es_white_v.png",
        title = "Colorado Department of Public Health and Environment",
        width="90%",
        height="90%" 
        )
      )
    )
  ),
)

# Define server logic required to draw a histogram
server <- function(input, output,session) {


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
     createMap(mapData = mapData, di = di, diPal = diPal,
              pal = colorRamp, palMap = palMap,
              oil=oil, rural = rural, coal = coal, justice40 = justice40)
    })

# indicator summary -------------------------------------------------------
  # output for indicator summary'
  desc1 <- descriptors
  output$indicatorDesc <- renderText({
    ind1 <- input$Indicator
    desc1 <- descriptors %>% dplyr::filter(Indicator == ind1) %>% dplyr::select("Desc") %>% pull()
    paste0(input$Indicator," : ", as.character(desc1))
  })


# histogram plots ---------------------------------------------------------
  # if input changes reset map value 
  plotGroup <- c( "Colorado EnviroScreen Score", "Environmental exposures","Environmental effects",
                  "Climate vulnerability","Sensitive population","Demographics")
  
  ### need individual output objects for each plot 
  output$histEnviroScreen <- renderPlotly({
    genPlots(dataframe = df1(),parameter = "Colorado EnviroScreen Score", geoid = input$mymap_shape_click)
  })
  output$histExposure<- renderPlotly({
    genPlots(dataframe = df1(),parameter = "Environmental exposures", geoid = input$mymap_shape_click)
  })
  output$histEffect<- renderPlotly({
    genPlots(dataframe = df1(),parameter = "Environmental effects", geoid = input$mymap_shape_click)
  })
  output$histClimate<- renderPlotly({
    genPlots(dataframe = df1(),parameter = "Climate vulnerability", geoid = input$mymap_shape_click)
  })
  output$histSocial<- renderPlotly({
    genPlots(dataframe = df1(),parameter = "Sensitive population", geoid = input$mymap_shape_click)
  })
  output$histDemo<- renderPlotly({
    genPlots(dataframe = df1(),parameter = "Demographics", geoid = input$mymap_shape_click)
  })
    
# table output ------------------------------------------------------------   
  # output for datatable based on columns selected

  tableData <- reactive({
    geoid1 <- input$mymap_shape_click
    if(is.null(geoid1$id)){
      geoid1 <- 1
    }
    genTable(tableData = df1(), geoid = geoid1, colSelected = input$tableSelect)
  })
  
  # storing GEOIDs from table/map selection -------------------------------------
  RV<-reactiveValues()
  
  observeEvent(input$tableAll_rows_selected, {
      RV$select <- isolate(tableData() %>% dplyr::slice(input$tableAll_rows_selected) %>% pull(GEOID))
  })
  
  observeEvent(input$mymap_shape_click, {
     RV$select <- isolate(input$mymap_shape_click$id)
  })


  # Render the table outputs ------------------------------------------------

  output$tableAll <- renderDataTable({
    DT::datatable(tableData(), 
                  options = list(autoWidth = TRUE, scrollX = TRUE))
                  #selection = list(mode = 'multiple', selected = which(tableData()$GEOID %in% RV$select)))
    })
    
  # Table proxy for selection
  observe({
    
    DT::dataTableProxy("tableAll") %>% 
      selectRows(which(tableData()$GEOID %in% RV$select))
  })
  

  # download data -----------------------------------------------------------
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$Geom, "_data.csv", sep = "")
    },
    content = function(file) {
      write.csv(df1() %>% sf::st_drop_geometry() %>% select(-"visParam"), file, row.names = FALSE)
    }
  )
  

  
# proxy map elements  -----------------------------------------------------
  observeEvent(input$updateMap, {
    ### helpful source https://stackoverflow.com/questions/37433569/changing-leaflet-map-according-to-input-without-redrawing
    # geography 
    geo <- input$Geom 
    
    # indicator 
    in1 <- input$Indicator
    indicator1 <- in1
    indicator2 <- paste0(in1," Percentile")
    
    if(input$Percentile == "Measured Value"){
      indicator <- indicator1 
    }
    if(input$Percentile == "Percentile Rank"){
      indicator <- indicator2
    }
    
    # filter and assign visparam 
    ed2 <- envoData[envoData$area == geo, ]
    ed2 <- ed2 %>%
      mutate(visParam = !!as.symbol(indicator))%>%# https://stackoverflow.com/questions/62862705/r-shiny-mutate-replace-how-to-mutate-specific-column-selected-from-selectinput
      dplyr::select(GEOID, `County Name`, indicator1, indicator2,`Coal Community`,`Oil and Gas Community`,`Rural Community`,visParam)%>%
      dplyr::mutate("stateAverageValue" = mean(indicator1),
                    "stateAverageScore" = mean(indicator2))
    
      
    ed2 <- ed2 %>%
      dplyr::mutate(
        popup = paste0(
          "<br/><strong>", as.character(in1),"</strong>", # needs to be text
          paste0("<br/><strong>",`County Name`,"</strong>"),
          paste0("<br/><b>Measured:</b> ", round(!!as.symbol(indicator1), digits = 2),
                   "<br/><b>Score:</b> ", round(!!as.symbol(indicator2), digits =  0)),
          paste0("<br/><b>Coal Community:</b> ", `Coal Community`),
          paste0("<br/><b>Oil and Gas Community:</b> ", `Oil and Gas Community`),
          paste0("<br/><b>Rural Community:</b> ", `Rural Community`)
      )
    )
    
    # palette 
    pal1 <- leaflet::colorNumeric(palette = colorRamp,
                                  domain = ed2$visParam,
                                  reverse = TRUE)
    # legend labels 
    labels1 <- defineLegend(in1)
    leafletProxy("mymap") %>%
      clearGroup(group = "Indicator Score") %>%
      addPolygons(
        data = ed2,
        color = "#F9C1AE", 
        weight = 0.2,
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
        
      )%>%
      removeControl(layerId = "firstLegend")%>%
      addLegend(
        "topright",
        colors = colorRamp,
        title = "Est. Values",
        labels = labels1,
        opacity = 1,
        layerId = "firstLegend",
        group = "Indicator Score"
        # labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))
      )
  })
  # add selected features to map 
  observeEvent(input$button_table, {
    mapFeatures <- envoData %>% select("GEOID") %>% dplyr::filter(GEOID %in% RV$select)
      # add features to map 
      leafletProxy("mymap") %>%
        clearGroup(group = "Table Highlight") %>%
        addPolygons(
          data = mapFeatures,
          fillColor = "#fffb17",
          fillOpacity = 0.3,
          color = "#fffb17", 
          options = pathOptions(pane = "index"),
          group = "Table Highlight"
        )
    })
  
  # remove selected features from map 
  observeEvent(input$removeHighlight, {
    # add features to map 
    leafletProxy("mymap") %>%
      clearGroup(group = "Table Highlight") 
  })

  # click observer event ----------------------------------------------------
  observeEvent(input$mymap_shape_click, {
    geoidMap <- input$mymap_shape_click$id  # typo was on this line
  })
}

# Run the application
shinyApp(ui = ui, server = server)
## display in browser for styling elements 
# options = list(launch.browser = TRUE)
