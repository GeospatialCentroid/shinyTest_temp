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


# source helpers ----------------------------------------------------------
lapply(list.files(path = "src",recursive = TRUE, full.names = TRUE), source)


# enviroscreen data
envoData <- readRDS("data/scores/allScores.rda")%>%
  dplyr::mutate(visParam = `Colorado EnviroScreen Score Percentile`)%>%
  dplyr::select("County Name", "GEOID", everything())


# Additional Data 
oil <- readRDS("data/scores/oilgasVis.rda") 
coal <- readRDS("data/scores/coalVis.rda")
rural <- readRDS("data/scores/ruralVis.rda")
descriptors <- read_csv("data/descriptions/indicatorDesc.csv")

# di community 
di <- getDI()
# palette for DI layer 
diPal <- colorFactor(palette = c(
  "#fdd18a", "#ade1e9","#51a198","#c095b4"), levels = c("Low Income", "People of Color",
                                                       "Housing Burden", "More then one category"), di$color
)


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
  shiny::titlePanel( title="Colorado EnviroScreen - Beta Test Version February 2022",windowTitle = "Colorado Enviroscreen - Beta "),
  
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
  # tags$style(HTML("
  #       .a.nav-link.active{
  #   	background: #ffcd00
  #       }")),
  fluidRow(class = "boarderElement", 
    h2("Understanding the EnviroScreen Tool")  
  ),
  br(),
  tabsetPanel(
    tabPanel("EnviroScreen Score",
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
    tabPanel("Additional Content",
             br(),
             p(
               tags$em("Coming Soon!") 
             ),
             br(),
            )
    ),



  # Select Reactive Elements ------------------------------------------------
  # content for the reactive elements of the map
  br(), 
  # h2("Colorado Enviroscreen Displayed"),
  fluidRow(class = "boarderElement", id = "map", 
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
          "EnviroScreen Score" = "Colorado EnviroScreen Score",
          "Group Component Scores" = c("Pollution & Climate Burden", "Socioeconomics & Demographics"),
          "Individual Component Scores" =c("Environmental exposures",
                                           "Environmental effects",
                                           "Climate vulnerability",
                                           "Sensitive population",
                                           "Demographics"),
          "Environmental exposures" = c("Ozone"                                                  
                                        ,"Particles"                                 
                                        ,"Lead exposure risk"                                    
                                        ,"Diesel PM"                              
                                        ,"Traffic proximity & volume"                                        
                                        ,"Air toxics emissions" 
                                      ),
          "Environmental effects" = c(
            "Wastewater discharge indicator"                                  
            ,"Proximity to National Priorities List (NPL) sites"                           
            ,"Proximity to RMP sites"                
            ,"Proximity to hazardous waste facilities"
          ),
          "Climate vulnerability" = c(
            "Wildfire risk"                                          
            ,"Floodplains" 
          ),
          "Sensitive population" = c(
            "Population under 5"                                  
            ,"Population over 64"                            
            ,"Heart disease in adults"                                          
            ,"Asthma hospitalization rate"                                                 
            ,"Life expectancy"                                        
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
        selected = "Colorado EnviroScreen Score",
        width = "90%"
      )
      )
    ),
    # toggle between measured and percentile
    column(
      3,
      tags$div(title="Click here to show measured value or rank of the variable",
      selectInput(
        inputId = "Percentile",
        label = "Measure or Percentile",
        choices = c("Measured Value", "Percentile Rank"),
        selected = "Percentile Rank"
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
      tags$div(title="Click here to remove highlighted features",
               actionButton("button_remove", "Remove Highlighted Areas")
      )
    ),
    br(),
    tags$blockquote(textOutput("indicatorDesc")),
    p(
      tags$strong("Coal Community"),": Areas designated Coal Community are census block groups that have a coal-burning power plant within the county. All census tracts and block groups within these counties are considered coal communities.",
      br(),
      tags$strong("Oil & Gas Community"),": Oil and Gas Communities are  census block groups that have active oil and gas operations within the county. All census tracts and block groups within these counties are considered oil and gas communities.",
      br(),
      tags$strong("Rural Community"),": The U.S. Census Bureau's urban areas are densely populated and include residential, commercial, and other properties. Counties that include these urban areas are considered urban. Rural Communities encompass all counties not included within urban counties.",
    )
  ),
  
  
  # display map -------------------------------------------------------------
  fluidRow(tags$style(type = "text/css", "#mymap {height: calc(100vh - 280px) !important;}"),style = {"background-color:#4d3a7d;"},
           #column(1),
           column(8, leafletOutput("mymap")),
           column(4, plotlyOutput("histEnviroScreen",height = "100%"))
  ),

  # describe indicators -----------------------------------------------------
  # sentence explaining the indicators
  # 
  # show plots --------------------------------------------------------------
  # plot of the datasets
  br(),
  fluidRow(class = "boarderElement",
           column(1),
           column(2,plotlyOutput("histExposure")),
           column(2,plotlyOutput("histEffect")),
           column(2,plotlyOutput("histClimate")),
           column(2,plotlyOutput("histSocial")),
           column(2,plotlyOutput("histDemo")),
           column(1),
    p("The EnviroScreen score combines five components: Environmental exposures, Environmental effects, Climate vulnerability, Sensitive population, and Demographics. When you click a location on the map, the orange bars in this chart show the score for that location. The orange bars show how the location compares to the rest of Colorado for each component score. Together, the charts show how the EnviroScreen score is calculated for the selected location.")
  ),
  br(),
  
  # show reactive table -----------------------------------------------------
  # table showing the results
  fluidRow(class = "boarderElement",
           h2("EnviroScreen Score Data"),
           p("Use the tabs above  the table to filter through different elements of the
    Colorado EnviroScreen Score. You can select specific rows in the table, then hit the
    Blue `Highlight Selection on Map` button in the upper right to view the location on the map."),
  ),
   # add selection to map button 
  fluidRow(column(
    2, offset = 10,
    tags$div(title="Click here to add selections to map display",
             actionButton("button_table", "Highlight Selection on Map")
    )
  )),
  # Output: Tabset w/ plot, summary, and table ----
  tabsetPanel(type = "tabs",
              tabPanel("Group Component Scores", dataTableOutput("gcomponentScore")),
              tabPanel("Component Score", dataTableOutput("componentScore")),
              tabPanel("Environmental exposures", dataTableOutput("evnEx")),
              tabPanel("Environmental Effects", dataTableOutput("evnEf")),
              tabPanel("Climate vulnerability", dataTableOutput("clim")),
              tabPanel("Sensitive population", dataTableOutput("senPop")),
              tabPanel("Demographics", dataTableOutput("socEco"))),
  # download table option  --------------------------------------------------
  fluidRow(
    column(2,downloadButton("downloadData", "Download Data for Current Geography"))
  ),
  
  h3("Additional Resources"),
  p(
    "For more information about how to use environmental justice mapping tools, 
    please review the Climate Equity Data Viewer guide (available in", 
    tags$a(href = "https://drive.google.com/file/d/1iytdPG5iK2VBNpIy8k6oT6lU6-QKMLOa/view?usp=sharing", "English", target = "_blank"),
    "and ",
    tags$a(href = "https://drive.google.com/file/d/17rQ90fNt3DF-0PbySpGjo2tiy9AmDiCc/view?usp=sharing", "Spanish", target = "_blank"),
    ")."
  ),
  
  br( ),
  br( ),
  br( ),
  br( ),
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
     createMap(mapData = mapData, di = di, diPal = diPal,
              pal = colorRamp, palMap = palMap,
              oil=oil, rural = rural, coal = coal)
    })

# # indicator summary -------------------------------------------------------
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
    
  
  
  # ### 
  # # output for ployly
  # output$plot2 <- renderPlotly({
  #   plots1 <- list()
  #   for(i in seq_along(plotGroup)){
  #     plots1[[i]] <- genPlots(dataframe = df1(),parameter = plotGroup[i], geoid = input$mymap_shape_click)
  #   }
  #   
  #   subplot(plots1, nrows = 1, shareY = TRUE, titleX = TRUE)%>%
  #     layout(annotations = annotations)
  #   
  #   
  # })
  
  
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
    ,"County Name"
    ,"Colorado EnviroScreen Score"                                  
    ,"Colorado EnviroScreen Score Percentile"                            
    ,"Pollution & Climate Burden"                                 
    ,"Pollution & Climate Burden Percentile"                           
    ,"Socioeconomics & Demographics"                                            
    ,"Socioeconomics & Demographics Percentile"
    ), 
    rownames = FALSE,
    options = list(autoWidth = TRUE, scrollX = TRUE,
              scrollY = "200px", scrollCollapse = TRUE,
              paging = FALSE, float = "left"),
     width = "80%", height = "70%")
  
  
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
    "County Name"
    ,"Environmental exposures",
    "Environmental effects",
    "Climate vulnerability",
    "Sensitive population",
    "Demographics"
  ), 
  options = list(autoWidth = TRUE, scrollX = TRUE))
  # enviromental exposures Score
  output$evnEx <- renderDataTable(tableData() %>% dplyr::select(
    "GEOID"
    ,"County Name"
    ,"Ozone"
    ,"Ozone Percentile"
    ,"Particles"                                              
    ,"Particles Percentile"                                         
    ,"Lead exposure risk"                                     
    ,"Lead exposure risk Percentile"                                
    ,"Diesel PM"                                              
    ,"Diesel PM Percentile"                                         
    ,"Traffic proximity & volume"                           
    ,"Traffic proximity & volume Percentile"                      
    ,"Air toxics emissions"                                   
    ,"Air toxics emissions Percentile"
  ), 
  options = list(autoWidth = TRUE, scrollX = TRUE))
  # enviromental effects Score
  output$evnEf <- renderDataTable(tableData() %>% dplyr::select(
    "GEOID"
    ,"County Name"
    ,"Wastewater discharge indicator"                         
    ,"Wastewater discharge indicator Percentile"                    
    ,"Proximity to National Priorities List (NPL) sites"      
    ,"Proximity to National Priorities List (NPL) sites Percentile" 
    ,"Proximity to RMP sites"                                 
    ,"Proximity to RMP sites Percentile"                            
    ,"Proximity to hazardous waste facilities"                
    ,"Proximity to hazardous waste facilities Percentile"
  ), 
  options = list(autoWidth = TRUE, scrollX = TRUE))
  # component Score
  output$clim <- renderDataTable(tableData() %>% dplyr::select(
    "GEOID"
    ,"County Name"
    ,"Wildfire risk"                                          
    ,"Wildfire risk Percentile"                                     
    ,"Floodplains"                                           
    ,"Floodplains Percentile"
  ), 
  options = list(autoWidth = TRUE, scrollX = TRUE))
  # component Score
  output$senPop <-  renderDataTable(tableData() %>% dplyr::select(
    "GEOID"
    ,"County Name"
    ,"Population under 5"                                     
    ,"Population under 5 Percentile"                                
    ,"Population over 64"                                     
    ,"Population over 64 Percentile"                                
    ,"Heart disease in adults"                                
    ,"Heart disease in adults Percentile"                           
    ,"Asthma hospitalization rate"                            
    ,"Asthma hospitalization rate Percentile"                       
    ,"Life expectancy"                                        
    ,"Life expectancy Percentile"                                   
    ,"Low weight birth rate"                                  
    ,"Low weight birth rate Percentile"
  ), options = list(autoWidth = TRUE, scrollX = TRUE))
  # component Score
  output$socEco <- renderDataTable(tableData() %>% dplyr::select(
    "GEOID"
    ,"County Name"
    ,"Percent people of color"                                
    ,"Percent people of color Percentile"                           
    ,"Percent less than high school education"                
    ,"Percent less than high school education Percentile"           
    ,"Percent low income"                                     
    ,"Percent low income Percentile"                                
    ,"Percent linguistic isolation"                           
    ,"Percent linguistic isolation Percentile"                      
    ,"Percent disability"                                     
    ,"Percent disability Percentile"
  ), 
  options = list(autoWidth = TRUE, scrollX = TRUE))
  # download data -----------------------------------------------------------
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$Geom, "_data.csv", sep = "")
    },
    content = function(file) {
      write.csv(df1() %>% sf::st_drop_geometry(), file, row.names = FALSE)
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
      dplyr::select(GEOID, `County Name`, indicator1, indicator2,coal,oilGas,rural,visParam)
    
      
    ed2 <- ed2 %>%
      dplyr::mutate(
        popup = paste0(
          "<br/><strong>", as.character(in1),"</strong>", # needs to be text
          paste0("<br/><strong>",`County Name`,"</strong>"),
          paste0(if(in1 %in% c("Colorado EnviroScreen Score",
                                    "Pollution & Climate Burden",
                                    "Socioeconomics & Demographics",
                                    "Environmental exposures",
                                    "Environmental Effects",
                                    "Climate vulnerability",
                                    "Sensitive population",
                                    "Demographics"
          )){
            paste0("<br/><b>Percentile:</b> ", round(!!as.symbol(indicator2), digits =  0))
          }else{
            paste0("<br/><b>Measured:</b> ", round(!!as.symbol(indicator1), digits = 2),
                   "<br/><b>Percentile:</b> ", round(!!as.symbol(indicator2), digits =  0))
          }),
          paste0("<br/><b>Coal Community:</b> ", coal),
          paste0("<br/><b>Oil and Gas Community:</b> ", oilGas),
          paste0("<br/><b>Rural Community:</b> ", rural)
      )
    )
    
    # palette 
    pal1 <- leaflet::colorNumeric(palette = colorRamp,
                                  domain = ed2$visParam,
                                  reverse = TRUE)
    # legend labels 
    labels1 <- defineLegend(in1)
    print(labels1)
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
    print(RV$Clicks)
    mapFeatures <- envoData %>% select("GEOID") %>% dplyr::filter(GEOID %in% RV$Clicks)
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
