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
  dplyr::mutate(visParam = `Colorado Enviroscreen Score_pcntl`)

# test1 <- envoData %>%
#   dplyr::filter(area == "Census Tract")%>%
#   dplyr::select("GEOID", "County Name")

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
    ,"Socioeconomics & Demographics"                                      
    ,"Environmental exposures"                                
    ,"Environmental effects"                                   
    ,"Climate Vulnerability"                                  
    ,"Sensitive population"                                   
    ,"Demographics"
  )

# set empty parameter for histogram funciton 
## set to non GEOID number for the histogram generate on loading. 
geoidMap <- "100"

### annotations for histograms 
annotations = list( 
  list(x = 0.05,  
    y = 1.0,  
    text = "<b> Colorado Enviroscreen Score </b>",  
    font=list(size=16),
    xref = "paper",  
    yref = "paper",  
    xanchor = "center",  
    yanchor = "bottom",  
    showarrow = FALSE),
  list(x = 0.26,  
    y = 1.0,  
    text = "<b> Environmental exposures </b>",  
    font=list(size=16),
    xref = "paper",  
    yref = "paper",  
    xanchor = "center",  
    yanchor = "bottom",  
    showarrow = FALSE),
  list(x = 0.42,  
    y = 1.0,  
    text = "<b> Environmental effects</b>",  
    font=list(size=16),
    xref = "paper",  
    yref = "paper",  
    xanchor = "center",  
    yanchor = "bottom",  
    showarrow = FALSE ),
  list(x = 0.58,  
    y = 1.0,  
    text = "<b> Climate Vulnerability </b>",  
    font=list(size=16),
    xref = "paper",  
    yref = "paper",  
    xanchor = "center",  
    yanchor = "bottom",  
    showarrow = FALSE ),
  list(x = 0.74,  
    y = 1.0,  
    text = "<b> Sensitive population </b>",  
    font=list(size=16),
    xref = "paper",  
    yref = "paper",  
    xanchor = "center",  
    yanchor = "bottom",  
    showarrow = FALSE ),
  list(x = 0.9,  
    y = 1.0,  
    text = "<b> Demographics </b>", 
    font=list(size=16),
    xref = "paper",  
    yref = "paper",  
    xanchor = "center",  
    yanchor = "bottom",  
    showarrow = FALSE 
  )
)




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
  shiny::titlePanel( title=div("Colorado Enviroscreen - Beta Test Version February 2022", img(src="MountainsToPlains.png")
                               ,windowTitle = "Colorado Enviroscreen - Beta ")),
  
  fluidRow(style={"padding-left:100px;padding-right:100px;"},
    p(HTML("</br><a href='#map'>Jump to Map</a>")),
    p("The Colorado Department of Public Health and Environment and a team at Colorado State University are working on an enhanced environmental health screening tool for Colorado. This interactive mapping tool is called CO EnviroScreen. 
      To learn more about the project please vist CDPHE ",
      tags$a(href = "https://cdphe.colorado.gov/enviroscreen", "Colorado EnviroScreen webpage. ", target = "_blank"),
      "The tool will enable users to identify disproportionately impacted (DI) communities based on the definition in Colorado’s Environmental Justice Act (HB21-1266). CO EnviroScreen will be one way Colorado addresses current and historic inequities.
      The mapping tool aims to:"),
    p(
      tags$ol(
        tags$li("Pinpoint areas that have a disproportionate burden of health and/or environmental harm."),
        tags$li("Help users maximize funding and resources for policy changes and other interventions to avoid, minimize, and mitigate environmental health risks."),
        tags$li("Advance a healthy and sustainable Colorado where everyone has the same degree of protection from environmental and health hazards.")
      )
    ),
    br(),
    p("Click here for more information about CDPHE’s work to",
      tags$a(href = "https://cdphe.colorado.gov/environmental-justice", "advance environmental justice", target = "_blank"),
      ", ",
      tags$a(href = "https://cdphe.colorado.gov/environment/toxicology-and-environmental-epidemiology", "understand the connection between the environment and our health", target = "_blank"),
      ", and ",
      tags$a(href = "https://cdphe.colorado.gov/air-pollution/climate-change#equity", "advance climate equity.", target = "_blank"),
     ),

  ),
  fluidRow(style={"padding-left:100px;padding-right:100px;"},
           p("Scroll down for addation information on how to use this resource and the current results of the Colorado Enviroscreen.")
  ),


  # description of use ------------------------------------------------------
  tags$style(HTML("
        .a.nav-link.active{
    	background: #ffcd00
        }")),
  fluidRow(style = {"border-style: solid; borderColor=:#4d3a7d;"},
    h2("Understanding the Enviroscreen Tool")  
  ),
  br(),
  tabsetPanel(
    tabPanel("EnviroScreen score",
             tags$strong("EnviroScreen score"),
             p(
               "Colorado EnviroScreen is an environmental justice mapping tool that uses 
               population and environmental factors to calculate an EnviroScreen score.
               A higher EnviroScreen score means the area is more likely to be affected 
               by environmental injustice. There are scores for each county, census tract, 
               and census block group in Colorado. CDPHE expects to make improvements and
               additions to the tool in response to feedback or as new data become available.",
               br(),
               "Although EnviroScreen provides a robust measure of cumulative environmental 
               burden, it is not a perfect tool. The tool uses limited environmental and 
               sociodemographic data to calculate the EnviroScreen score. "
             ),
             p(
               "Colorado EnviroScreen does not:",
               
               tags$ol(
                 tags$li("Define all areas that might be affected by environmental injustice or specific environmental burdens."),
                 tags$li("Tell us about individuals who have health problems that make them more likely to experience negative effects from environmental exposures."),
                 tags$li("Take all environmental exposures into account."),
                 tags$li("Tell us about smaller areas within a county, census tract, or census block groups that may be more vulnerable to environmental exposures."),
              )
             ),
    ),
    tabPanel("Using the Map",
             tags$strong("Using the map"),
             p(
               "The default view of the map shows the state of Colorado. Individual counties,
               census tracts, or census block groups are color-coded, based on their climate
               equity score. The legend on the right-hand side of the map shows what these
               colors represent. The lighter colors mean a worse EnviroScreen score",
               br(),
               "The data viewer map is interactive. Users can zoom in and out, drag
               the map to a different location,  and click on a location to
               learn more about its EnviroScreen score. Explore these features:",
               tags$ol(
                 tags$li("Use your mouse or trackpad to scroll up and down to zoom in and out of the map."),
                 tags$li("Click, hold, and move your cursor to drag to a new location."),
                 tags$li("Click on a location to view more information for that location in the graph and table below the map.")
                 ),
               br(),
               "Users can search for an address, city, ZIP code, or place name using the magnifying glass icon and “Search using OSM Geocoder” search bar.",
             )
            ),
    tabPanel("Understanding the data",
             tags$strong("Understanding the data"),
             p(
               "EnviroScreen displays a cumulative impact EnviroScreen score derived from
               relative rankings (percentiles) of individual data indicators. These individual
               data indicators are aggregated into topic- based sub-components. These 
               sub-components are aggregated further into the Environmental Burden and
               Population Characteristics scores.  These two scores are multiplied together
               to get the EnviroScreen score. In this tool, the EnviroScreen percentile is 
               the default data visualized on the map. Sub-component scores and individual 
               datasets are also available for display." 
               
             ),
    ),
    tabPanel("Example",
             tags$strong("Example"),
      p("A non-profit group wants to apply for a grant it will use to install more air 
        pollution monitoring in its community. The grant rules say applicants
        have to show that their community needs the funds. The group knows science-based 
        information will strengthen its application. The group uses Colorado EnviroScreen
        to get more information about their community.",
        br(),
        "Here’s how the community group would use the tools in Colorado EnviroScreen:",
        tags$ol(
          tags$li("The non-profit staff navigates to the Colorado EnviroScreen webpage."),
          tags$li("They use  the “search” feature to find the census tract where their community office is located. "),
          tags$li("They click this area on the map and scroll below the map to view the additional information."),
          tags$li("They compare their overall and component EnviroScreen scores to compare their community with the rest of the state. "),
          tags$li("They download the data for their community at the bottom of the page."),
          tags$li("They use this information to write a compelling grant application for community air monitors.")
        )
      ), 
    ),
    tabPanel("Additional Content",
             tags$em("Coming Soon!")
            )
    ),



  # Select Reactive Elements ------------------------------------------------
  # content for the reactive elements of the map
  br(), 
  # h2("Colorado Enviroscreen Displayed"),
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
          "Group Component Scores" = c("Pollution and Climate Burden", "Socioeconomics & Demographics"),
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
          "Climate Vulnerability" = c(
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
      tags$div(title="Click to remove highilights features",
               actionButton("button_remove", "Remove Highlighted Areas")
      )
    ),
    br(),
    tags$blockquote(textOutput("indicatorDesc")),
  ),
  
  
  # display map -------------------------------------------------------------
  fluidRow(tags$style(type = "text/css", "#mymap {height: calc(100vh - 280px) !important;}"),style = {"background-color:#4d3a7d;"},
           column(1),
           column(10, leafletOutput("mymap")),
           column(1)
  ),

  # describe indicators -----------------------------------------------------
  # sentence explaining the indicators
  # 
  # show plots --------------------------------------------------------------
  # plot of the datasets
  br(),
  fluidRow(style = {"border-style: solid; borderColor=:#4d3a7d;"},
    plotlyOutput("plot2",width = "100%"),
    # Describe plots  --------------------------------------------------------
    # paragraph explaining the plot
    # h2("Understanding the Charts"),
    p("The EnviroScreen score combines five components. When you click a location
    on the map, the orange bars in this chart will show the score for that
    location. The orange bars show how the location compares to the rest of
    Colorado for each component score. Together, the charts show how the 
    EnviroScreen score is calculated for the selected location."),
    
  ),
  
  # show reactive table -----------------------------------------------------
  # table showing the results
  h2("Enviroscreen Score Data"),
  p("Use the tabs on top of the table to filter through different elements of the 
    Colorado Enviroscreen score. You can select specific rows on the table, then hit the
    `Highlight Selection on Map` button to view where in the Colorado those areas are found."),
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
              tabPanel("Environmental effects", dataTableOutput("evnEf")),
              tabPanel("Climate Vulnerability", dataTableOutput("clim")),
              tabPanel("Sensitive population", dataTableOutput("senPop")),
              tabPanel("Demographics", dataTableOutput("socEco"))
    ),
  # download table option  --------------------------------------------------
  fluidRow(
    # column(2,selectInput("download", "Choose a dataset:",
    #               choices = c("All Data" 
    #                           # ,"Group Component Scores"
    #                           # ,"Component Score"
    #                           # ,"Environmental Exposures"
    #                           # ,"Environmental Effects"
    #                           # ,"Climate"
    #                           # ,"Sensitive Population"
    #                           # ,"Socioeconomic"
    #                           )
    #               ),
    # ),
    column(2,downloadButton("downloadData", "Download Data for Current Geography"))
  ),
  
  h3("Additional Resources"),
  em("Coming Soon!"),
  
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
  plotGroup <- c( "Colorado Enviroscreen Score", "Environmental exposures","Environmental effects",
                  "Climate Vulnerability","Sensitive population","Demographics")
  # output for ployly
  output$plot2 <- renderPlotly({
    plots1 <- list()
    for(i in seq_along(plotGroup)){
      plots1[[i]] <- genPlots(dataframe = df1(),parameter = plotGroup[i], geoid = input$mymap_shape_click)
    }
    
    subplot(plots1, nrows = 1, shareY = TRUE, titleX = TRUE)%>%
      layout(annotations = annotations)
    
    
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
    ,"County Name"
    ,"Colorado Enviroscreen Score"                                  
    ,"Colorado Enviroscreen Score_pcntl"                            
    ,"Pollution and Climate Burden"                                 
    ,"Pollution and Climate Burden_pcntl"                           
    ,"Socioeconomics & Demographics"                                            
    ,"Socioeconomics & Demographics_pcntl"
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
    "County Name"
    ,"Environmental exposures",
    "Environmental effects",
    "Climate Vulnerability",
    "Sensitive population",
    "Demographics"
  ))
  # enviromental exposures Score
  output$evnEx <- renderDataTable(tableData() %>% dplyr::select(
    "GEOID"
    ,"County Name"
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
    ,"County Name"
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
    ,"County Name"
    ,"Wildfire Risk"                                          
    ,"Wildfire Risk_pcntl"                                     
    ,"Flood Plains"                                           
    ,"Flood Plains_pcntl"
  ))
  # component Score
  output$senPop <-  renderDataTable(tableData() %>% dplyr::select(
    "GEOID"
    ,"County Name"
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
    ,"County Name"
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
      mutate(visParam = !!as.symbol(indicator))%>%# https://stackoverflow.com/questions/62862705/r-shiny-mutate-replace-how-to-mutate-specific-column-selected-from-selectinput
      dplyr::select(GEOID, `County Name`, indicator1, indicator2,coal,oilGas,rural,visParam)
    
      
    ed2 <- ed2 %>%
      dplyr::mutate(
        popup = paste0(
          "<br/><strong>", as.character(in1),"</strong>", # needs to be text
          paste0("<br/><strong>",`County Name`,"</strong>"),
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
