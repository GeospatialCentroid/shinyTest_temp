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

version <- 4

# enviroscreen data
envoData <- readRDS(paste0("data/scores/allScores_",version,".rds"))%>%
  dplyr::mutate(visParam = `Colorado EnviroScreen Score Percentile`)%>%
  dplyr::select("County Name", "GEOID", everything())%>% 
  dplyr::select(-"GEOID3")


# Additional Data 
oil <- readRDS("data/scores/oilgasVis.rds") 
coal <- readRDS("data/scores/coalVis.rds")
rural <- readRDS("data/scores/ruralVis.rds")
descriptors <- read_csv("data/descriptions/indicatorDesc.csv") %>% 
  dplyr::select(1:6)%>%
  `colnames<-`(c("Indicator", "Source", "Date", "Units", "Measured_Geography", "Description"))
justice40 <- readRDS("data/scores/justice40.rds") %>%
  dplyr::mutate(popup = paste0(
    "Census Tract ", GEOID ," in ", County_Name," County."    
    ,br()
    ,"A total of ", Total.threshold.criteria.exceeded," clauses defined this area as disadvantaged."
    ,br()
    ,br()
    ,paste0("<strong>Definition: </strong>")
    ,"In early 2022, the White House launched the Justice40 Initiative. The goal of the Justice40 Initiative is to provide 40 percent of the overall benefits of certain Federal investments in seven key areas to disadvantaged communities. These seven key areas are: climate change, clean energy and energy efficiency, clean transit, affordable and sustainable housing, training and workforce development, the remediation and reduction of legacy pollution, and the development of critical clean water infrastructure. According to the definition of Justice40, a community qualifies as “disadvantaged,” if the census tract is above the threshold for one or more environmental or climate indicators and the tract is above the threshold for the socioeconomic indicators."
    ))

# di community 
di <- getDI()
# storyMap Locations 
sm <- getStoryMaps()

# palette for DI layer 
diPal <- colorFactor(palette = c(
  "#a6cee3", "#33a02c","#b2df8a","#1f78b4"), levels = c("Low Income", "People of Color",
                                                       "Housing Burden", "More then one category"), di$color
)

### light as low 
#colorRamp <- c(  "#f2f0f7"  ,"#cbc9e2"  ,"#9e9ac8"  ,"#756bb1"  ,"#54278f")
### dark as low 
colorRamp <- c( "#54278f","#756bb1","#9e9ac8","#cbc9e2","#f2f0f7")



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
  heading_font = "Trebuchet MS,sans-serif"
    )%>%
    bslib::bs_add_rules(sass::sass_file("www/style.scss")),

  # Title ------------------------------------------------------------------
  fluidRow(
    class = "titleElement",
    column(4,
           tags$br(), 
           tags$a(
             href = "https://cdphe.colorado.gov/enviroscreen",
             tags$img(
               src="EnviroScreen Logos/co_cdphe_pr_es_white_v.png",
               title = "Colorado Department of Public Health and Environment",
               width="70%",
               height="auto" 
             )
           )
          ),
    column(8, h1("Colorado EnviroScreen"), p("Open Beta April 2022"))
  ),
  br(),
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
    tabPanel(title = "Purpose and Limitations",
             br(),
             p(
               tags$strong("EnviroScreen Score: Purpose and Limitations")
               ,br()
               ,"Colorado EnviroScreen is an environmental health justice mapping tool that uses population and environmental factors to calculate an “EnviroScreen score.” A higher EnviroScreen score means the area is more likely to be affected by environmental health injustice. The tool includes scores for each county, census tract, and census block group in Colorado. CDPHE expects to make improvements and additions to the tool in response to feedback and as new data become available. Please note that areas under the Ute Mountain Ute and Southern Ute Tribal Jurisdictions are not currently represented on the map. Although EnviroScreen provides a robust measure of cumulative environmental burden, it is not a perfect tool. The tool uses limited environmental, health, and sociodemographic data to calculate the EnviroScreen score."
             ),
             p(
               tags$strong("Colorado EnviroScreen does not:")
               ,tags$ul(
                 tags$li("Define all areas that may be affected by environmental health injustice or specific environmental burdens."),
                 tags$li("Provide information at the individual level from those who have health problems that make them more likely to experience negative effects from environmental exposures."),
                 tags$li("Take all environmental exposures into account."),
                 tags$li("Tell us about smaller areas within a census block group that may be more vulnerable to environmental exposures than other areas."),
              )
             ),
             p(
               tags$strong("Colorado EnviroScreen does:"),
               tags$ul(
                 tags$li("Show which areas in Colorado are more likely to have higher environmental health injustices."),
                 tags$li("Identify areas in Colorado where government agencies can prioritize resources and work to reduce pollution and other sources of environmental health injustices."),
                 tags$li("Provide information to empower community members to advocate to improve public health and the environment in their communities."),
                 tags$li("Identify areas that meet the definition of “Disproportionately Impacted Community” under the Colorado Environmental Justice Act (HB21-1266)."),
               )
             ),
          ),
    tabPanel(title = "How to Use the Map",
             br(),
             p(
               tags$strong("How to Use the Map")
               ,br()
               ,"The default view of the map shows the state of Colorado. Individual counties, census tracts, or census block groups are color-coded based on their EnviroScreen score. The legend on the right side of the map shows what each color represents. The lighter the color, the worse the EnviroScreen score. Users can zoom in and out, drag the map to a different location, and click on a location to learn more about its EnviroScreen score and scoring details."
               ,br()
             ),
            tags$img(
                   id = "mapDesc", 
                   src="MapElements_3_crop.jpg",
                   title = "Map Elements",
                   height="auto"
               ),
            p(
              br()
              ,"To use the map, follow the steps below."
             ),
            tags$h4("Step 1: Select your map settings."),
            p(
              tags$strong("First, Select Geo Scale:")
              ," Use the drop-down menu to view the map at the county level, census tract level, or census block group level. Proceed to"
              ,tags$em("Indicator")
            ),
            br(),
            p(
              tags$strong("Next, Select Indicator:")
              ," The default layer displayed on the map is the overall EnviroScreen Score, which is calculated from a variety of data sources. Click on the drop-down menu and scroll through the options to view the individual data sources that feed into the EnviroScreen score. Proceed to"
              ,tags$em("Measure or %.")
            ),
            br(),
            p(
              tags$strong("Third, Select Measure or Percentile:")
              ," Choose how to display the selected map layer. Measure displays the actual measurement of the data source and is most meaningful for individual data layers such as ozone or asthma hospitalizations. Percentile is like a ranking; the number represents the percentage of the state’s areas that have a lower score compared to the selected area." 
            ),
            br(),
            p(
              "Once you have made your selections, click"
              ,tags$strong(" Update Map.")
              ,"It may take a moment for the map to load."
            ),
            p(
              tags$em("Note: The")
              ,tags$strong(" Remove Highlighted Areas")
              ,tags$em(" button on the right side clears the highlighted areas. Users can highlight areas using the data table at the bottom of the tool.")  
            ),
            tags$h4("Step 2: Interact with the map."),
            p(
              "Individual counties, census tracts, or census block groups are color-coded based on the map layer selected. The legend on the right side of the map shows what these colors represent. The lighter colors mean greater values, which represent a worse EnviroScreen score. Use the icons in the upper left of the map to zoom in and out, search for a specific address, center the map, customize the background map color, or add additional available map layers."
              ,br()
              ,br()
              ,tags$strong("Learn more about an area")
              ,br()
              ,"Click an area on the map to learn more. The pop-up window displays the value selected in the settings toolbar above the map. The graph and table below the map display more area-specific information."
              ,br()
              ,br()
              ,tags$strong("Search for an address")
              ,br()
              ,"Search for an address, city, ZIP code, or place name using the magnifying glass icon and “Search using OSM Geocoder” search bar."
              ,br()
              ,br()
              ,tags$strong("Customize and add context (additional available map layers)")
              ,br()
              ,"On the left side of the map, customize the display by selecting the background map or by adding additional information about oil and gas, coal power plants, Justice40 areas, or disproportionately impacted communities."
            ),
            tags$h4("Step 3: Explore the data in another way."),
            p(
              "Learn more about a selected area"
              ,tags$b(" below the map.")
              ,br()
              ,br()
              ,tags$strong("Bar Charts")
              ,br()
              ,"The bar chart on the right side of the map illustrates the overall EnviroScreen score and the bar charts below the map illustrate the scores of each of the five components that make up the overall score. The selected area is displayed in orange. These visualizations illustrate whether an area is more or less affected than other parts of the state for each category."
              ,br()
              #insert image 
              ,tags$img(
                id = "histoDesc", 
                src="histoDesc.png",
                title = "Bar Charts Elements",
                height="auto"
              )
              ,br()
              ,tags$strong("Data Table")
              ,br()
              ,"The area selected in the map also filters the data table below the charts. Explore the data by sorting the table. Select a row or rows to highlight the selection in the map. For example, a user could sort the table to find the areas with the highest climate vulnerability score, select rows in the table, and click “Highlight Selection on Map.” The areas selected in the table will be highlighted on the map."
            )
        ),
    tabPanel(title = "Understanding the Data",
             br()
             #insert image 
             ,tags$img(
               id = "scoreDesc", 
               src="scoreDesc.png",
               title = "Score Calculation",
               height="auto"
             )
             ,br()
             ,p(
               tags$strong("Understanding the Data")
               ,br()
               ,br()
               ,tags$strong("EnviroScreen Score")
               ,br()
               ,"Colorado EnviroScreen maps the overlap of environmental exposures and effects, climate vulnerability, sensitive populations, and demographics to understand environmental health injustice in Colorado. The EnviroScreen score is derived from relative rankings (percentiles) of individual data indicators. These individual indicators are aggregated into topic-based sub-components, such as climate vulnerability or demographics. These sub-components are aggregated further into the Pollution & Climate Burden and Socioeconomics & Demographics scores. These two scores are multiplied together to get the EnviroScreen score. The EnviroScreen score is the default data visualized on the map. Sub-component scores and individual datasets are also available for display on the map."
               ,br()
               ,"A higher EnviroScreen score means the area is more likely to be affected by environmental health injustices."
             ),
             p(
               tags$strong("Component Scores")
               ,br()
               ,"Users can view the components that make up the EnviroScreen score by selecting either of the Group Component Scores or any of the Individual Component Scores. Each of these scores is made up of multiple individual indicators, which can also be viewed on their own. These component scores represent the impact associated with the category. Like the overall EnviroScreen score, component scores reflect relative rankings (percentiles) of each input dataset" 
             ),
             p(
               tags$strong("Individual Indicators")
               ,br()
               ,"Individual data indicators can also be viewed on the map. In the"
               ,tags$b(" Select Layer for Map") 
               ," drop-down menu below, these individual indicators are organized under the component they feed into. For example, measures of air quality can be found under Environmental Exposures."
               ,br()
               ,"Each individual data indicator represents something different and uses a different unit of measure. For example, some indicators (such as air quality,) are estimated concentrations while others (such as heart disease) are rates of certain health outcomes."
             ),
             p(
               tags$strong("Data Sources")
               ,br()
               ,"Indicators included in Colorado EnviroScreen come from a variety of data sources. Many of these sources are from publicly available datasets from state and federal agencies, such as the US Environmental Protection Agency (EPA), the Centers for Disease Control and Prevention (CDC), the Colorado Department of Public Health and Environment (CDPHE), the Colorado Oil and Gas Conservation Commission (COGCC), and others."
               ,br()
               ,"Although there are many indicators included in EnviroScreen, some of these indicators are from a few years ago or represent measurements at a larger geographic scale. There are also some environmental exposures, climate impacts, health outcomes, and demographic factors that are not included in EnviroScreen since there are not any reliable data sources available."
             ),
          ),
    tabPanel(title = "Example Use",
             br(), 
            p(
              tags$strong("Example Use")
              ,br()
              ,"Here is just one example of how Colorado EnviroScreen can be used by stakeholders to meet their goals."
              ,br()
              ,"A non-profit group is applying for a grant to install more air pollution monitoring in its community. The grant rules specify that applicants must show that their community needs the funds. The group knows science-based information will strengthen its application. The group uses Colorado EnviroScreen to get more information about their community."
              ,br()
              ,"Here is how the community group could use Colorado EnviroScreen:"
              ,tags$ol(
                tags$li("The non-profit staff navigates to the Colorado EnviroScreen webpage")
                ,tags$li("They use the “Search” feature to find the census tract in which their office is located.")
                ,tags$li("They click this area on the map and scroll below the map to view additional information.")
                ,tags$li("They review how the overall and component EnviroScreen scores for their census tract compare with the rest of the state.")
                ,tags$li("They download the data for their community at the bottom of the page.")
                ,tags$li("They use this information to write a compelling grant application for community air monitors.")
              )
            ), 
          ),
    tabPanel(title =  "Definitions",
             br(),
             p(
             tags$strong("Definitions")
             ,br()
             ,br()
             ,tags$strong("EnviroScreen Score")
             ,br()
             ,"The EnviroScreen Score combines population characteristics and environmental burdens. The score goes from 0 to 100, with the highest score representing the highest burden."
             ,br()
             ,"The EnviroScreen score is a percentile, which is like a ranking. The number represents how many of the state’s counties, census tracts, or census block groups have a lower score than the geography in question."
             ,br()
             ,"Suppose a census block group has an EnviroScreen score of 50. This means its EnviroScreen score is higher than 50% of all census block groups in Colorado. A score of 90 means its EnviroScreen score is higher than 90% of all census block groups in Colorado."
             ,br()
             ,br()
             ,tags$strong("Environmental Exposures")
             ,br()
             ,"The Environmental Exposures score represents a community’s exposure to certain harmful environmental pollutants relative to the rest of the state. It does not represent all environmental pollutants.It is the average of data for air toxics, fine particles, traffic proximity, ozone, noise, drinking water quality,  lead exposure risk, and other air pollutants. Ranging from 0 to 100, the higher the score, the higher the burden."
             ,br()
             ,br()
             ,tags$strong("Environmental Effects")
             ,br()
             ,"This score represents how many facilities that can cause pollution or other health and safety risks are in a community relative to the rest of the state. Because most people are not directly exposed to these sites, this score is weighted half as much as environmental exposures in the overall environmental burden score. The score is the average of data involving proximity to oil and gas facilities, mining locations, National Priority List (Superfund) sites, wastewater discharge facilities, impaired surface waters, and risk management plans (RMPs). Ranging from 0 to 100, the higher the score, the higher the burden."
             ,br()
             ,br()
             ,tags$strong("Climate Burden")
             ,br()
             ,"This score represents a community’s risk of drought, flood, extreme heat, and wildfire relative to the rest of the state. Ranging from 0 to 100, the higher the score, the higher the burden."
             ,br()
             ,br()
             ,tags$strong("Sensitive Populations")
             ,br()
             ,"This score captures a community’s potential biological susceptibility to environmental exposures and climate risks. For example, air pollution has stronger impacts on older and younger people, and people with chronic conditions such as asthma. The result is calculated using data about asthma, cancer, diabetes, heart disease, low weight birth, mental health, and the numbers of people over 64 years and under 5 years. Ranging from 0 to 100, the higher the score, the higher the burden."
             ,br()
             ,br()
             ,tags$strong("Demographics ")
             ,br()
             ,"This score represents a community’s social vulnerability. It is calculated using data about education levels, linguistic isolation, income, housing cost burden, and race & ethnicity. Ranging from 0 to 100, the higher the score, the higher the social vulnerability."
             ,br()
             ,br()
             ,tags$strong("Disproportionately Impacted Community")
             ,br()
             ,'This term refers to areas that meet the definition of "Disproportionately Impacted Community” in the Colorado Environmental Justice Act (House Bill 21-1266). The definition includes census block groups where more than 40% of the population are low-income, housing cost-burdened, or people of color. “Low-income” means that median household income is at or below 200% of the federal poverty line. “Housing cost-burdened” means that a household spends more than 30% of its income on housing costs. “People of color” includes all people who do not identify as non-Hispanic white.'
             ,br()
             ,br()
             ,tags$strong("Coal Community")
             ,br()
             ,"All census tracts and block groups within counties that have a coal-burning power plant are designated as coal communities."
             ,br()
             ,br()
             ,tags$strong("Oil and Gas Community")
             ,br()
             ,"All census tracts and block groups within counties that have active oil and gas operations are designated as oil and gas communities."
             ,br()
             ,br()
             ,tags$strong("Urban/Rural")
             ,br()
             ,"The U.S. Census Bureau's `urban areas` are densely populated and include residential, commercial, and other properties; counties that include these urban areas are considered urban. All counties not included within urban centers are considered rural counties."
             ,br()
             ,br()
             ,tags$strong("Justice40")
             ,br()
             ,"In early 2022, the White House launched the Justice40 Initiative. The goal of the Justice40 Initiative is to provide 40 percent of the overall benefits of certain Federal investments in seven key areas to disadvantaged communities. These seven key areas are: climate change, clean energy and energy efficiency, clean transit, affordable and sustainable housing, training and workforce development, the remediation and reduction of legacy pollution, and the development of critical clean water infrastructure. According to the definition of Justice40, a community qualifies as “disadvantaged,” if the census tract is above the threshold for one or more environmental or climate indicators and the tract is above the threshold for the socioeconomic indicators."
             ,br()
             ,br()
             ,tags$strong("Bar Charts")
             ,br()
             ,"The EnviroScreen Score combines five components. When you click a location on the map, the orange bars in each chart show the score for that location, illustrating how it compares to the rest of Colorado for each component score. Together, the five charts show how the EnviroScreen score is calculated for the selected location."
             ,br()
          ),

    ),
    tabPanel("Tool Development",
             br()
             ,tags$strong("Development through partnership")
             ,p(
              "future content" 
             )
             ,br()
             ,tags$strong("Community engagement")
             ,p(
               "future content" 
             )
    ),
    tabPanel("Additional Resources",
             br()
             ,p(
               "future content" 
             )
    ),
    tabPanel("FAQ",
             br(),
             p(
               tags$a(
                 href = "https://docs.google.com/document/d/1_GEjGbOd3CmXwZu09QJ9oO4ZI8hqXtFwZAAeTsNV5lQ/edit?usp=sharing"
                 ,tags$em("Frequently Asked Questions")
                 , target = "_blank"
               ) 
             )
      ),
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
        label = "Indicator",
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
            "Proximity to Mining",
            "Proximity to National Priorities List (NPL) sites",
            "Proximity to Oil and Gas",
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
            "Cancer prevalence",
            "Diabetes prevalence",
            "Heart disease in adults",
            "Life expectancy",                                       
            "Low weight birth rate",
            "Mental Health Indicator",
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
    Orange `Highlight Selection on Map` button below the table to view the location on the map."),

  ),
  fluidRow(class = "dataTableArea",
  radioGroupButtons(inputId = "tableSelect", label = "",
                    choices = c("Group Component Scores", "Component Score",
                                "Environmental Exposures", "Environmental Effects",
                                "Climate Vulnerability", "Sensitive Population",
                                "Demographics", "Community Classifications", "Indicator Descriptions"),
                    #justified = TRUE
                    ),
  # data table output ----
  # changed to just single table
  DT::dataTableOutput("tableAll")
  ),
  
  # download table option  --------------------------------------------------
  fluidRow(
    column(3,
           offset = 1,
           tags$div(title="Click here to add selections to map display",
                    actionButton("button_table", "Highlight Selection on Map")
           ),
    ),
    column(3,
           tags$div(title="Click here to download content",
           downloadButton("downloadData", "Download Data for Current Geography")
           ),
    ),
    column(3,
           tags$div(title="Click here to download Indicator Descriptions",
                    downloadButton("downloadData2", "Download Indicator Descriptions")
           ),
    )
  ),
  br(),
  
  fluidRow( class = "titleElement", 
               column(4,   
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
        width="100%",
        height="auto" 
        )
      )
    ),
    column(4,tags$a(
      href = "https://www.colostate.edu/",
      tags$img(
        src="csu.png",
        title = "Colorado State University",
        width="50%",
        height="auto"
      )
    )
    )
  ),
  
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
              oil=oil, rural = rural, coal = coal, justice40 = justice40,
              storyMap = sm)
    })

  # indicator summary -------------------------------------------------------
  # output for indicator summary'
  desc1 <- descriptors
  output$indicatorDesc <- renderText({
    ind1 <- input$Indicator
    desc1 <- descriptors %>% dplyr::filter(Indicator == ind1) %>% dplyr::select("Description") %>% pull()
    paste0(input$Indicator," : ", as.character(desc1))
  })


  # histogram plots ---------------------------------------------------------
  # if input changes reset map value 
  plotGroup <- c( "Colorado EnviroScreen Score", "Environmental exposures","Environmental effects",
                  "Climate vulnerability","Sensitive population","Demographics")
  
  ### need individual output objects for each plot 
  output$histEnviroScreen <- renderPlotly({
    genPlots(dataframe = df1(),parameter = "Colorado EnviroScreen Score", geometry = input$Geom, geoid = input$mymap_shape_click)
  })
  output$histExposure<- renderPlotly({
    genPlots(dataframe = df1(),parameter = "Environmental exposures",geometry = input$Geom, geoid = input$mymap_shape_click)
  })
  output$histEffect<- renderPlotly({
    genPlots(dataframe = df1(),parameter = "Environmental effects",geometry = input$Geom, geoid = input$mymap_shape_click)
  })
  output$histClimate<- renderPlotly({
    genPlots(dataframe = df1(),parameter = "Climate vulnerability",geometry = input$Geom, geoid = input$mymap_shape_click)
  })
  output$histSocial<- renderPlotly({
    genPlots(dataframe = df1(),parameter = "Sensitive population",geometry = input$Geom, geoid = input$mymap_shape_click)
  })
  output$histDemo<- renderPlotly({
    genPlots(dataframe = df1(),parameter = "Demographics",geometry = input$Geom, geoid = input$mymap_shape_click)
  })
    
  # table output ------------------------------------------------------------  
  # output for datatable based on columns selected
  
  tableData <- reactive({
    geoid1 <- input$mymap_shape_click
    if(is.null(geoid1$id)){
      geoid1 <- 1
    }
    if(input$tableSelect == "Indicator Descriptions"){
      ## Need a GEOID value to support the click selection function 
      descriptors %>% 
        dplyr::mutate("GEOID" = NA)%>%
        dplyr::select("GEOID","Indicator Name"="Indicator","Data Source"= "Source",
                      "Date (data collection)"= "Date","Units","Measured Geography" = "Measured_Geography")
    }else{
      genTable(tableData = df1(), geoid = geoid1, colSelected = input$tableSelect)
    }
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
      write.csv(df1() %>% sf::st_drop_geometry() %>% select(-"visParam"), file, row.names = FALSE)    }
  )
  # Downloadable csv of data description ----
  output$downloadData2 <- downloadHandler(
    filename = function() {
      "enviroscreenDataDescriptions.csv"
    },
    content = function(file) {
      write.csv(descriptors, file, row.names = FALSE)
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
      dplyr::select(GEOID, `County Name`, indicator1, indicator2,`Coal Community`,`Oil and Gas Community`,`Rural Community`,visParam)
      
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
  
  # remov-e selected features from map 
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

