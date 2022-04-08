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


ui <- fluidPage(
  titlePanel("Colorado Enviroscreen", windowTitle = "Colorado Enviroscreen"),
  
  fluidRow(style={"padding-left:100px;padding-right:100px;"},
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
  
  tabsetPanel(
        tabPanel("Enviroscreen Score",
                 h3("What is the Enviroscreen Score"),
                 fluidRow(
                   column(width = 6, 
                          h2("Data")),
                   column(width = 6, 
                          h2("take"))
                 )
              ),
        tabPanel("Using the Map",
                 h3("How to use the map")
             ),
        tabPanel("Understanding the Data",
                 h3("What to do with the data.")
                 ),
        tabPanel("Addational Ideas",
                 h3("These tabs can continue for other discussion points"),
                 )
  ),
  # Select Reactive Elements ------------------------------------------------
  # content for the reactive elements of the map
  # action button 
  fluidRow(
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
  fluidRow(
    column(
      width = 1, style = {"background-color:#4d3a7d;"}),
    column(
      width = 10,
      style = {"border-style: solid; borderColor=:#4d3a7d;"},
      tags$style(type = "text/css", "#mymap {height: calc(100vh - 80px) !important;}"),
      leafletOutput("mymap")
    ),
    column(
      width = 1, style = "background-color:#4d3a7d;"
    ),
  ),
  
)

server <- function(input, output,session){
  output$mymap <- renderLeaflet({
    map <- leaflet() %>% addTiles()
    map
  })
}

shinyApp(ui, server)