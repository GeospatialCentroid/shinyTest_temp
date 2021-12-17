library(shiny)
library(leaflet)

data(quakes)
quakes <- quakes[1:10,]
oil <- readRDS("data/scores/oil.rda")


leafIcons_A <- icons(
  iconUrl = "https://leafletjs.com/examples/custom-icons/leaf-green.png",
  iconWidth = 38, iconHeight = 95,
  iconAnchorX = 22, iconAnchorY = 94)
leafIcons_B <- icons(
  iconUrl = "https://leafletjs.com/examples/custom-icons/leaf-red.png",
  iconWidth = 38, iconHeight = 95,
  iconAnchorX = 22, iconAnchorY = 94)

html_legend_A <- "<img src='https://leafletjs.com/examples/custom-icons/leaf-green.png'>green<br/>"
html_legend_B <- "<img src='https://leafletjs.com/examples/custom-icons/leaf-red.png'>red<br/>"

ui <- fluidPage(
  leafletOutput("map")
)
server <- function(input, output, session){
  output$map <- renderLeaflet({
    dataA <- quakes[quakes$mag < 4.6,]
    dataB <- quakes[quakes$mag > 4.6,]
    
    map <- leaflet() %>% addTiles() %>%
      addMarkers(dataA$long, dataA$lat, icon = leafIcons_A, group = "Group A") %>%
      addMarkers(dataB$long, dataB$lat, icon = leafIcons_B, group = "Group B") %>%
      addLayersControl(position = "topleft", overlayGroups = c("Group A","Group B")) %>%
      hideGroup("Group B")%>%
      addPolylines(
        data = oil,
        stroke = TRUE,
        color = "#54A800",
        weight = 1,
        layerId = "Oil and Gas",
        group = "Oil Community"
      ) %>%
      addLayersControl(position = "topleft", overlayGroups = c("Group A","Group B", "Oil Community")) %>%
      hideGroup("Group B")
    map
  })
  observe({
    map <- leafletProxy("map") %>% clearControls()
    if (any(input$map_groups %in% "Group A")) {
      map <- map %>%
        addControl(html = html_legend_A, position = "bottomleft") %>%
        addLegend(title="Group A", position="bottomright", opacity=1, colors="green",labels = "Group A")}
    if (any(input$map_groups %in% "Group B")) {
      map <- map %>%
        addControl(html = html_legend_B, position = "bottomleft") %>%
        addLegend(title="Group B", position="bottomright", opacity=1,colors="red",labels = "Group B")}
  })
}

shinyApp(ui, server)