#' createMap
#' @description : generates the base leaflet map for the applicaiton.
#'
#' @param mapData : SF object containing all data for the initial map.
#' @param pal : palette to be used in the legend
#' @param palMap : the leaflet palette option to but used for the map visualization
#'
#' @return leaftlet map object
#' @export
#'
#' @examples
#'
#'

createMap <- function(mapData, pal, palMap, oil, rural, coal) {
  mymap <- leaflet() %>%
    # add z levels ------------------------------------------------------------
  addMapPane("index", zIndex = 410) %>%
    addMapPane("binary", zIndex = 420) %>%
    addMapPane("di", zIndex = 421) %>%
    # add tiles ---------------------------------------------------------------
  addProviderTiles("CartoDB.DarkMatter", group = "Dark") %>%
    addProviderTiles("OpenStreetMap", group = "OpenStreetMap") %>%
    addProviderTiles("Stamen.Toner", group = "Light")%>%
    # add search function -----------------------------------------------------
  leaflet.extras::addSearchOSM(options = leaflet.extras::searchOptions(autoCollapse = TRUE, hideMarkerOnCollapse = TRUE)) %>%
    # add spatial Data --------------------------------------------------------
  addPolygons(
    data = mapData,
    color = "#454547",
    weight = 1,
    smoothFactor = 0.5,
    opacity = 1.0,
    fillOpacity = 0.5,
    fillColor = ~ palMap(`Colorado Enviroscreen Score_pcntl`),
    # https://stackoverflow.com/questions/48953149/dynamic-color-fill-for-polygon-using-leaflet-in-shiny-not-working
    highlightOptions = highlightOptions(
      color = "white",
      weight = 2,
      bringToFront = TRUE
    ),
    popup = mapData$popup,
    options = pathOptions(pane = "index"),
    layerId = mapData$GEOID,
    group = "Indicator Score"
  ) %>%
    addPolyLine(sf1 = oil, group = "Oil and Gas Community") %>%
    addPolyLine(sf1 = rural, group = "Rural Community") %>%
    addPolyLine(sf1 = coal, group = "Coal Community") %>%
    # add legend --------------------------------------------------------------
  addLegend(
    "topright",
    colors = pal,
    title = "Est. Values",
    labels = c("Most Burdened", "", "", "", "Least Burdened"),
    opacity = 1
    # labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))
  ) %>%
    # add control groups ------------------------------------------------------
  addLayersControl(
    baseGroups = c("Dark","Light", "OpenStreetMap"),
    overlayGroups = c(
      "Indicator Score",
      "Coal Community",
      "Rural Community",
      "Oil and Gas Community"
    ),
    options = layersControlOptions(collapsed = FALSE),
    position = "topleft"
  ) %>%
    htmlwidgets::onRender("
        function() {
            $('.leaflet-control-layers-overlays').prepend('<label style=\"text-align:center\">Map Layers</label>');
            $('.leaflet-control-layers-list').prepend('<label style=\"text-align:center\">Base Maps</label>');
        }
    ")%>%
    # hide layers (off when stating)
    hideGroup(
      group = c(
        "Coal Community",
        "Rural Community",
        "Oil and Gas Community",
        "Disproportionally Impacted Community"
      )
    ) %>%
    # add map reset -----------------------------------------------------------
  addResetMapButton()
  return(mymap)
}