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
#'
createMap <- function(mapData,pal, palMap, diPal, oil, rural, coal, di) {

  map <- leaflet() %>%
    # add z levels ------------------------------------------------------------
  addMapPane("index", zIndex = 408) %>%
    addMapPane("binary", zIndex = 409) %>%
    addMapPane("di", zIndex = 410) %>%
    # add tiles ---------------------------------------------------------------
  addProviderTiles("CartoDB.DarkMatter", group = "Dark") %>%
    addProviderTiles("OpenStreetMap", group = "OpenStreetMap")%>%
    addProviderTiles("Stamen.Toner", group = "Light")%>%
    # add search function -----------------------------------------------------
  leaflet.extras::addSearchOSM(
    options = leaflet.extras::searchOptions(autoCollapse = TRUE,
                                            hideMarkerOnCollapse = TRUE))%>%
    # add map reset -----------------------------------------------------------
  leaflet.extras::addResetMapButton() %>%
    # add spatial Data --------------------------------------------------------
  addPolygons(
    data = mapData,
    color = "#454547",
    weight = 1,
    smoothFactor = 0.5,
    opacity = 1.0,
    fillOpacity = 0.5,
    fillColor = ~ palMap(`Colorado EnviroScreen Score Percentile`),
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
  )%>%
    addPolyLine(sf1 = oil, group = "Oil and Gas Community") %>%
    addPolyLine(sf1 = rural, group = "Rural Community") %>%
    addPolyLine(sf1 = coal, group = "Coal Community") %>%
    addPolygons(
      data = di,
      fillColor =  ~diPal(`color`),
      color = "#454547",
      weight = 1,
      fillOpacity = 0.8,
      popup = di$popup,
      group = "Disproportionately Impacted Community"
    )%>%
    # add legend --------------------------------------------------------------
  addLegend(
    "topright",
    colors = pal,
    title = "Est. Values",
    labels = c("Most Burdened", "", "", "", "Least Burdened"),
    opacity = 1,
    layerId = "firstLegend",
    group = "Indicator Score"
    # labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))
  ) %>%
  addLegend("topright",
            colors = c("#fdd18a", "#ade1e9","#51a198","#c095b4"), 
            title = "Disproportionately Impacted Community",
            labels = c("Low Income", "People of Color",
                       "Housing Burden", "More then one category"),
            opacity = 1,
            group = "Disproportionately Impacted Community"
            )%>%
    # add control groups ------------------------------------------------------
  addLayersControl(
    baseGroups = c("Light","Dark", "OpenStreetMap"),
    overlayGroups = c(
      "Indicator Score",
      "Coal Community",
      "Rural Community",
      "Oil and Gas Community",
      "Disproportionately Impacted Community"
    ),
    position = "topleft", 
    options = layersControlOptions(collapsed = TRUE))%>%
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
        "Disproportionately Impacted Community"))

  return(map)
}