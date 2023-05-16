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
createMap <- function(mapData,pal, palMap, di, diPal, #oil, rural, coal, 
                      di_2023, diPal_2023, di_AQCC, diPal_AQCC, 
                      di_MHC, justice40, storyMaps) {
  #story map icon 
  sm_Icon <- makeIcon("www/StoryMaps.png",
                      iconWidth = 40,
                      iconHeight = 40)
  
  map <- leaflet(options = leafletOptions(minZoom = 6)) %>%
    setView( lng = -105.76356278240084
             , lat = 39.13085942963124
             , zoom = 7 )%>%
    # add z levels ------------------------------------------------------------
  addMapPane("index", zIndex = 408) %>%
    addMapPane("binary", zIndex = 409) %>% # for the addPolyLine objects
    addMapPane("elements", zIndex = 410) %>%
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
    color = "#F9C1AE", #"#454547",
    weight = 0.2,
    smoothFactor = 0.5,
    opacity = 1.0,
    fillOpacity = 0.5,
    fillColor = ~ palMap(`EnviroScreen Score Percentile`),
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
    # addPolyLine(sf1 = oil, group = "Oil and Gas Community", 
    #             popup = "<strong>Definition: </strong> Counties that have active oil and gas operations.")%>%
    # addPolyLine(sf1 = rural, group = "Rural", 
    #             popup = "<strong>Definition: </strong> Counties that do not contain a U.S. Census Bureau's urban area") %>%
    # addPolyLine(sf1 = coal, group = "Coal Community", 
    #             popup = "<strong>Definition: </strong> Counties that have a coal-burning power plant.") %>%
  
    addPolygons(
      data = di_2023,
      fillColor =  ~diPal_2023(`color`),
      color = "#454547",
      weight = 1,
      fillOpacity = 0.8,
      popup = di_2023$popup,
      group = "Disproportionately Impacted Community (May 2023)",
      options = pathOptions(pane = "elements")
    )%>%
    addPolygons(
      data = di_AQCC,
      fillColor =  ~diPal_AQCC(`color`),
      color = "#454547",
      weight = 1,
      fillOpacity = 0.8,
      popup = di_AQCC$popup,
      group = "AQCC Reg. 3 - Disproportionately Impacted Community",
      options = pathOptions(pane = "elements")
    )%>%    
    leaflet::addCircleMarkers(
      data = di_MHC,
      label = ~Park.Name,
      popup = ~popup,
      fillColor = "goldenrod",
      fillOpacity = 1,
      radius = 4,
      stroke = F,
      group = "DI Community: Mobile Home Communities",
      options = pathOptions(pane = "elements")
    )%>%  
    addPolygons(
      data = di,
      fillColor =  ~diPal(`color`),
      color = "#454547",
      weight = 1,
      fillOpacity = 0.8,
      popup = di$popup,
      group = "Prior Disproportionately Impacted Community (January 2023-May 2023)",
      options = pathOptions(pane = "elements")
    )%>%  
    addPolygons(
      data = justice40,
      popup = justice40$popup,
      fillColor  = "#fb9a99",
      fillOpacity = 0.8,
      color = "#636363",
      weight = 1,
      group = "Federal CEJST (Justice40)",
      options = pathOptions(pane = "elements")
    )%>%
    addMarkers(
      data = storyMaps,
      label = ~Area,
      popup = ~popup,
      # fillColor = "goldenrod",
      # fillOpacity = 1,
      # stroke = F,
      group = "Story Maps",
      options = pathOptions(pane = "elements"),
      icon = sm_Icon
    )%>%
    # add legend --------------------------------------------------------------
  addLegend(
    "topright",
    colors = pal,
    title = "Est. Values",
    labels = c("Most Burdened", "", "", "", "Least Burdened"),
    opacity = .5,
    layerId = "firstLegend",
    group = "Indicator Score",
    na.label = "No Data"
    
    # labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))
  ) %>%
    addLegend("topright",
              colors = c('#332288', '#88CCEE', '#44AA99', '#117733', '#999933', '#DDCC77', '#CC6677', '#882255'), 
              title = "Disproportionately Impacted Community (May 2023)",
              labels = c("Low Income", "People of Color",
                         "Housing Burden", "Linguistically Isolated", 
                         "Federal CEJST (Justice40)", "Tribal Lands", 
                         "EnviroScreen Score", "More than one category"),
              opacity = .8,
              group = "Disproportionately Impacted Community (May 2023)"
    )%>%
    addLegend("topright",
              colors = "goldenrod",
              title = "DI Community: Mobile Home Communities",
              label = "Mobile Home Community",
              opacity = 1,
              group = "DI Community: Mobile Home Communities"
    )%>%
    addLegend("topright",
              colors = c("#d95f02", "#1b9e77"), 
              title = "AQCC Reg. 3 - Disproportionately Impacted Community",
              labels = c("Socioeconomically Vulnerable Community", "Cumulatively Impacted Community"),
              opacity = .8,
              group = "AQCC Reg. 3 - Disproportionately Impacted Community"
    )%>%
    addLegend("topright",
              colors = c("#a6cee3", "#33a02c","#b2df8a", "#fc8d62", "#1f78b4"),
              title = "Prior Disproportionately Impacted Community (January 2023-May 2023)",
              labels = c("Low Income", "People of Color", "Housing Burden", "EnviroScreen Score", "More than one category"),
              opacity = .8,
              group = "Prior Disproportionately Impacted Community (January 2023-May 2023)"
    )%>%
    # addLegendImage(images = "www/oilGas.png",
    #                labels = "Oil and Gas Community",
    #                width = 25,
    #                height = 25,
    #                position = 'topright',
    #                group = "Oil and Gas Community",
    #                labelStyle = "font-size: 16")%>%
    # addLegendImage(images = "www/rural.png",
    #                labels = "Rural",
    #                width = 25,
    #                height = 25,
    #                position = 'topright',
    #                group = "Rural",
    #                labelStyle = "font-size: 16")%>%
    # addLegendImage(images = "www/coal.png",
    #                labels = "Coal Community",
    #                width = 25,
    #                height = 25,
    #                position = 'topright',
    #                group = "Coal Community",
    #                labelStyle = "font-size: 16")%>%
    addLegend("topright",
              colors = "#fb9a99", 
              labels =  "Federal CEJST (Justice40)",
              opacity = .8,
              group = "Federal CEJST (Justice40)"
    )%>%
    # add control groups ------------------------------------------------------
  addLayersControl(
    baseGroups = c("Light","Dark", "OpenStreetMap"),
    overlayGroups = c(
      "Indicator Score",
      # "Coal Community",
      # "Rural",
      # "Oil and Gas Community",
      "Disproportionately Impacted Community (May 2023)",
      "DI Community: Mobile Home Communities",
      "AQCC Reg. 3 - Disproportionately Impacted Community",
      "Federal CEJST (Justice40)",
      "Story Maps",
      "Prior Disproportionately Impacted Community (January 2023-May 2023)"
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
        # "Coal Community",
        # "Rural",
        # "Oil and Gas Community",
        "Disproportionately Impacted Community (May 2023)",
        "DI Community: Mobile Home Communities",
        "AQCC Reg. 3 - Disproportionately Impacted Community",
        "Prior Disproportionately Impacted Community (January 2023-May 2023)",
        "Federal CEJST (Justice40)",
        "Story Maps"))
  
  return(map)
}