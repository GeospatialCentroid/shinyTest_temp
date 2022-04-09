#' addPolyLine
#'
#' @param data : sf object representing on of the three community metrics  
#' @param group : unique Id to used for object within the leaflet call. 
#' @param map : leaflet map object
#' 
#' @return : leafleft polyline object 
#' @export
#'
#' @examples
#' 
addPolyLine <- function(map, sf1, group){
  ### data : sf file of poly line feature 
  ### group : name of group to use in feature control.
  output <- addPolylines(map,
    data = sf1,
    stroke = TRUE,
    color = "#F9C1AE", # "#54A800",
    weight = 0.2,
    layerId = group,
    options = pathOptions(pane = "binary"),
    group = group)
  
  return(output)
}
