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
addPolyLine <- function(map, sf1, group, popup){
  ### data : sf file of poly line feature 
  ### group : name of group to use in feature control.
  output <- addPolylines(map,
    data = sf1,
    stroke = TRUE,
    color = "#C6FF0D",
    weight = 2,
    layerId = group,
    options = pathOptions(pane = "binary"),
    popup = popup,
    group = group)
  
  return(output)
}
