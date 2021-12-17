#' initialMapData
#'
#' @param data1 : sf object with colorado enviroscreen score 
#' @description : generate the spatial data for the intital leaflet map 
#' @return
#' @export
#'
#' @examples
initialMapData <- function(data1){
  d1 <- data1 %>%
    dplyr::filter(area == "County")%>%
    dplyr::select(GEOID, "Colorado Enviroscreen Score_pcntl")%>%
    dplyr::mutate(
    popup = paste0(
          "<br/><h4>Colorado Enviroscreen Score</h4>", # needs to be text
          paste0("<br/><b>Percentile:</b> ", as.character(round(`Colorado Enviroscreen Score_pcntl`), digits =  0)))
          # "<br/><b>Coal Community:</b> ", coalCommunity,
          # "<br/><b>Rural:</b> ", rural,
          # "<br/><b>Oil Community:</b> ", oilCommunity# needs to be value
        )
  return(d1)
}