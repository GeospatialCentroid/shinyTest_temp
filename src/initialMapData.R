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
    dplyr::select(GEOID, "Colorado Enviroscreen Score_pcntl", name, visParam)%>%
    dplyr::mutate(
    popup = paste0(
          "<br/><strong>Colorado Enviroscreen Score</strong>", # needs to be text
          paste0("<br/><strong>",name,"</strong>"),
          paste0("<br/><b>Percentile:</b> ", as.character(round(`Colorado Enviroscreen Score_pcntl`), digits =  0)))
        )
  # error with st_cast, removing from pipe 
  # d1 <- d1 %>% sf::st_make_valid() %>% st_cast("MULTIPOLYGON") 
  d1 <- as(d1, "sf")
  return(d1)
}