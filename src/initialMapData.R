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
    dplyr::select(GEOID, "Colorado EnviroScreen Score Percentile", `County Name`, visParam,coal,oilGas,rural)%>%
    dplyr::mutate(
    popup = paste0(
          "<br/><strong>Colorado Enviroscreen Score</strong>", # needs to be text
          paste0("<br/><strong>",`County Name`,"</strong>"),
          paste0("<br/><b>Percentile:</b> ", as.character(round(`Colorado EnviroScreen Score Percentile`), digits =  0)),
          paste0("<br/><b>Coal Community:</b> ", coal),
          paste0("<br/><b>Oil and Gas Community:</b> ", oilGas),
          paste0("<br/><b>Rural Community:</b> ", rural)
        )
    )
  d1 <- as(d1, "sf")
  return(d1)
}