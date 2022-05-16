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
    dplyr::select(GEOID,"Colorado EnviroScreen Score",  "Colorado EnviroScreen Score Percentile", `County Name`, visParam,`Coal Community`,`Oil and Gas Community`,`Rural Community`)%>%
    dplyr::mutate(
    popup = paste0(
          "<br/><strong>Colorado Enviroscreen Score</strong>", # needs to be text
          paste0("<br/><strong>",`County Name`,"</strong>"),
          paste0("<br/><b>Measured:</b> ", round(`Colorado EnviroScreen Score`, digits = 2),
            "<br/><b>Score:</b> ", as.character(round(`Colorado EnviroScreen Score Percentile`), digits =  0)),
          paste0("<br/><b>Coal Community:</b> ", `Coal Community`),
          paste0("<br/><b>Oil and Gas Community:</b> ", `Oil and Gas Community`),
          paste0("<br/><b>Rural Community:</b> ", `Rural Community`)

        )
    )
  
  d1 <- as(d1, "sf")
  return(d1)
}