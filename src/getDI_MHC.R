#' generate di community 
#' @description : produces a sf object of disporpotionally impacted communities
#'
#' @return : sf object 
#' @export
#'
#' @examples
#' 
#' 
#' 
#' 
getDI_MHC <- function(){
  
  diCommunity_MHC <- readRDS("data/scores/diCommunities_MHC.rds")%>%
    mutate(popup =
             paste0(
               "<br/><strong>DI Community: Mobile Home Community </strong>",
               "<br/><b>Name: </b>", Park.Name,
               "<br/>",
               "<br/>",
               "Read more about Colorado's definition of Disproportionately Impacted Communities on the ",
               tags$a(href = "https://cdphe.colorado.gov/environmental-justice", 
                      "CDPHE Environmental Justice Program website.", target = "_blank")
             )
    )%>%
    mutate(
      color = "black"
    )%>%
    as('sf')
  return(diCommunity_MHC)
  
  
}