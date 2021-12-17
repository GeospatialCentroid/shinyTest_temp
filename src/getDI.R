#' generate di community 
#' @description : produces a sf object of disporpotionally impacted communities
#'
#' @return : sf object 
#' @export
#'
#' @examples
#' 
#' 
getDI <- function(){
  
  diCommunity <- readRDS("data/scores/diCommunities.rda")%>%
    mutate(
      Mn_FLAG = case_when(
        Mn_FLAG == 1 ~ "Yes",
        Mn_FLAG == 0 ~ "No"
      ),
      FLP_FLA = case_when(
        FLP_FLA == 1 ~ "Yes",
        FLP_FLA == 0 ~ "No"
      ),
      Br_FLAG = case_when(
        Br_FLAG == 1 ~ "Yes",
        Br_FLAG == 0 ~ "No"
      )
    )%>%
    mutate(popup =
             paste0(
               "<br/><h3>Disproportionally Impacted Community: </h3>",
               "<br/><b>Census Block Group: </b>", GEOID,
               "<br/>",
               "<br/><b>40% of Households are Low Income: </b>", FLP_FLA,
               "<br/><b>Percent Low Income: </b>", Pov_PCT,
               "<br/>",
               "<br/><b>40% of Households are Minority : </b>", Mn_FLAG,
               "<br/><b>Percent Minority: </b>", Min_PCT,
               "<br/>",
               "<br/><b>40% of Households are Housing Burdened : </b>", Br_FLAG,
               "<br/><b>Percent Housing Burdened: </b>", HH_Br_P
             )
          )
  return(diCommunity)
}