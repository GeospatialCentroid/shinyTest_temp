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
getDI <- function(){
  
  diCommunity <- readRDS("data/scores/diCommunities.rds")%>%
    mutate(
      Mn_FLAG = case_when(
        Min_FLAG == 1 ~ "Yes",
        Min_FLAG == 0 ~ "No"
      ),
      FLP_FLA = case_when(
        FLP_FLAG == 1 ~ "Yes",
        FLP_FLAG == 0 ~ "No"
      ),
      Br_FLAG = case_when(
        Burdened_FLAG == 1 ~ "Yes",
        Burdened_FLAG == 0 ~ "No"
      ),
      Sc_FLAG = case_when(
        Score_FLAG == 1 ~ "Yes",
        Score_FLAG == 0 ~ "No"
      )
    )%>%
    mutate(popup =
             paste0(
               "<br/><strong>Disproportionately Impacted Community: </strong>",
               "<br/><b>Census Block Group: </b>", GEOID,
               "<br/>",
               "<br/><b>Over 40% of Households are Low Income: </b>", FLP_FLA,
               "<br/><b>Percent Low Income: </b>", round(Pov_PCT*100, digits = 1),
               "<br/>",
               "<br/><b>Over 40% of Households are People of Color:  </b>", Mn_FLAG,
               "<br/><b>Percent People of Color: </b>", round(Min_PCT*100, digits = 1),
               "<br/>",
               "<br/><b>Over 40% of Households are Housing Burdened: </b>", Br_FLAG,
               "<br/><b>Percent Housing Burdened: </b>", round(HH_Burdened_Pct*100, digits = 1),
               "<br/>",
               "<br/><b>EnviroScreen Score (Percentile) is over 80: </b>", Sc_FLAG,
               "<br/><b>EnviroScreen score (Percentile): </b>", round(EnviroScreen_Pctl, digits = 1),
               "<br/>",
               "<br/>",
               "Read more about Colorado's definition of Disproportionately Impacted Communities in the ",
                tags$a(href = "https://cdphe.colorado.gov/environmental-justice", 
                          "Environmental Justice Act.", target = "_blank")
            )
          )%>%
    mutate(
      color = as.factor(case_when(
        Mn_FLAG == "Yes" & FLP_FLA == "No" & Br_FLAG == "No" & Sc_FLAG == "No" ~ "People of Color",
        Mn_FLAG == "No" & FLP_FLA == "Yes" & Br_FLAG == "No" & Sc_FLAG == "No"~ "Low Income",
        Mn_FLAG == "No" & FLP_FLA == "No" & Br_FLAG == "Yes" & Sc_FLAG == "No"~ "Housing Burden",
        Mn_FLAG == "No" & FLP_FLA == "No" & Br_FLAG == "No" & Sc_FLAG == "Yes"~ "EnviroScreen Score",
        TRUE ~ "More then one category"
      ))
    )%>%
    as('sf')
  return(diCommunity)
}