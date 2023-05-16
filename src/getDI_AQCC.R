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
getDI_AQCC <- function(){

  diCommunity_AQCC <- readRDS("data/scores/diCommunities_AQCC.rds")%>%
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
      Ling_FLAG = case_when(
        LingIso_FLAG == 1 ~ "Yes",
        LingIso_FLAG == 0 ~ "No"
      ),
      Sc_FLAG = case_when(
        Score_FLAG == 1 ~ "Yes",
        Score_FLAG == 0 ~ "No"
      )
    )%>%
    mutate(popup =
             paste0(
               "<br/><strong>AQCC Reg. 3 - Disproportionately Impacted Community </strong>",
               "<br/><b>Census Block Group: </b>", GEOID,
               "<br/>",
               "<br/><b>Socioeconomic Vulnerabilities:</b>",
               "<br/>",
               "<br/><b>Over 40% of the Population is Low Income: </b>", FLP_FLA,
               "<br/><b>Percent Low Income: </b>", round(Pov_PCT*100, digits = 1),
               "<br/>",
               "<br/><b>Over 40% of the Population are People of Color:  </b>", Mn_FLAG,
               "<br/><b>Percent People of Color: </b>", round(Min_PCT*100, digits = 1),
               "<br/>",
               "<br/><b>Over 50% of Households are Housing Burdened: </b>", Br_FLAG,
               "<br/><b>Percent Housing Burdened: </b>", round(HH_Burdened_Pct*100, digits = 1),
               "<br/>",
               "<br/><b>Over 20% of the Population is Linguistically Isolated: </b>", Br_FLAG,
               "<br/><b>Percent Linguistic Isolation: </b>", round(LingIso_PCT*100, digits = 1),
               "<br/>",
               "<br/><b>Cumulative Impacts:</b>",
               "<br/>",
               "<br/><b>EnviroScreen Score (Percentile) is over 80: </b>", Sc_FLAG,
               "<br/><b>EnviroScreen score (Percentile): </b>", round(EnviroScreen_Pctl, digits = 1),
               "<br/>",
               "<br/>",
               "Read more about Colorado's definition of Disproportionately Impacted Communities on the ",
               tags$a(href = "https://cdphe.colorado.gov/environmental-justice", 
                      "CDPHE Environmental Justice Program website.", target = "_blank")
             )
    )%>%
    mutate(
      color = as.factor(case_when(
        Sc_FLAG == "Yes" ~ "Cumulatively Impacted Community",
        TRUE ~ "Socioeconomically Vulnerable Community"
        # Sc_FLAG == "Yes" & DI_communityCount == 1 ~ "Cumulatively Impacted Community",
        # Sc_FLAG == "No" & DI_communityCount > 1 ~ "Socioeconomically Vulnerable Community"
        # TRUE ~ "Both"
        
      ))
    )%>%
    as('sf')
  return(diCommunity_AQCC)
  
}