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
getDI_2023 <- function(){
  
  diCommunity_2023 <- readRDS("data/scores/diCommunities_2023.rds")%>%
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
      J40_FLAG = case_when(
        Justice40_FLAG == 1 ~ "Yes",
        Justice40_FLAG == 0 ~ "No"
      ),
      Trib_FLAG = case_when(
        TribalLands_FLAG == 1 ~ "Yes",
        TribalLands_FLAG == 0 ~ "No"
      ),
      Sc_FLAG = case_when(
        Score_FLAG == 1 ~ "Yes",
        Score_FLAG == 0 ~ "No"
      )
    )%>%
    mutate(popup =
             paste0(
               "<br/><strong>Current Disproportionately Impacted Community (May 2023): </strong>",
               "<br/><b>Census Block Group: </b>", GEOID,
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
               "<br/><b>Over 20% of the Population is Linguistically Isolated: </b>", Ling_FLAG,
               "<br/><b>Percent Linguistic Isolation: </b>", round(LingIso_PCT*100, digits = 1),
               "<br/>",
               "<br/><b>Area under Tribal Jurisdiction: </b>", Trib_FLAG,
               "<br/>",
               "<br/><b>Area qualifies as Disadvantaged in the federal Climate and Economic Justice Screening Tool: </b>", J40_FLAG,
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
        Mn_FLAG == "Yes" & DI_communityCount == 1 ~ "People of Color",
        FLP_FLA == "Yes" & DI_communityCount == 1 ~ "Low Income",
        Br_FLAG == "Yes" & DI_communityCount == 1 ~ "Housing Burden",
        Ling_FLAG == "Yes" & DI_communityCount == 1 ~ "Linguistically isolated",
        J40_FLAG == "Yes" & DI_communityCount == 1 ~ "Federally identified (CEJST)",
        Trib_FLAG == "Yes" & DI_communityCount == 1 ~ "Tribal Lands",
        Sc_FLAG == "Yes" & DI_communityCount == 1 ~ "EnviroScreen Score",
        TRUE ~ "More than one category"
      ))
    )%>%
    as('sf')
  return(diCommunity_2023)
  
  
}