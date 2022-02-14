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

# Add categories and specific visualization parameters 
# Will need to add custom legend as well
# low income : rgb(253,209,138)   #fdd18a
# 
# people of color : id="rgb(173,225,233)"   #ade1df
# 
# housing burden : rgb(81,161,152) #51a198
# 
# more then one categoy :rgb(192,149,180) #c095b4
# 
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
               "<br/><strong>Disproportionally Impacted Community: </strong>",
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
          )%>%
    mutate(
      color = as.factor(case_when(
        Mn_FLAG == "Yes" & FLP_FLA == "No" & Br_FLAG == "No" ~ "People of Color",
        Mn_FLAG == "No" & FLP_FLA == "Yes" & Br_FLAG == "No" ~ "Low Income",
        Mn_FLAG == "No" & FLP_FLA == "No" & Br_FLAG == "Yes" ~ "Housing Burden",
        TRUE ~ "More then one category"
      ))
    )%>%
    as('sf')
  return(diCommunity)
}