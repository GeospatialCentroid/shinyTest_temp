
genTable <- function(tableData, geoid, colSelected){ 
  # primary table. 
  table1 <- tableData %>% sf::st_drop_geometry()
  # sort table if geoid has been selected 
  if(geoid[1] %in% table1$GEOID){
    #sort table by GEOID 
    table1 <- setorder(x = table1, GEOID, na.last= TRUE)
    
    feature <- grep(pattern = geoid[1], x = table1$GEOID)
    # order based on selected values 
    order2 <- c(feature:nrow(table1), 1:(feature-1))
    
    table1 <- table1[order2, ]
  }
  
  #select columns based on input
  
  if(colSelected == "Group Component Scores"){
    table2 <-  table1 %>% 
      select(
        "GEOID",
        "County Name",
        "EnviroScreen Score Percentile",
        "EnviroScreen Score",
        "Pollution and Climate Burden Percentile",
        "Pollution and Climate Burden Score",
        "Health and Social Factors Percentile",
        "Health and Social Factors Score"
      )
  } else if(colSelected == "Component Score") {
    table2 <-  table1 %>% 
      select(
        "GEOID",
        "County Name",
        "Environmental Exposures Percentile",
        "Environmental Exposures Score",
        "Environmental Effects Percentile",
        "Environmental Effects Score",
        "Climate Burden Percentile",
        "Climate Burden Score",
        "Sensitive Populations Percentile",
        "Sensitive Populations Score",
        "Demographics Percentile",
        "Demographics Score"
      )
  } else if(colSelected == "Environmental Exposures") {
    table2 <-  table1 %>% 
      select(
        "GEOID",
        "County Name",
        "Air toxics emissions Percentile",
        "Air toxics emissions",
        "Diesel particulate matter (PM) Percentile", 
        "Diesel particulate matter (PM)",
        "Drinking water regulations Percentile",
        "Drinking water regulations",
        "Lead exposure risk Percentile",
        "Lead exposure risk",
        "Noise Percentile",
        "Noise",
        "Other air pollutants Percentile",
        "Other air pollutants",
        "Ozone Percentile",
        "Ozone",
        "Fine particle pollution Percentile",
        "Fine particle pollution" ,  
        "Traffic proximity & volume Percentile",
        "Traffic proximity & volume"
      )
  } else if(colSelected == "Environmental Effects") {
    table2 <- table1 %>% 
      select(
        "GEOID",
        "County Name",
        "Impaired streams and rivers Percentile",
        "Impaired streams and rivers",
        "Proximity to hazardous waste facilities Percentile",
        "Proximity to hazardous waste facilities",
        "Proximity to mining locations Percentile",
        "Proximity to mining locations",
        "Proximity to National Priorities List sites Percentile",
        "Proximity to National Priorities List sites",
        "Proximity to oil and gas Percentile",
        "Proximity to oil and gas",
        "Proximity to Risk Management Plan sites Percentile", 
        "Proximity to Risk Management Plan sites",
        "Wastewater discharge indicator Percentile",
        "Wastewater discharge indicator"
      )
  }  else if(colSelected == "Climate Vulnerability") {
    table2 <- table1 %>% 
      select(
        "GEOID",
        "County Name",
        "Drought Percentile",
        "Drought",
        "Extreme heat days Percentile",
        "Extreme heat days",
        "Floodplains Percentile",
        "Floodplains",
        "Wildfire risk Percentile",
        "Wildfire risk"
      )
  } else if(colSelected == "Sensitive Population") {
    table2 <- table1 %>% 
      select(
        "GEOID",
        "County Name",
        "Asthma hospitalization rate Percentile",
        "Asthma hospitalization rate",
        "Cancer prevalence Percentile",
        "Cancer prevalence",
        "Diabetes prevalence Percentile",
        "Diabetes prevalence",
        "Heart disease in adults Percentile",
        "Heart disease in adults",
        "Life expectancy Percentile", 
        "Life expectancy", 
        "Low birth weight Percentile",
        "Low birth weight",
        "Mental health indicator Percentile",
        "Mental health indicator",
        "Population over 64 Percentile",
        "Population over 64",
        "Population under 5 Percentile", 
        "Population under 5"
      )
  } else if(colSelected == "Demographics") {
    table2 <- table1 %>%  
      select(
        "GEOID",
        "County Name",
        "Housing cost burdened Percentile",
        "Housing cost burdened",
        "Percent disability Percentile",
        "Percent disability",
        "Percent less than high school education Percentile", 
        "Percent less than high school education",
        "Percent linguistic isolation Percentile",
        "Percent linguistic isolation",
        "Percent low income Percentile", 
        "Percent low income", 
        "Percent people of color Percentile",
        "Percent people of color"
      )
  }  else if(colSelected == "Community Classifications") {
    table2 <- table1 %>%  
      select(
        "GEOID",
        "County Name",
        "Disproportionately Impacted Community",
        "Justice40","Coal Community",
        "Oil and Gas Community","Urban/Rural", 
        "Total Population"
      )
  } 
  
  
  return(table2)
}


