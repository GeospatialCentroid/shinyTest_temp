
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
        "Colorado EnviroScreen Score",
        "Colorado EnviroScreen Score Percentile",
        "Pollution & Climate Burden",
        "Pollution & Climate Burden Percentile",
        "Socioeconomics & Demographics",
        "Socioeconomics & Demographics Percentile"
      )
  } else if(colSelected == "Component Score") {
   table2 <-  table1 %>% 
      select(
        "GEOID",
        "County Name",
        "Environmental exposures",
        "Environmental exposures Percentile",
        "Environmental effects",
        "Environmental effects Percentile",
        "Climate vulnerability",
        "Climate vulnerability Percentile",
        "Sensitive population",
        "Sensitive population Percentile",
        "Demographics",
        "Demographics Percentile"
      )
  } else if(colSelected == "Environmental Exposures") {
    table2 <-  table1 %>% 
      select(
        "GEOID",
        "County Name",
        "Air toxics emissions",
        "Air toxics emissions Percentile",
        "Diesel PM",
        "Diesel PM Percentile", 
        "Drinking Water Violations",
        "Drinking Water Violations Percentile",
        "Lead exposure risk",
        "Lead exposure risk Percentile",
        "Noise",
        "Noise Percentile",
        "Other Air Pollutants",
        "Other Air Pollutants Percentile",
        "Ozone",
        "Ozone Percentile",
        "Particles" ,  
        "Particles Percentile",
        "Traffic proximity & volume",
        "Traffic proximity & volume Percentile"
      )
  } else if(colSelected == "Environmental Effects") {
    table2 <- table1 %>% 
      select(
        "GEOID",
        "County Name",
        "Impaired Surface Water",
        "Impaired Surface Water Percentile",
        "Proximity to hazardous waste facilities",
        "Proximity to hazardous waste facilities Percentile",
        "Proximiy to Mining and Smelting",
        "Proximiy to Mining and Smelting Percentile",
        "Proximity to National Priorities List (NPL) sites",
        "Proximity to National Priorities List (NPL) sites Percentile",
        "Proximiy to Oil and Gas",
        "Proximiy to Oil and Gas Percentile",
        "Proximity to RMP sites",
        "Proximity to RMP sites Percentile", 
        "Wastewater discharge indicator",
        "Wastewater discharge indicator Percentile"
      )
  }  else if(colSelected == "Climate Vulnerability") {
    table2 <- table1 %>% 
      select(
        "GEOID",
        "County Name",
        "Drought",
        "Drought Percentile",
        "Extreme Heat Days",
        "Extreme Heat Days Percentile",
        "Floodplains",
        "Floodplains Percentile",
        "Wildfire risk",
        "Wildfire risk Percentile"
      )
  } else if(colSelected == "Sensitive Population") {
    table2 <- table1 %>% 
      select(
        "GEOID",
        "County Name",
        "Asthma hospitalization rate",
        "Asthma hospitalization rate Percentile",
        "Cancer Incidence",
        "Cancer Incidence Percentile",
        "Diabetes Incidence",
        "Diabetes Incidence Percentile",
        "Heart disease in adults",
        "Heart disease in adults Percentile",
        "Life expectancy", 
        "Life expectancy Percentile", 
        "Low weight birth rate",
        "Low weight birth rate Percentile",
        "Mental Health Incidence",
        "Mental Health Incidence Percentile",
        "Population over 64",
        "Population over 64 Percentile",
        "Population under 5",
        "Population under 5 Percentile" 
      )
  } else if(colSelected == "Demographics") {
    table2 <- table1 %>%  
      select(
        "GEOID",
        "County Name",
        "Housing Cost Burdened",
        "Housing Cost Burdened Percentile",
        "Percent disability",
        "Percent disability Percentile",
        "Percent less than high school education",
        "Percent less than high school education Percentile", 
        "Percent linguistic isolation",
        "Percent linguistic isolation Percentile",
        "Percent low income", 
        "Percent low income Percentile", 
        "Percent people of color",
        "Percent people of color Percentile"
      )
  }  else if(colSelected == "Extra Evaluations") {
    table2 <- table1 %>%  
      select(
        "GEOID",
        "County Name",
        "Disproportionately Impacted Community",
        "Justice 40 Community","Coal Community",
        "Oil and Gas Community","Rural Community"
      )
  } 
  

  return(table2)
}