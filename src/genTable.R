
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
        "Colorado EnviroScreen Score Percentile",
        "Colorado EnviroScreen Score",
        "Pollution & Climate Burden Percentile",
        "Pollution & Climate Burden",
        "Socioeconomics & Demographics Percentile",
        "Socioeconomics & Demographics"
      )
  } else if(colSelected == "Component Score") {
    table2 <-  table1 %>% 
      select(
        "GEOID",
        "County Name",
        "Environmental exposures Percentile",
        "Environmental exposures",
        "Environmental effects Percentile",
        "Environmental effects",
        "Climate vulnerability Percentile",
        "Climate vulnerability",
        "Sensitive population Percentile",
        "Sensitive population",
        "Demographics Percentile",
        "Demographics"
      )
  } else if(colSelected == "Environmental Exposures") {
    table2 <-  table1 %>% 
      select(
        "GEOID",
        "County Name",
        "Air toxics emissions Percentile",
        "Air toxics emissions",
        "Diesel PM Percentile", 
        "Diesel PM",
        "Drinking Water Violations Percentile",
        "Drinking Water Violations",
        "Lead exposure risk Percentile",
        "Lead exposure risk",
        "Noise Percentile",
        "Noise",
        "Other Air Pollutants Percentile",
        "Other Air Pollutants",
        "Ozone Percentile",
        "Ozone",
        "Particles Percentile",
        "Particles" ,  
        "Traffic proximity & volume Percentile",
        "Traffic proximity & volume"
      )
  } else if(colSelected == "Environmental Effects") {
    table2 <- table1 %>% 
      select(
        "GEOID",
        "County Name",
        "Impaired Surface Water Percentile",
        "Impaired Surface Water",
        "Proximity to hazardous waste facilities Percentile",
        "Proximity to hazardous waste facilities",
        "Proximiy to Mining and Smelting Percentile",
        "Proximiy to Mining and Smelting",
        "Proximity to National Priorities List (NPL) sites Percentile",
        "Proximity to National Priorities List (NPL) sites",
        "Proximiy to Oil and Gas Percentile",
        "Proximiy to Oil and Gas",
        "Proximity to RMP sites Percentile", 
        "Proximity to RMP sites",
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
        "Extreme Heat Days Percentile",
        "Extreme Heat Days",
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
        "Cancer Incidence Percentile",
        "Cancer Incidence",
        "Diabetes Incidence Percentile",
        "Diabetes Incidence",
        "Heart disease in adults Percentile",
        "Heart disease in adults",
        "Life expectancy Percentile", 
        "Life expectancy", 
        "Low weight birth rate Percentile",
        "Low weight birth rate",
        "Mental Health Incidence Percentile",
        "Mental Health Incidence",
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
        "Housing Cost Burdened Percentile",
        "Housing Cost Burdened",
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
  }  else if(colSelected == "Other Map Layers") {
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





# old version 
# genTable <- function(tableData, geoid){ 
#   # primary table. 
#   table1 <- tableData %>% sf::st_drop_geometry()
#   # sort table if geoid has been selected 
#   if(geoid[1] %in% table1$GEOID){
#     #sort table by GEOID 
#     table1 <- setorder(x = table1, GEOID, na.last= TRUE)
#     
#     feature <- grep(pattern = geoid[1], x = table1$GEOID)
#     # order based on selected values 
#     order2 <- c(feature:nrow(table1), 1:(feature-1))
#     
#     table1 <- table1[order2, ]
#   }
#   
#   gcs <- table1 %>% dplyr::select("GEOID","County Name"
#                                   ,"Colorado EnviroScreen Score Percentile", "Colorado EnviroScreen Score"  
#                                   ,"Pollution & Climate Burden Percentile","Pollution & Climate Burden"
#                                   ,"Socioeconomics & Demographics Percentile","Socioeconomics & Demographics"
#                                   ) 
#   cs1 <- table1 %>% dplyr::select("GEOID","County Name"
#                                   ,"Environmental exposures Percentile","Environmental exposures"
#                                   ,"Environmental effects Percentile","Environmental effects"                                       
#                                   , "Climate vulnerability Percentile","Climate vulnerability"
#                                   , "Sensitive population Percentile","Sensitive population"                                        
#                                   ,"Demographics Percentile","Demographics") 
#   ee1 <- table1 %>% dplyr::select("GEOID","County Name"
#                                   ,"Ozone Percentile","Ozone"
#                                   ,"Particles Percentile","Particles"
#                                   ,"Lead exposure risk Percentile","Lead exposure risk"
#                                   ,"Diesel PM Percentile","Diesel PM","Diesel PM Percentile"
#                                   ,"Traffic proximity & volume Percentile","Traffic proximity & volume"
#                                   ,"Air toxics emissions Percentile","Air toxics emissions"
#                                   ,"Other Air Pollutants Percentile","Other Air Pollutants"
#                                   ,"Drinking Water Violations Percentile","Drinking Water Violations"
#                                   ,"Noise Percentile","Noise") 
#   ee2 <- table1 %>% dplyr::select("GEOID","County Name"
#                                   ,"Wastewater discharge indicator Percentile","Wastewater discharge indicator"
#                                   ,"Proximity to National Priorities List (NPL) sites Percentile","Proximity to National Priorities List (NPL) sites"
#                                   ,"Proximity to RMP sites Percentile","Proximity to RMP sites"
#                                   ,"Proximity to hazardous waste facilities Percentile","Proximity to hazardous waste facilities"
#                                   ,"Proximiy to Oil and Gas Percentile","Proximiy to Oil and Gas"
#                                   ,"Proximiy to Mining and Smelting Percentile","Proximiy to Mining and Smelting"
#                                   ,"Impaired Surface Water Percentile","Impaired Surface Water"
#                                   ) 
#   clm <- table1 %>% dplyr::select("GEOID","County Name"
#                                   ,"Wildfire risk Percentile","Wildfire risk"                                                                                    
#                                   ,"Floodplains Percentile","Floodplains"                                                                                        
#                                   ,"Drought Percentile","Drought"                                                                                                
#                                   ,"Extreme Heat Days Percentile","Extreme Heat Days"                                           
#                                   ) 
#   sen <- table1 %>% dplyr::select("GEOID","County Name"
#                                   ,"Population under 5 Percentile","Population under 5"                                                                          
#                                   ,"Population over 64 Percentile","Population over 64"                                                                          
#                                   ,"Heart disease in adults Percentile","Heart disease in adults"                                                                
#                                   ,"Asthma hospitalization rate Percentile","Asthma hospitalization rate"                                                        
#                                   ,"Life expectancy Percentile","Life expectancy"                                                                                
#                                   ,"Low weight birth rate Percentile","Low weight birth rate"                                                                    
#                                   ,"Cancer Incidence Percentile","Cancer Incidence"                                                                              
#                                   ,"Diabetes Incidence Percentile","Diabetes Incidence"                                                                          
#                                   ,"Mental Health Incidence Percentile","Mental Health Incidence"
#                                   )                                       
#   dem <- table1 %>% dplyr::select("GEOID","County Name"
#                                   ,"Percent people of color Percentile","Percent people of color"                                                                
#                                   ,"Percent less than high school education Percentile","Percent less than high school education"                      
#                                   ,"Percent low income Percentile" ,"Percent low income"                                                                         
#                                   ,"Percent linguistic isolation Percentile","Percent linguistic isolation"                                                      
#                                   ,"Percent disability Percentile","Percent disability"                                                                          
#                                   ,"Housing Cost Burdened Percentile","Housing Cost Burdened"
#                                   ) 
#   ext <- table1 %>% dplyr::select("GEOID","County Name","Disproportionately Impacted Community","Justice 40 Community","Coal Community",
#                                   "Oil and Gas Community","Rural Community") 
# 
#   # if(geoid[1] %in% table1$GEOID){
#   # 
#   #   gcs <- gcs # %>% formatDT() 
#   #   cs1 <- cs1 # %>% formatDT() 
#   #   ee1 <- ee1 # %>% formatDT() 
#   #   ee2 <- ee2 # %>% formatDT() 
#   #   clm <- clm # %>% formatDT() 
#   #   sen <- sen # %>% formatDT() 
#   #   dem <- dem # %>% formatDT() 
#   #   ext <- ext # %>% formatDT() 
#   #   
#   # }
#   table2 <- list(gcs, cs1, ee1,ee2,clm,sen,dem,ext )
#   return(table2)
# }