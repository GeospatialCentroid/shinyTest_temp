
genTable <- function(tableData, geoid){ 
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
  
  gcs <- table1 %>% dplyr::select("GEOID","County Name","Colorado EnviroScreen Score"  ,"Colorado EnviroScreen Score Percentile"
                                  ,"Pollution & Climate Burden","Pollution & Climate Burden Percentile","Socioeconomics & Demographics"
                                  ,"Socioeconomics & Demographics Percentile") 
  cs1 <- table1 %>% dplyr::select("GEOID","County Name","Colorado EnviroScreen Score"  ,"Colorado EnviroScreen Score Percentile"
                                  ,"Pollution & Climate Burden","Pollution & Climate Burden Percentile","Socioeconomics & Demographics"
                                  ,"Socioeconomics & Demographics Percentile") 
  ee1 <- table1 %>% dplyr::select("GEOID","County Name","Colorado EnviroScreen Score"  ,"Colorado EnviroScreen Score Percentile"
  ,"Pollution & Climate Burden","Pollution & Climate Burden Percentile","Socioeconomics & Demographics"
  ,"Socioeconomics & Demographics Percentile") 
  ee2 <- table1 %>% dplyr::select("GEOID","County Name","Colorado EnviroScreen Score"  ,"Colorado EnviroScreen Score Percentile"
                                  ,"Pollution & Climate Burden","Pollution & Climate Burden Percentile","Socioeconomics & Demographics"
                                  ,"Socioeconomics & Demographics Percentile") 
  clm <- table1 %>% dplyr::select("GEOID","County Name","Colorado EnviroScreen Score"  ,"Colorado EnviroScreen Score Percentile"
                                  ,"Pollution & Climate Burden","Pollution & Climate Burden Percentile","Socioeconomics & Demographics"
                                  ,"Socioeconomics & Demographics Percentile") 
  sen <- table1 %>% dplyr::select("GEOID","County Name","Colorado EnviroScreen Score"  ,"Colorado EnviroScreen Score Percentile"
                                  ,"Pollution & Climate Burden","Pollution & Climate Burden Percentile","Socioeconomics & Demographics"
                                  ,"Socioeconomics & Demographics Percentile") 
  dem <- table1 %>% dplyr::select("GEOID","County Name","Colorado EnviroScreen Score"  ,"Colorado EnviroScreen Score Percentile"
                                  ,"Pollution & Climate Burden","Pollution & Climate Burden Percentile","Socioeconomics & Demographics"
                                  ,"Socioeconomics & Demographics Percentile") 
  if(geoid[1] %in% table1$GEOID){
    formatDT <- function(dataframe){
      dataframe %>% datatable() %>% formatStyle(
        "GEOID", target = "row", 
        backgroundColor = styleRow(c(1), c("yellow")))
    } 
    
    gcs <- gcs  %>% formatDT() 
    cs1 <- cs1 %>% formatDT() 
    ee1 <- ee1  %>% formatDT() 
    ee2 <- ee2  %>% formatDT() 
    clm <- clm  %>% formatDT() 
    sen <- sen  %>% formatDT() 
    dem <- dem  %>% formatDT() 
  }
  table2 <- list(gcs, cs1, ee1,ee2,clm,sen,dem )
  return(table2)
}