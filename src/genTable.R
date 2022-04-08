
genTable <- function(tableData, geoid){
  # create a table regardless of the geoid input 
  table1 <- tableData %>% sf::st_drop_geometry()
  ## determine if map click value is reflective of current map data
  if(geoid[1] %in% table1$GEOID){
      #sort table by GEOID 
      table1 <- setorder(x = table1, GEOID, na.last= TRUE)
    
      feature <- grep(pattern = geoid[1], x = table1$GEOID)
      # order based on selected values 
      order2 <- c(feature:nrow(table1), 1:(feature-1))
      
      table1 <- table1[order2, ]
      # # select row of interest 
      # d1 <- table1 %>% dplyr::filter(order2)  
      # # select all other rows 
      # d2 <- table1 %>% dplyr::filter(GEOID != geoid[1])
      # # recombine features 
      # table1 <- bind_rows(d1, d2)
    }
  return(table1)
}