# geoid
createHistogram <- function(data){
  
  # prep this dataset in the loading of the map 
  df <- data 
  # plot 6 histograms 
  ## enviroscreen score , "Environmental exposures" ,"Environmental effects"    ,"Climate Vulnerability"    ,"Sensitive population"                                   ,"Demographics"

  

  # enviroscreen score ------------------------------------------------------
  
  ## filte on geometry {{reactive}}
  df1  <- df %>%
    dplyr::select("GEOID","Colorado Enviroscreen Score")
  
  
  ## generate histogram to pull bins 
  t1 <- hist(df1$`Colorado Enviroscreen Score`)
  df1$bins <-  findInterval(x = df1$`Colorado Enviroscreen Score`, vec = t1$breaks)
  # generate color ramp 
  # colors <- df1 %>% count(bins) %>%mutate(
  #   color = case_when(
  #     bins == geoid  ~"#ef7521",
  #     TRUE ~"#009add"
  #   )
  # )
  ## create plot
  p1 <- plot_ly(df1,x=~`Colorado Enviroscreen Score`)%>%
    add_histogram()%>%
    layout(title = 'Enviroscreen Score',
           xaxis = list(title = 'Colorado Enviroscreen Score (cm)')
           )
  
  return(p1)
  
}
