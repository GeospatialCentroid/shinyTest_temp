genPlots <- function(dataframe, parameter, geoid = NULL){
  # filter to input parameter 
  df1 <- dataframe %>% 
    dplyr::select("value" = parameter, GEOID) %>%
    as.data.frame()
  # construct histograms to get bin splits
  t1 <- hist(df1$value)
  # determine bins of histogram feature
  df1$bins <-  findInterval(x = df1$value, vec = t1$breaks)
  # set title for the plot 
  title <- parameter
  
  if(parameter %in% c("Sensitive population")){
    xlabel <- "Susceptibility"
  }
  if(parameter %in% c("Demographics")){
    xlabel <- "Vulnerability"
  }
  if(parameter %in% c("Colorado Enviroscreen Score" ,"Environmental exposures","Environmental effects","Climate Vulnerability")){
    xlabel <- "Burden"
  }
  
  minBin <- min(t1$breaks)
  maxBin <- max(t1$breaks)
  
  # generate plot regardless of geoid selection 
  p1 <- plot_ly(df1,x=~value, nbinsx = length(unique(df1$bins)))%>%
    add_histogram(
      marker = list(color = "#009add",
                    line = list(width = 2,
                                color = 'rgb(0, 0, 0)')))%>%
    layout(xaxis = list(title = xlabel,
               ticktext = list("Least", "Most"), 
               tickvals = list(minBin,maxBin),
               tickmode = "array",
               tickangle = 45
             ), 
           yaxis = list(title = "Number of Areas"))%>%
    hide_legend()
  
  # if geoid is in feature, reassign p1 and produce altered plot 
  ## determine if the map has been clicked 
  if(!is.null(geoid$id)){
    ## determine if map click value is reflective of current map data
    if(geoid[1] %in% df1$GEOID){
      binGroup <- df1[df1$GEOID == geoid[1], "bins"]
      # set color 
      colors <- df1 %>% 
        dplyr::count(bins) %>%
        dplyr::mutate(
          color = case_when(
            bins == binGroup  ~"#ef7521",
            TRUE ~"#009add"
          )
        )
      # generate plot 
      p1 <- plot_ly(df1,x=~value, nbinsx = length(unique(df1$bins)))%>%
        add_histogram(
          marker = list(color = colors$color,
                        line = list(width = 2,
                                    color = 'rgb(0, 0, 0)')))%>%
        layout(xaxis = list(title = xlabel,
                           ticktext = list("Least", "Most"), 
                           tickvals = list(minBin,maxBin),
                           tickmode = "array",
                           tickangle = 45
        ),
        yaxis = list(title = "Number of Areas"))%>%
        hide_legend() 
    }
  }
  return(p1) 
}
