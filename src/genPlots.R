genPlots <- function(dataframe, parameter, geometry, geoid = NULL){
  # setting text for font elements 
  if(parameter == "EnviroScreen Score"){
    # font for title  
    fontHeader <- list(
      family = "museo-sans",
      color = "#000000",
      size = 18
    )
    bg_color <- "#FFFFFF" # removing the blue background for this 
  }else{
    # font for title  
    fontHeader <- list(
      family = "museo-sans",
      color = "#000000",
      size = 14
      )
    bg_color <- "#FFFFFF"
  }
  
  # font for labels
  fontBody <- list(
    family = "Trebuchet MS")
  # plot Margins 
  mrg <- list(l = 20, r = 20,
              b = 20, t = 30,
              pad = 10)
  

  
  # filter to input parameter 
  df1 <- dataframe %>% 
    dplyr::select("value" = parameter, GEOID) %>%
    as.data.frame()
  
  # condition for y axis label 
  if(geometry == "County"){
    yaxisLabel <- "Number of Counties"
  }
  if(geometry == "Census Tract"){
    yaxisLabel <- "Number of Census Tracts"
  }
  if(geometry == "Census Block Group"){
    yaxisLabel <- "Number of Census Block Groups"
  }
  
  # construct histograms to get bin splits
  t1 <- hist(df1$value)
  # determine bins of histogram feature
  df1$bins <-  findInterval(x = df1$value, vec = t1$breaks)
  # set title for the plot 
  title <- parameter
  xlabel <- "test"
  
  if(parameter %in% c("Sensitive Populations Score")){
    xlabel <- "Susceptibility"
  }
  if(parameter %in% c("Demographics Score")){
    xlabel <- "Vulnerability"
  }
  if(parameter %in% c("EnviroScreen Score", "Environmental Exposures Score",
                      "Environmental Effects Score","Climate Burden Score")){
    xlabel <- "Burden"
  }

  
  minBin <- min(t1$breaks)
  maxBin <- max(t1$breaks)
  
  
  # generate plot regardless of geoid selection 
  p1 <- plot_ly(df1,x=~value, nbinsx = length(unique(df1$bins)))%>%
    add_histogram(
      marker = list(color = "#6d3a5d",
                    line = list(width = 0.5,
                                color = 'rgb(0, 0, 0)')))%>%
    layout(title = list(text=parameter,font  = fontHeader)
           ,xaxis = list(title = xlabel,
                        ticktext = list("Least", "Most"), 
                        tickvals = list(minBin,maxBin),
                        tickmode = "array",
                        tickangle = 45),
           yaxis = list(title = yaxisLabel),
           plot_bgcolor = bg_color,
           font = fontBody,
           margin = mrg)%>%
    hide_legend()%>%
    style(hoverinfo = 'none')
  
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
            bins == binGroup  ~"#bc6123",
            TRUE ~"#6d3a5d"
          )
        )
      # generate plot 
      p1 <- plot_ly(df1,x=~value, nbinsx = length(unique(df1$bins)))%>%
        add_histogram(
          marker = list(color = colors$color,
                        line = list(width = 0.5,
                                    color = 'rgb(0, 0, 0)')))%>%
        layout(title = list(text= parameter,font = fontHeader)
               ,xaxis = list(title = xlabel,
                           ticktext = list("Least", "Most"), 
                           tickvals = list(minBin,maxBin),
                           tickmode = "array",
                           tickangle = 45),
        yaxis = list(title = yaxisLabel),
        plot_bgcolor = bg_color, 
        font = fontBody,
        margin = mrg)%>%
        hide_legend()%>%
        style(hoverinfo = 'none')
    }
  }
  return(p1) 
}
