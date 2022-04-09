genPlots <- function(dataframe, parameter, geoid = NULL){
  # font for title  
  fontHeader <- list(
    family = "museo-sans",
    color = "#000000",
    size = 18
    )
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
  if(parameter %in% c("Colorado EnviroScreen Score", "Environmental exposures","Environmental effects","Climate vulnerability")){
    xlabel <- "Burden"
  }
  if(parameter == "Colorado EnviroScreen Score"){
    bg_color <- "#e5ecf6"
  }else{
    bg_color <- "#FFFFFF"
  }
  
  
  minBin <- min(t1$breaks)
  maxBin <- max(t1$breaks)
  
  
  # generate plot regardless of geoid selection 
  p1 <- plot_ly(df1,x=~value, nbinsx = length(unique(df1$bins)))%>%
    add_histogram(
      marker = list(color = "#009add",
                    line = list(width = 2,
                                color = 'rgb(0, 0, 0)')))%>%
    layout(title = list(text=parameter, titlefont  = fontHeader),
           xaxis = list(title = xlabel,
                        ticktext = list("Least", "Most"), 
                        tickvals = list(minBin,maxBin),
                        tickmode = "array",
                        tickangle = 45),
           yaxis = list(title = "Number of Areas"),
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
        layout(title = list(text= parameter,titlefont  = fontHeader),
               xaxis = list(title = xlabel,
                           ticktext = list("Least", "Most"), 
                           tickvals = list(minBin,maxBin),
                           tickmode = "array",
                           tickangle = 45),
               
        yaxis = list(title = "Number of Areas"),
        plot_bgcolor = bg_color, 
        font = fontBody,
        margin = mrg)%>%
        hide_legend()%>%
        style(hoverinfo = 'none') 
    }
  }
  return(p1) 
}
