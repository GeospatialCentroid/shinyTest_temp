
library(plotly)
library(dplyr)

df <- readRDS("data/scores/allScores.rda")  



# create df of require features  ------------------------------------------
df2 <- df %>%
  sf::st_drop_geometry()%>%
  dplyr::select(
  "GEOID"                                                   
  ,"Colorado Enviroscreen Score"                            
  ,"Pollution and Climate Burden"                           
  ,"Population Burden"                                      
  ,"Environmental exposures"                                
  ,"Environmental effects"                                   
  ,"Climate Vulnerability"                                  
  ,"Sensitive population"                                   
  ,"Demographics",
  "area"
  )
# plot 6 histograms 
## enviroscreen score , "Environmental exposures" ,"Environmental effects"    ,"Climate Vulnerability"    ,"Sensitive population"                                   ,"Demographics"

## filte on geometry 
county <- df2 %>% dplyr::filter(area == "County") 
censusTract <- df2 %>% dplyr::filter(area == "Census Tract") 

genPlots <- function(dataframe, parameter){
  # filter to input parameter 
  df1 <- dataframe %>% 
    dplyr::select("value" = parameter) %>%
    as.data.frame()
  # construct histograms to get bin splits
  t1 <- hist(df1$value)
  
  df1$bins <-  findInterval(x = df1$value, vec = t1$breaks)
  
  colors <- df1 %>% 
    count(bins) %>%
    mutate(
    color = case_when(
      bins == 2  ~"#ef7521",
      TRUE ~"#009add"
    )
  )
  title <- parameter
  
  # generate plot 
  p1 <- plot_ly(df1,x=~value, nbinsx = nrow(colors))%>%
    add_histogram(
      marker = list(color = colors$color,
                    line = list(width = 2,
                                color = 'rgb(0, 0, 0)')))%>%
    layout(xaxis = list(title = title))%>%
    hide_legend()
  return(p1) 
}



plotGroup <- c( "Colorado Enviroscreen Score", "Environmental exposures","Environmental effects",
                "Climate Vulnerability","Sensitive population","Demographics")

plots1 <- list()
for(i in seq_along(plotGroup)){
  plots1[[i]] <- genPlots(dataframe = county,parameter = plotGroup[i])
}
subplot(plots1,nrows = 1,shareY = TRUE, titleX = TRUE)

### enviroscreen score 
df1 <- county %>% dplyr::select("Colorado Enviroscreen Score")
t1 <- hist(df1$`Colorado Enviroscreen Score`)

df1$bins <-  findInterval(x = df1$`Colorado Enviroscreen Score`, vec = t1$breaks)

colors <- df1 %>% count(bins) %>%mutate(
  color = case_when(
    bins == 2  ~"#ef7521",
    TRUE ~"#009add"
  )
)


colors <- df1 %>% 
  dplyr::select(bins)%>%mutate(
  color = case_when(
    bins == 2  ~"#ef7521",
    TRUE ~"#009add"
  )
)
# df1 <- df1 %>% count(bins)


p1 <- plot_ly(df1,x=~`Colorado Enviroscreen Score`)%>%
  add_histogram(
    marker = list(color = colors$color,
                  line = list(width = 5,
                              color = 'rgb(0, 0, 0)')))%>%
  layout(title = 'Enviroscreen Score',
         xaxis = list(title = 'Colorado Enviroscreen Score (cm)'), 
         legend = list(title=list(text='<b> Species of Iris </b>')))%>%
  hide_legend()

p1

genPlots(dataframe = county, parameter = "Colorado Enviroscreen Score")



interval <- (max(county$`Colorado Enviroscreen Score`) - min(county$`Colorado Enviroscreen Score`)) / 10
breaks <- seq(min(county$`Colorado Enviroscreen Score`),max(county$`Colorado Enviroscreen Score`),interval)

county <- county %>% 
  mutate("Colorado Enviroscreen Score_bins" = 
  case_when(
    `Colorado Enviroscreen Score` >= breaks[1] & `Colorado Enviroscreen Score` < breaks[2] ~ 1,
    `Colorado Enviroscreen Score` >= breaks[2] & `Colorado Enviroscreen Score` < breaks[3] ~ 2,
    `Colorado Enviroscreen Score` >= breaks[3] & `Colorado Enviroscreen Score` < breaks[4] ~ 3,
    `Colorado Enviroscreen Score` >= breaks[4] & `Colorado Enviroscreen Score` < breaks[5] ~ 4,
    `Colorado Enviroscreen Score` >= breaks[5] & `Colorado Enviroscreen Score` < breaks[6] ~ 5,
    `Colorado Enviroscreen Score` >= breaks[6] & `Colorado Enviroscreen Score` < breaks[7] ~ 6,
    `Colorado Enviroscreen Score` >= breaks[7] & `Colorado Enviroscreen Score` < breaks[8] ~ 7,
    `Colorado Enviroscreen Score` >= breaks[8] & `Colorado Enviroscreen Score` < breaks[9] ~ 8,
    `Colorado Enviroscreen Score` >= breaks[9] & `Colorado Enviroscreen Score` <= breaks[10] ~ 9,
  )
)
df3 <- county %>%
  dplyr::group_by(`Colorado Enviroscreen Score_bins`)%>%
  dplyr::summarise("count" = n())



### construct enviroscreen score 
hist(county$`Colorado Enviroscreen Score`)
