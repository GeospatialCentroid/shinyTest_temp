

defineLegend<- function(indicator){
    #establish classes 
    cat1 <- c("Sensitive population","Population under 5","Population over 64"
    ,"Heart disease in adults","Asthma hospitalization rate","Life Expectancy"
    ,"Low weight birth rate")
    
    cat2 <- c("Demographics","Percent people of color","Percent less than high school education"
              ,"Percent low income","Percent linguistic isolation","Percent disability")
    
    # conditional statement to assign label value. 
    t <- ifelse(test = indicator %in% cat1,
           yes = labels <- c("Most Susceptible", "", "", "", "Least Susceptible"),
           no = ifelse(
             test = indicator %in% cat2,
             yes = labels <- c("Most Vulnerable", "", "", "", "Least Vulnerable"),
             no = labels <- c(" Most Burdened", "", "", "", "Least Burdened")
           )
          )
  
    return(labels)
}