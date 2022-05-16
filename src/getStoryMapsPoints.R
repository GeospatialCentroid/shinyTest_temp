###
# Locations for story map elements 
### 

getStoryMaps <- function(){
  # Four Corners:  125 Mike Wash Rd. Towaoc, Colorado, 81334 (UMU tribal headquarters)
  p1 <- c(-108.72899716653774, 37.20196682678452)
  # San Luis Valley:   401 E Church Pl, San Luis, CO 81152 (address for the Sangre de Cristo Acequia Association)
  p2 <- c(-105.42485992382689, 37.199663723970666)
  # Arkansas Valley:  317 Main St.,. Fowler, CO 81039 (address for the Town of Fowler municipal government)
  p3 <- c(-104.02375113856519,38.12911006843698)
  # Commerce City/North Denver:  5801 Brighton Blvd, Commerce City, CO 80022 (address of the Suncor refinery)
  p4 <- c(-104.94911866809777, 39.80253978971713)
  # Greeley:  614 E 20th St, Greeley, CO 80631 (address of Bella Romero K-8 school)
  p5 <- c(-104.67061937301114,40.40607858327397)
  
  
  # generate dataframe 
  df <- data.frame(
    Area = c(
      "Four Corners",
      "San Luis Valley",
      "Arkansas Valley",
      "Commerce City/North Denver",
      "Greeley"),
    Address  = c(
      "125 Mike Wash Rd. Towaoc, Colorado, 81334",
      "401 E Church Pl, San Luis, CO 81152",
      "317 Main St.,. Fowler, CO 81039",
      "5801 Brighton Blvd, Commerce City, CO 80022",
      "614 E 20th St, Greeley, CO 80631"
    ),
    Organization  = c(
      "UMU tribal headquarters",
      "Sangre de Cristo Acequia Association",
      "Town of Fowler municipal government",
      "Suncor refinery",
      "Bella Romero K-8 school"
    ),
    Lon  = c(
      -108.72899716653774,
      -105.42485992382689,
      -104.02375113856519,
      -104.94911866809777,
      -104.67061937301114
    ),
    Lat  = c(
      37.20196682678452,
      37.199663723970666,
      38.12911006843698,
      39.80253978971713,
      40.40607858327397
    ),
    storyMap = c(
      "https://cdphe.colorado.gov/enviroscreen",
      "https://cdphe.colorado.gov/enviroscreen",
      "https://cdphe.colorado.gov/enviroscreen",
      "https://cdphe.colorado.gov/enviroscreen",
      "https://cdphe.colorado.gov/enviroscreen"
    )
  )%>%
    dplyr::mutate(popup = 
      paste0(
        "Learn more about the environmental justice story in the ", `Area`, " region",
        paste0("<a href=",storyMap,"> on this Story Map.</a>"))
    )%>%
    sf::st_as_sf(coords = c("Lon","Lat"),remove = FALSE)%>%
    sf::st_set_crs(value = 4326)
  
  return(df)
}

