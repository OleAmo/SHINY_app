
library(shiny)
library(sf)
library(dplyr)
library(readr)

comarques <- st_read("data/raw/COMARQUES_CENTROIDES.shp")


create_coordenates <- function(data){
  
  data_processed <- data %>%
    mutate(
      coords = st_centroid(geometry) %>%
        st_transform(4326) %>% 
        st_coordinates() ,
      lat = coords[,1],
      long = coords[,2]
    )
  
  long <- data_processed$long
  lat <- data_processed$lat
  
  return(data_processed)
  
}



create_coordenates (comarques)