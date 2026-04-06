
library(shiny)
library(sf)
library(dplyr)
library(readr)

# --- SHAPE ----
# --------------

#   -) Tenia un SHAPE de comarques de un altre projecte
#   -) Però no té la info de LATITUT i LONG 



comarques <- st_read("data/raw/COMARQUES_CENTROIDES.shp")


# --- FUNCIÓ ----
# --------------

#   -) Faig una FUNCIÓ
#   -) Creo 3 columnes = COORDS, LAT i LONG
 


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

# --- NETEJO DATA FRAME ----
# --------------------------

#   -) Em quedo amb només 3 columnes
#   -) Canvio el nom de una columna
#   -) Elimino una de les altres columnes



comarques_coord <- create_coordenates(comarques) %>%
  select(nm_cmrc,lat,long) %>%
  mutate( nom_comarca = nm_cmrc) %>% 
  select(-nm_cmrc) 

#   -) ORDENO les columne
#   -) Ho guardo com a GPKG

comarques_coord <- comarques_coord[, c("nom_comarca", "lat", "long")]

st_write(comarques_coord, "data/processed/COMARQUES_COORDS.gpkg", delete_layer = TRUE)

