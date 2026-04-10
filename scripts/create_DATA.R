
library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(readr)
library(shinycssloaders)   # SALVA PANTALLES de SHINY. Per si està pensant a l'inici



# ------ DATA FRAME = INFO 3 SETMANES -----
# -----------------------------------------

#    -) Creare un DATA FRAME de
#    -) TOTES COMARQUES
#    -) INF DE 3 SETMANES

#    -) Creare un SHAPE i el GUARDARE a una carpeta
#    -) Així la APPI SHINY anirà molt ràpid ja que les dades estaran fetes
#    -) Així el VISOR DE DADES anirà RAPID



# -------- FUNCIONS per obtenrir DADES API METEO ---------
# -------------------------------------------------------

source("scripts/funcions.R")
source("scripts/funcions_v2.R")

# ----- COMARQUES GEOMETRIA ----
# ------------------------------

#   -) Es la geometria a projectar al mapa
#   -) Són els punts x CADA COMARCA = SORTIRANT TOTS ALHORA AL MAPA

#   -) Passar a PROJECCIÓ 4326  (google maps)
#   -) El shape estava en 25831 (catalunya)

comarques <- st_read("data/processed/COMARQUES_COORDS.gpkg")
comarques <- st_transform(comarques, 4326)



create_DF_NO_GEOM_ONE_DAY <- function(lat,long,date){
  
  res <- GET(
    "https://archive-api.open-meteo.com/v1/archive",
    query = list(
      latitude = lat,
      longitude = long,
      start_date = date,
      end_date = date,
      hourly = "temperature_2m,relative_humidity_2m,windspeed_10m"
    )
  )  
  
  text <- content(res, "text")
  dades <- fromJSON(text)
  
  temp_vec <- dades$hourly$temperature_2m
  hum_vec <- dades$hourly$relative_humidity_2m
  wind_vec <- dades$hourly$windspeed_10m
  
  resultat <- data.frame(
    Dies = date,
    T_max = max(temp_vec),
    T_min = min(temp_vec),
    Hum_max = max(hum_vec),
    Hum_min = min(hum_vec),
    Win_max = max(wind_vec),
    Win_min = min(wind_vec)
    
  )
  
  return(resultat)
  
}


#  ------------ PROVA DE CREAR DATA FARAME ----------
# --------------------------------------------------

comarques

data_1 <- "2026-02-10"
data_2 <- "2026-02-15"

lat <- 41.32810 
long <- 1.3084410

comarques_dades <- comarques %>%
  rowwise() %>%
  mutate(
    T_max = create_DF_GEOM(lat,long,data_1,data_2)$T_max,
    Hum_max = create_DF_GEOM(lat,long,data_1,data_2)$Hum_max,
    Win_max = create_DF_GEOM(lat,long,data_1,data_2)$Win_max
  )

dresultat <- data.frame(
  Dies = date,
  T_max = max(temp_vec),
  T_min = min(temp_vec),
  Hum_max = max(hum_vec),
  Hum_min = min(hum_vec),
  Win_max = max(wind_vec),
  Win_min = min(wind_vec)
  
)


