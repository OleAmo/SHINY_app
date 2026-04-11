
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


#  ------------ PROVA SENZILLA ----------
# ---------------------------------------

#   -) Creo un DATA FRAME amb dos FORS
#   -) El 1r FOR és per les DATES
#   -) El 2n FOR es per les DADES

df <- data.frame()

for(a in 1:3) {
  for (i in 1:5) {
    
    fila <- data.frame(
      
      dia = paste("dia_",a),
      id = i,
      quadrat = i^2,
      doble = i*2
    )
    
    df <- rbind(df, fila)
  }
}

print(df)





#  ------------ PROVA NOVA DE CREAR DATA FARAME ----------
# ----------------------------------------------------------

#   -) Aplico NOU BUCLE de FORS
#   -) Aveure si va!


data_1 <- "2026-02-10"
data_2 <- "2026-02-15"

lat_v <- c(41.32810, 42.30470, 41.38778)
long_v <- c(1.3084410, 2.9537545, 1.6970029)

num_lat <- 3

num_dies <- as.integer(as.Date(data_2) - as.Date(data_1)) 

df <- data.frame()

for(a in 1:num_lat) {
  for (i in 1:num_dies) {
    
    fila <- data.frame(
      
      Dies = as.Date(data_1)+i,
      lat = lat_v[a],
      long = long_v[a],
      T_max = create_DF_GEOM(lat_v[a],long_v[a],data_1,data_2)$T_max[i],
      T_min = create_DF_GEOM(lat_v[a],long_v[a],data_1,data_2)$T_min[i],
      Hum_max = create_DF_GEOM(lat_v[a],long_v[a],data_1,data_2)$Hum_max[i],
      Hum_min = create_DF_GEOM(lat_v[a],long_v[a],data_1,data_2)$Hum_min[i],
      Win_max = create_DF_GEOM(lat_v[a],long_v[a],data_1,data_2)$Win_max[i],
      Win_min = create_DF_GEOM(lat_v[a],long_v[a],data_1,data_2)$Win_min[i]
    )
    
    df <- rbind(df, fila)
  }
}

df


#  ---------- GUARDAR DF = RDS ------------
#  ----------------------------------------

#   -) Vull guardar DINS de RSTUDIO la info del GRAN DATA FRAME de Meteo
#   -) El format més eficient es RDS = No vull GEOMETRIA

#        -) guarda l’objecte exactament com és a R
#        -) es carrega molt ràpid
#        -) ocupa poc
#        -) manté tipus de dades (Date, numeric, character, etc.)
#        -) és ideal per Shiny


saveRDS(df, "data/processed/prova_2.rds")


prova_2  <- readRDS("data/processed/prova_2.rds")
prova_2




