
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



#  -------- PROVA amb DATA FARAME COMARQUE ---------
# --------------------------------------------------

#   -) Ho faig amb dades de COMARQUES
#   -) Busco vectors LAT i LONG
#   -) Començo amb 5 dies

data_1 <- "2026-03-01"
data_2 <- "2026-03-07"

lat_v <- comarques$lat
long_v <- comarques$long

num_lat <-as.integer(length(comarques$long))  

num_dies <- as.integer(as.Date(data_2) - as.Date(data_1))+1 

# ---- VECTOR DIES -----
# ----------------------

# -) Creo el VECTOR amb tots el dies
# -) Així al DF podre posar cada dia


# -) Em dona error al BUCLE
# -) I quan ho feina amb MENYS LATITUDS i DIES no em sortia



dies_v <- seq(as.Date(data_1), by = "day", length.out = num_dies)

df <- data.frame()

temps <-  system.time({                   # Calcula DURADA de al funció

    for(a in 1:num_lat) {
      for (i in 1:num_dies) {
        
        fila <- data.frame(
          
          Dies = dies_v[i],
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
  
})

print(temps)                           # ESCRIU la DURADA de al funció 
  




#  ---------- GUARDAR DF = RDS ------------
#  ----------------------------------------

#   -) Vull guardar DINS de RSTUDIO la info del GRAN DATA FRAME de Meteo
#   -) El format més eficient es RDS = No vull GEOMETRIA

#        -) guarda l’objecte exactament com és a R
#        -) es carrega molt ràpid
#        -) ocupa poc
#        -) manté tipus de dades (Date, numeric, character, etc.)
#        -) és ideal per Shiny





prova_2  <- readRDS("data/processed/prova_2.rds")
prova_2


saveRDS(df, "data/processed/comarques_.rds")


