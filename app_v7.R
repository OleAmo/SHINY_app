# install.packages("leaflet")

library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(readr)




# -------- MAPA API METEO ---------
# ---------------------------------

#   -) XXXX


source("scripts/funcions.R")

# --- OBTENIR PUNT GEOMETRIA ---
# ------------------------------

#   -) OBTINDREM Long i Lag 
#   -) Podrem PORJECTAR ho al mapa

#   -) Passar a PROJECCIÓ 4326  (google maps)
#   -) El shape estava en 25831 (catalunya)

comarques <- st_read("data/processed/COMARQUES_CENTROIDES.shp")
comarques <- st_transform(comarques, 4326)


# --- UI ---
# ----------


ui <- fluidPage(
 
)


# --- SERVER ---
# --------------


server <- function(input, output, session) {
  
 
}

shinyApp(ui, server)

