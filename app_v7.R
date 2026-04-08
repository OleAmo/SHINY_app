# install.packages("leaflet")

library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(readr)

# ---- MAPA BÀSIC -----
# ---------------------

ui <- fluidPage(
  titlePanel("Primer mapa amb Shiny"),
  leafletOutput("mapa", height = 500)
)

server <- function(input, output, session) {
  
  output$mapa <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%   # mapa base OpenStreetMap
      setView(lng = 2.17, lat = 41.38, zoom = 10)   # Barcelona
  })
}

shinyApp(ui, server)


# -------- MAPA PUNTS ---------
# -----------------------------

#   -) Necessito un DATA FRAME
#   -) UI = nomes poso TITOL i que SURT un LEAFLETOUT
#   -) SERVER:
#        +) leaflet() = Indica quin DATAFRAME projectaré
#        +) addMarkers() = sòn per ETIQUETES
#              -) per funcionar necessita saber:
#              -) La columna LONGITUD i LATITUD del DATA FRAME
#              -) I quin nom donc



# --- DATA FRAME ---
# ------------------

llocs <- data.frame(
  nom_comarca = c("Barcelona", "Girona", "Lleida"),
  longitud = c(2.17, 2.82, 0.62),
  latitud = c(41.38, 41.98, 41.62)
)

ui <- fluidPage(
  titlePanel("Mapa amb marcadors"),
  leafletOutput("mapa", height = 500)
)

server <- function(input, output, session) {
  
  output$mapa <- renderLeaflet({
    leaflet(data = llocs) %>%
      addTiles() %>%
      setView(lng = 2.0, lat = 41.6, zoom = 8) %>%
      addMarkers(
        lng = ~longitud,
        lat = ~latitud,
        popup = ~nom_comarca
      )
  })
}

shinyApp(ui, server)


# -------- MAPA COMARQUES ---------
# ---------------------------------

#   -) USARE Shape COMARQUE
#   -) Pintaré els PUNTS en funció TEMP_MAX

#   -) Necessito un DATA FRAME
#   -) UI = nomes poso TITOL i que SURT un LEAFLETOUT
#   -) SERVER:
#        +) leaflet() = Indica quin DATAFRAME projectaré
#        +) addMarkers() = sòn per ETIQUETES
#              -) per funcionar necessita saber:
#              -) La columna LONGITUD i LATITUD del DATA FRAME
#              -) I quin nom donc




# --- DATA FRAME ---
# ------------------

#   -) Passar a PROJECCIÓ 4326  (google maps)
#   -) El shape estava en 25831 (catalunya)

comarques <- st_read("data/processed/COMARQUES_CENTROIDES.shp")
comarques <- st_transform(comarques, 4326)


# --- UI ---
# ----------


ui <- fluidPage(
  titlePanel("Mapa comarques"),
  leafletOutput("mapa", height = 500)
)


# --- SERVER ---
# --------------


server <- function(input, output, session) {
  
  # ---- PALETA DE COLOR ----
  # -------------------------
  
  #     -) Creo una PALETA DE COLOR
  #     -) Pot ser diferents = "YlOrRd", "Blues"
  #     -) Serà en funció de la columna T_max
  
  pal <- colorNumeric("viridis", domain = comarques$T_max)
  
  output$mapa <- renderLeaflet({
    leaflet(data = comarques) %>%
      addTiles() %>%
      setView(lng = 2.0, lat = 41.6, zoom = 7) %>%
      addCircles(
        
        fillColor = ~pal(T_max),   # color interior
        color = "black",           # contorn
        fillOpacity = 0.8,
        radius = 8000,
        popup = ~nm_cmrc
      )
  })
}

shinyApp(ui, server)

