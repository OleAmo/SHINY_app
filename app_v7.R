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

comarques <- st_read("data/processed/COMARQUES_COORDS.gpkg")
comarques <- st_transform(comarques, 4326)
comarques_vector <- c()

for(i in comarques$nom_comarca){
  comarques_vector <- c(comarques_vector,i)
}

# --- UI ---
# ----------


ui <- fluidPage(
  
  titlePanel("Mapa Comarques"),
  dateRangeInput(
    inputId = "periode",
    label = "Selecciona període:",
    start = Sys.Date() - 7,
    end = Sys.Date()
  ),
  selectInput(
    inputId = "comarca",
    label = "Tria una Comarca:",
    choices = comarques_vector
  ),
  leafletOutput("mapa", height = 500)
 
)


# --- SERVER ---
# --------------


server <- function(input, output, session) {
  
  
  output$mapa <- renderLeaflet({
    
    # --- DADES SHAPE ----
    # --------------------
    
    #   -) LAT i LONG de COMARCA
    #   -) DATA = Ve del INPUT de la UI
    
    comarca <- input$comarca
    comarca_df <- comarques %>% filter(nom_comarca==comarca)
    
    # --- LAT i LONG ---
    # ------------------
    
    #   -) Tranasformo a 4326 (x projectar al Mapa)
    #   -) Calculo Lat i Long
    
    coords <- comarca_df$geom %>% st_transform(4326) %>% 
      st_coordinates()
    long <- coords[1]
    lat <- coords[2]
    
    # --- DATA ---------
    # ------------------
    
    data <- input$data_examen
    
    data_inici <- input$periode[1]
    data_final <- input$periode[2]
    
    
    # --- DADES API METEO ----
    # ------------------------
    
    #   -) Uso FUNCIÓ de extreure DADES API = create_DF_GEOM()
    #   -) Les usaré NOMÉS per
    #         -) Valor de CADA TEMPERATURA MAXIMIA
    #         -) Crear una PALETA DE COLOR
    
    df <- create_DF_GEOM(lat,long,data_inici,data_final)
    
    
    
    # ---- PALETA DE COLOR ----
    # -------------------------
    
    #     -) Creo una PALETA DE COLOR
    #     -) Pot ser diferents = "YlOrRd", "Blues"
    #     -) Serà en funció de la columna T_max
    
    pal <- colorNumeric("viridis", domain = c(-10, 50))
    
    
    leaflet(data = comarca_df) %>%
      addTiles() %>%
      setView(lng = 2.0, lat = 41.6, zoom = 7) %>%
      addCircles(
        
        fillColor = ~pal(df$T_max),   # color interior
        color = "black",        # contorn
        fillOpacity = 0.8,
        radius = 8000,
        popup = ~paste0(
          "<b>Comarca:</b> ", comarca, "<br>",
          "<b>Dia:</b> ", data_inici, "<br>",
          "<b>Temp Màxma:</b> ", df[1,]$T_max, " ºC <br>",
          "<b>Humitat Màx:</b> ", df[1,]$Hum_max, " % <br>",
          "<b>Vent Màxma:</b> ", df[1,]$Win_max, " km/h <br>"
        )
        ) %>%
      addLegend(
        pal = pal,
        values = c(0, 50),
        title = "Temperatura"
      )
  })
  
 
}

shinyApp(ui, server)


# ------------- ERROR !! -------------
# ------------------------------------


#   -) Corregir en SHINY
#   -) La LATITUD i LONGITUD =  No son correctes i donen TEMP RARES ALTES
#         -) lat <- comarca_df$lat
#        -) long <- comarca_df$long







