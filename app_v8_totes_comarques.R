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
    
    #   -) Uso el SHAPE COMARQUES
    #   -) A cada fila afegeixo 3 COLUMENS = MUTATE
    #   -) Cada columna aplico FUNCIÓ de extreure DADES API = create_DF_GEOM()
    #   -) Per aplicar FUNCIÓ a cada fila uso rowwise()
  
    #   -) PROBLEMA = Va leeeeeeeeeeeeent!!  

    comarques_dades <- comarques 
    
    comarques_dades$data <- "2026-03-01" #data_inici
    
    comarques_dades <- comarques_dades %>%
      rowwise() %>%
      mutate(
        T_max = create_DF_GEOM(lat,long,data,data)$T_max,
        Hum_max = create_DF_GEOM(lat,long,data,data)$Hum_max,
        Win_max = create_DF_GEOM(lat,long,data,data)$Win_max
      )
    

    # ---- PALETA DE COLOR ----
    # -------------------------
    
    #     -) Creo una PALETA DE COLOR
    #     -) Pot ser diferents = "YlOrRd", "Blues"
    #     -) Serà en funció de la columna T_max
    
    
    temp_max <- max(comarques_dades$T_max)
    temp_min <- min(comarques_dades$T_max)
    
    pal <- colorNumeric("viridis", domain = c(temp_min, temp_max))
    
    
    leaflet(data = comarques_dades) %>%
      addTiles() %>%
      setView(lng = 2.0, lat = 41.6, zoom = 7) %>%
      addCircles(
        
        fillColor = ~pal(comarques_dades$T_max),   # color interior  
        color = "black",        # contorn
        fillOpacity = 0.8,
        radius = 8000,
        popup = ~paste0(
          "<b>Comarca:</b> ", comarca, "<br>",
          "<b>Dia:</b> ",data_inici , "<br>",
          "<b>Temp Màxma:</b> ", comarques_dades$T_max, " ºC <br>",
          "<b>Humitat Màx:</b> ", comarques_dades$Hum_max, " % <br>",
          "<b>Vent Màxma:</b> ", comarques_dades$Win_max, " km/h <br>"
        )
        ) %>%
      addLegend(
        pal = pal,
        values = c(temp_min, temp_max),
        title = "Temp_Maxima"
      )
  })
  
 
}

shinyApp(ui, server)


# ------------ EL CALCULAR TOTES LE COMARQUES ALHORA 
# --------------  TARDA BASTANT RATO !!!!








