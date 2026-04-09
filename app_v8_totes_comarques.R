# install.packages("leaflet")
# install.packages("shinycssloaders")

library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(readr)
library(shinycssloaders)   # RELLOTGE SHINY està pensant al inici



# -------- FUNCIONS per obtenrir DADES API METEO ---------
# -------------------------------------------------------

source("scripts/funcions.R")


# ----- COMARQUES GEOMETRIA ----
# ------------------------------

#   -) Es la geometria a projectar al mapa
#   -) Són els punts x CADA COMARCA = SORTIRANT TOTS ALHORA AL MAPA

#   -) Passar a PROJECCIÓ 4326  (google maps)
#   -) El shape estava en 25831 (catalunya)

comarques <- st_read("data/processed/COMARQUES_COORDS.gpkg")
comarques <- st_transform(comarques, 4326)

# --- UI ---
# ----------


ui <- fluidPage(
  
  titlePanel("Mapa Comarques"),
  dateInput(
    inputId = "data",
    label = "Selecciona una data:",
    value = Sys.Date()
  ),
  dateRangeInput(
    inputId = "periode",
    label = "Selecciona període:",
    start = Sys.Date() - 7,
    end = Sys.Date()
  ),
  leafletOutput("mapa", height = 500)%>% withSpinner()
 
)


# --- SERVER ---
# --------------


server <- function(input, output, session) {
  
  
  output$mapa <- renderLeaflet({
    
    # --- LAT i LONG de TOTES LES COMARQUES ---
    # -----------------------------------------
    
    #   -) Tranasformo a 4326 (x projectar al Mapa)
    #   -) Calculo Lat i Long
    
    coords <- comarques$geom %>% st_transform(4326) %>% 
      st_coordinates()
    long <- coords[1]
    lat <- coords[2]
    
    # --- DATES ---------
    # -------------------
    
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
    
    comarques_dades$data <- data_inici
    
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
          "<b>Comarca:</b> ", comarques_dades$nom_comarca, "<br>",
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


# ------------ ELIMINAR LO DE DOS DATES DE INICI
# ---------------------------------------------------








