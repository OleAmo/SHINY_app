# install.packages("leaflet")
# install.packages("shinycssloaders")

library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(readr)
library(shinycssloaders)   # SALVA PANTALLES de SHINY. Per si està pensant a l'inici




# ----- OBJECTIU QUE VAGI MÉS RÀPID ----
# ---------------------------------------

#   -) USO per calcular el TEMPS la f(x) = system.time({
#   -) Li assigno variable TEMPS
#   -) I dsps faig PRINT de TEMPS
#   -) I dona :

#   user  system elapsed 
#   1.77    0.06   10.49 

#   -) On el ELAPSED és el TEMPS que usa la funció = 10,49 segons
#   -) I aixó que la funció que CREA LES DADES = create_DF_NO_GEOM
#   -) La he modificat de l'arxiu FUNCIONS per que no calculi GEOMETRIA




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

# --- UI ---
# ----------


ui <- fluidPage(
  
  titlePanel("Mapa Comarques"),
  dateInput(
    inputId = "data",
    label = "Selecciona una data:",
    value = Sys.Date()
  ),
  leafletOutput("mapa", height = 500) %>% withSpinner() # Salava Pantalles

  # ------ MISSTGE de APP està PENSANT ----
  # ---------------------------------------
  
  #   -)   Quan una APP està CALCULANT DADES i TARDA
  #   -)   Es pot posar un SALVAPANTALLES mentres pensa
  #   -)   Es posa darrere de leafletOutput()
  
  #   -)   TIPUS:
  
  #   -)   withSpinner() → rodeta de càrrega
  #   -)   showNotification() → missatge “calculant...”   = complicat de usar
  #   -)   withProgress() → barra de progrés              = complicat de usar
 
)


# --- SERVER ---
# --------------


server <- function(input, output, session) {
  
  
  output$mapa <- renderLeaflet({
    
    
    # --- DATA ---------
    # -------------------
    
    
    data <- input$data
   
  
    
    # --- DADES API METEO ----
    # ------------------------
    
    #   -) Uso el SHAPE COMARQUES
    #   -) A cada fila afegeixo 3 COLUMENS = MUTATE
    #   -) Cada columna aplico FUNCIÓ de extreure DADES API = create_DF_GEOM()
    #   -) Per aplicar FUNCIÓ a cada fila uso rowwise()
  
    #   -) PROBLEMA = Va leeeeeeeeeeeeent!!  

    comarques_dades <- comarques 
    
    comarques_dades$data <- data
    
    temps <-  system.time({                   # Calcula DURADA de al funció
    comarques_dades <- comarques_dades %>%
      rowwise() %>%
      mutate(
        T_max = create_DF_NO_GEOM_ONE_DAY(lat,long,data)$T_max,
        Hum_max = create_DF_NO_GEOM_ONE_DAY(lat,long,data)$Hum_max,
        Win_max = create_DF_NO_GEOM_ONE_DAY(lat,long,data)$Win_max
      ) 
    })
    
    print(temps)                           # ESCRIU la DURADA de al funció
    

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
          "<b>Dia:</b> ",data , "<br>",
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


# ------------ NOVA FUNCIÓ ---------------------
# ---------------------------------------------------

#    -) Que vagi més ràpid = Potser sense geometria?? Funcions més rapides!!
#    -) Que mostri Temp, Hum , minima
#    -) Estadística amb dies ???






