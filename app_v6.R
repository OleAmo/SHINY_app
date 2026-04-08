# install.packages("shiny")

library(shiny)
library(sf)
library(dplyr)
library(readr)
library(leaflet)

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


# - DADES ---
# ------------


source("scripts/funcions.R")

comarques <- st_read("data/processed/COMARQUES_COORDS.gpkg")

comarques_vector <- c()
for(i in comarques$nom_comarca){
   comarques_vector <- c(comarques_vector,i)
 }


# --- UI ---
# ----------


ui <- fluidPage(
  titlePanel("Mapa comarques"),
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
  

  
  
  
  
  # ---- MAPA ----
  # --------------
  
  output$mapa <- renderLeaflet({
   
    # ---- DADES DEL SHAPE ----
    # -------------------------
    
    comarca <- input$comarca
    comarca_df <- comarques %>% filter(nom_comarca==comarca)
    
    lat <- comarca_df$lat
    long <- comarca_df$long
    
    data <- input$data_examen
    
    
    data_inici <- input$periode[1]
    data_final <- input$periode[2] 
    
    #  ---- CANVI DE PROJECCIÓ ----
    # -----------------------------
    
    #   -) comarca_select <- st_transform(comarca_select, 4326)
    #   -) Passar a PROJECCIÓ 4326  (google maps)
    #   -) El shape estava en 25831 (catalunya)
    
   
    comarca_select <- create_DF_GEOM(lat,long,data_inici,data_final)
    comarca_select_2 <- st_transform(comarca_select, 4326)
    
    # de tots els dies només projecto UN DIA
    comarca_select_2 <-comarca_select_2[1,] 
    
    # ---- PALETA DE COLOR ----
    # -------------------------
    
    #     -) Creo una PALETA DE COLOR
    #     -) Pot ser diferents = "YlOrRd", "Blues"
    #     -) Serà en funció de la columna T_max
    
    pal <- colorNumeric("viridis", domain = comarca_select$T_max)
    
    
    leaflet(data = comarca_select_2) %>%
      addTiles() %>%
      setView(lng = 2.0, lat = 41.6, zoom = 7) %>%
      addCircles(
        
        fillColor = ~pal(T_max),   # color interior
        color = "black",           # contorn
        fillOpacity = 0.8,
        radius = 8000,
        popup = ~paste(comarca," = ",as.character(T_max)," ºC")
      )
  })
}

shinyApp(ui, server)



#  -------- ERROR!!!!  ------------
#  -----------------------------------

#    -) NO PROJECTA
#    -) NO DETECTA LA LATITUD I LONGITUD
#    -) Si poso AIXÒ en el SERVER si que funciona
#         -) long = 2.0
#         -) lat = 41.6

#    -) REVISAR ja que ara barrejo el que feia amb:

#        -) mapa_exemple = output$mapa <- renderLeaflet({
#        -) app_v5       = output$text_HTML <- renderUI({

#    -) Deu haver temes de REACTIVE DADES o algo per l'estil




 








