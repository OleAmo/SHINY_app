# install.packages("shiny")

library(shiny)
library(sf)
library(dplyr)
library(readr)



#  --------- EXEMPLE - FUNCIÓ EXTERNA --------
#  -------------------------------------------

#    -) FUNCIONS us de RANG DE DATES
#    -) FUNCIONS de OPTIONS
#    -) dateRangeInput → seleccionar interval de dates
#    -) choices → seleccionar opcions select

#    -) AFEGEIXO FUNCIONS Exteriors 
#    -) Son les FUNCIONS de OBTENIR DADES D'API OPEN METEO

#    -) Afegir CSS
#    -) Per afegir CSS en lloc diferent al d'HTML(paste())
#    -) O faig a la UI usant:

#        -)   tags$head(
#        -)     tags$style(HTML("

#    -) I després al server en el HTML(paste()) afegeixo class='xxx'

# ----- FUNCIONS en ALTRE ARXIU -------
# ------------------------------------

source("scripts/funcions.R")


# ----- DADES -------
# -------------------

comarques <- st_read("data/processed/COMARQUES_COORDS.gpkg")

comarques_vector <- c()

for(i in comarques$nom_comarca){
  comarques_vector <- c(comarques_vector,i)
}

# ----- SHINY -------
# -------------------


ui <- fluidPage(
  

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
  tags$head(
    tags$style(HTML("
    
      .titol {
        color: blue;
        font-size: 22px;
      }
      
      .titol_2{
        color: #383194;
        font-size: 17px;
      
      }
      
      .error{
        color = #6E1B00 ;
        background-color: #FA845C;
      
      }
      
      
      table {
        font-family: arial, sans-serif;
        border-collapse: collapse;
        width: 100%;
      }
      
      td, th {
        border: 1px solid #dddddd;
        text-align: left;
        padding: 8px;
      }
      
      tr:nth-child(even) {
        background-color: #dddddd;
      }
    "))
  ),
  htmlOutput("text_HTML")
)

server <- function(input, output) {
  
  output$text_HTML <- renderUI({
    
    
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
    
    if (data_inici<=data_final){
      
      df <- create_DF_GEOM(lat,long,data_inici,data_final)
      num_dies <-as.numeric(length(df$Dies)) 
      
      html <- ""
      
      for (i in 1:num_dies){
        
        dia <- df$Dies[i]
        temp_max <- df$T_max[i]
        temp_min <- df$T_min[i]
        
        
        Hum_max  <- df$Hum_max[i]
        Hum_min  <- df$Hum_min[i]
        Win_max  <- df$Win_max[i]
        Win_min  <- df$Win_min[i]
        
        html <- paste(html,
          
          "<tr>",
          "<td>",dia,"</td>" , 
          "<td>",temp_max," ºC</td>",  
          "<td>",temp_min," ºC</td>",
          "<td>",Hum_max,"</td> " ,
          "<td>",Hum_min,"</td> ", 
          "<td>",Win_max,"</td> " , 
          "<td>",Win_min,"</td> " , 
          "</tr>"
          
        )
        
      }
      
    } else {
      html <- paste( "<td class='error'> ERROR  !!! </td>",
                     "<td class='error'> Data 2 > Data 1 </td>")
    }
    
    
    HTML(paste0(
      "<p class='titol'>DADES METEO per COMARCA</p>",
      "<p class='titol_2'>COMARCA = ",comarca,"</p>",
      
      "<table>",
        
        "<tr>",
        "<th>Dia</th>" ,
        "<th>Temp_Max</th>",
        "<th>Temp_Min</th>",
        "<th>Hum_Max</th>" ,
        "<th>Hum_Min</th>",
        "<th>Win_Max</th>",
        "<th>Win_Min</th>", 
        "</tr>",
      html,
      "</table>"
      
    ))
    
  })
}


shinyApp(ui, server)



#  ----- REPRESENTAR EN MAPA ---------
#  -----------------------------------

#    -) BUSCAR FORMAT QUE ES VEGI BE
#    -) HAURIA DE SER FORMAT SHAPE A MAPA



 








