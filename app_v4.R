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
  
    lat <- comarca_df$lat
    long <- comarca_df$long
    
    data <- input$data_examen
    
    
    data_inici <- input$periode[1]
    data_final <- input$periode[2] 
    
    if (data_inici<=data_final){
      
      df <- create_DF_GEOM(lat,long,data_inici,data_final)
      num_dies <- length(df$Dies)
      
      dia <- df$Dies[1]
      temp_max <- df$T_max[1]
      temp_min <- df$T_min[1]
      
     
      Hum_max  <- df$Hum_max[1]
      Hum_min  <- df$Hum_min[1]
      Win_max  <- df$Win_max[1]
      Win_min  <- df$Win_min[1]
     
      html <- paste(
      
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



#  ----- REPRESENTAR TOOOOOOOOOOTS ELS DIES DE DADES ---------
#  ----------------------------------------------------------

#    -) BUSCAR FORMAT QUE ES VEGI BE
#    -) HAURIA DE SER FORMAT DATA FRAME



 








