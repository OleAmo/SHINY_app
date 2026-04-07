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
      
      temp_max <- df$T_max[1]
      temp_min <- df$T_min[1]
      
     
      Hum_max  <- df$Hum_max[1]
      Hum_min  <- df$Hum_min[1]
      Win_max  <- df$Win_max[1]
      Win_min  <- df$Win_min[1]
      
      html <- paste(
        
        "<tr>",
          "<td>DIA!!</td>" , 
          "<td>",temp_max," ºC</td>",  
          "<td>",temp_min," ºC</td>",
          "<td>",Hum_max,"</td> " ,
          "<td>",Hum_min,"</td> ", 
          "<td>",Win_max,"</td> " , 
          "<td>",Win_min,"</td> " , 
        "</tr>"      )
      
      
    } else {
      html <- paste( "<td> ERROR  !!! </td>",
                     "<td> Data 2 > Data 1 </td>")
    }
    
    
    HTML(paste0(
      "<h3 style='color:blue;'>DADES METEO per COMARCA</h2>",
      
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



 








