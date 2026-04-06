# install.packages("shiny")

library(shiny)





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
    choices = c("Barcelona", "Girona", "Tarragona")
  ),
  htmlOutput("text_HTML")
)

server <- function(input, output) {
  
  output$text_HTML <- renderUI({
    
    
    lat <- 41.4051879
    long <- 1.9964933 
    
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
      
      
    } else {
      temp_max <- "ERROR Data Final"
      temp_min <- "ERROR Data Final"
    }
    
    
      
    comarca <- input$comarca
    
    HTML(paste0(
      "<h3 style='color:blue;'>EXEMPLE TAULA HTML</h2>",
      
      "<h4><b>DIES:<b></h4>",
      "<ul>",
      "<li>Dia Inici = [ ",data_inici," ]</li>",
      "<li>Dia Final = [ ",data_final," ]</li>",
      "</ul>",
      "</br>",
      "<h4><b>DADES:<b></h4>",
      "<ul>",
      "<li><b>COMARCA</b> = ",comarca," </li>",
      "<li>La Temperatura màxima = ",temp_max," ºC</li>",
      "<li>La Temperatura mínim = ",temp_min," ºC</li>",
      "<li>La Humitat màxima = ",Hum_max," </li>",
      "<li>La Humitat mínim = ",Hum_min," </li>",
      "<li>El Vent màxim = ",Win_max," </li>",
      "<li>El Vent mínim = ",Win_min," </li>",
      "</ul>"
      
    ))
    
  })
}


shinyApp(ui, server)





#  --------- EXEMPLE xxxx ---------
#  ------------------------------

#    -) Fer exemple que AGAFI DADES DE API METEO
#    -) Que les representi
#    -) Max, Min, Dia,...



#  SEGUIIIIIIIIIIIIIIIR!!!!!!








