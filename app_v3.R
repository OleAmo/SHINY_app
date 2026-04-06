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
    label = "Tria una assignatura:",
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
    
    create_DF_GEOM <- function(lat,long,data_inici,data_final)
      
    print(create_DF_GEOM)
    
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
      "<li>La Temperatura màxima =  ºC</li>",
      "<li>La Temperatura mínim = ºC</li>",
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








