# install.packages("shiny")

library(shiny)





#  --------- EXEMPLE - FUNCIÓ EXTERNA --------
#  -------------------------------------------

#    -) FUNCIONS us de RANG DE DATES
#    -) FUNCIONS de OPTIONS
#    -) dateRangeInput → seleccionar interval de dates
#    -) choices → seleccionar opcions select

#    -) AFEGEIXO FUNCIONS Exteriors 








ui <- fluidPage(
  sliderInput(
    "temp",
    label = "Temperatura",
    min = 5, 
    max = 21, 
    value = 11
  ),
  sliderInput(
    "rang",
    label = "Rang de Temperatura",
    min = 1, 
    max = 7, 
    value = 3
  ),
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
    
    temp <- input$temp
    rang <- input$rang
    
    temperatura <- calcul_temp(temp,rang)
    
    temp_max <- temperatura$max
    temp_min <- temperatura$min
    
    data <- input$data_examen
    data_inici <- input$periode[1]
    data_final <- input$periode[2]
    num_dies <- data_final-data_inici
    
    comarca <- input$comarca
    
    HTML(paste0(
      "<h3 style='color:blue;'>EXEMPLE TAULA HTML</h2>",
      
      "<h4><b>DIES:<b></h4>",
      "<ul>",
      "<li>Dia Inici = [ ",data_inici," ]</li>",
      "<li>Dia Final = [ ",data_final," ]</li>",
      "<li>NÚMERO DE DIES = ",num_dies," dies</li>",
      "</ul>",
      "</br>",
      "<h4><b>Les TEMPERATURES:<b></h4>",
      "<ul>",
      "<li><b>COMARCA</b> = ",comarca," </li>",
      "<li>La Temperatura màxima = ",temp_max," ºC</li>",
      "<li>La Temperatura mínim = ",temp_min,"ºC</li>",
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








