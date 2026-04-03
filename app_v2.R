# install.packages("shiny")

library(shiny)



#  --------- EXEMPLE 01 ---------
#  ------------------------------

#    -) Exemple de la web de SHINY R
#    -) Num de alumnes
#    -) Mitjana de NOTES


ui <- fluidPage(
  sliderInput(
    "n",
    label = "Nombre d'alumnes:",
    min = 10, 
    max = 100, 
    value = 30
  ),
  textOutput("mitjanaText")
)

server <- function(input, output) {
  
  output$mitjanaText <- renderText({
    notes <- rnorm(input$n, mean = 6, sd = 1)
    paste("La mitjana de les notes és:", round(mean(notes), 2))
  })
}

shinyApp(ui, server)


#  --------- EXEMPLE 02 ---------
#  ------------------------------

#    -) Creo DOS SLIDERS INPUT = Temp Max i Temp Min
#    -) Els REPRESENTO de 3 FORMES
#    -) TEXT, TAULA i TAULA PRO


ui <- fluidPage(
  sliderInput(
    "temp_max",
    label = "Temperatura Max",
    min = 17, 
    max = 31, 
    value = 15
  ),
  sliderInput(
    "temp_min",
    label = "Temperatura Min",
    min = -5, 
    max = 18, 
    value = 9
  ),
  textOutput("temp_text"),
  tableOutput("temp_taula"),
  dataTableOutput("temp_taula_Pro")
)

server <- function(input, output) {
  
  output$temp_text <- renderText({
    tmax <- input$temp_max
    tmin <- input$temp_min
    
    mitja <- round(tmax)
    
    paste0("Temp màxima =",tmax,"  Temp mínima =",tmin)
  })
  
  output$temp_taula <- renderTable({
    tmax <- input$temp_max
    tmin <- input$temp_min
    
    data.frame(Temp_max = tmax, Temp_min = tmin)
  })
  
  output$temp_taula_Pro <- renderDataTable({
    tmax <- input$temp_max
    tmin <- input$temp_min
    
    data.frame(Temp_max = tmax, Temp_min = tmin)
  })
  
  
}

shinyApp(ui, server)














#  --------- EXEMPLE xxxx ---------
#  ------------------------------

#    -) Fer exemple que AGAFI DADES DE API METEO
#    -) Que les representi
#    -) Max, Min, Dia,...



#  SEGUIIIIIIIIIIIIIIIR!!!!!!








