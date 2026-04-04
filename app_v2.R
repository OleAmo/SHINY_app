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


#  --------- EXEMPLE 02 - TEXT, TAULA --------
#  -------------------------------------------

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


#  --------- EXEMPLE 02 - HTML --------
#  -------------------------------------------

#    -) Creo DOS SLIDERS INPUT = Temp Max i Temp Min
#    -) Ho faig en HTML


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
  htmlOutput("resumHTML")
)

server <- function(input, output) {
  
  output$resumHTML <- renderUI({
    
    tmax <- input$temp_max
    tmin <- input$temp_min
    
    HTML(paste0(
      "<h3 style='color:blue;'>EXEMPLE TAULA HTML</h2>",
      
      "<h4><b>Les TEMPERATURES:<b></h4>",
      "<ul>",
      "<li>La Temperatura màxima = ",tmax," ºC</li>",
      "<li>La Temperatura mínim = ",tmin,"ºC</li>",
      "</ul>"
      
    ))
    
  })
}

server_v2 <- function(input, output) {
  
  output$resumHTML <- renderUI({
    
    tmax <- input$temp_max
    tmin <- input$temp_min
    
    HTML(glue::glue("
    
      <h3 
         style='
         font-family: arial, sans-serif;
         background-color: #A6C0FF;
         color: #223B7D;
         width: 150px
         '
      >HTML Table</h3>
      
      <div 
         style='
         font-family: arial, 
         sans-serif;
         width: 150px;
         border: 2px solid #FF1C1C;
         '
        >
       <table
          style='
          width: 150px;
       '>
        <tr style='background-color: #99FF94;'>
          <th>T Màx</th>
          <th>T Mín</th>
        </tr>
        <tr>
          <td>{tmax}</td>
          <td>{tmin}</td>
        </tr>
        <tr>
          <td>{tmax*2}</td>
          <td>{tmin*2}</td>
        </tr>
        <tr>
          <td>{tmax*3}</td>
          <td>{tmin*3}</td>
        </tr>
        
      </table>
      </div>

    "))
    
  })
}


shinyApp(ui, server)
shinyApp(ui, server_v2)




  



#  ----------- EXEMPLE 03 - DATES ----------
#  -------------------------------------------

#    -) FUNCIONS us de DATES
#    -) dateInput → seleccionar una data


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
  dateInput(
    inputId = "data_examen",
    label = "Selecciona la data:",
    value = Sys.Date()
  ),
  htmlOutput("text_HTML")
)

server <- function(input, output) {
  
  output$text_HTML <- renderUI({
    
    tmax <- input$temp_max
    tmin <- input$temp_min
    data <- input$data_examen
    
    HTML(paste0(
      "<h3 style='color:blue;'>EXEMPLE TAULA HTML</h2>",
      
      "<h4><b>Les TEMPERATURES:<b></h4>",
      "<ul>",
      "<li>Dia seleccionat = [ ",data," ]</li>",
      "<li>La Temperatura màxima = ",tmax," ºC</li>",
      "<li>La Temperatura mínim = ",tmin,"ºC</li>",
      "</ul>"
      
    ))
    
  })
}


shinyApp(ui, server)


#  ----------- EXEMPLE 04 - PERÍODE DATES ----------
#  -------------------------------------------

#    -) FUNCIONS us de DATES
#    -) dateRangeInput → seleccionar interval de dates



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
  dateRangeInput(
    inputId = "periode",
    label = "Selecciona període:",
    start = Sys.Date() - 7,
    end = Sys.Date()
  ),
  htmlOutput("text_HTML")
)

server <- function(input, output) {
  
  output$text_HTML <- renderUI({
    
    tmax <- input$temp_max
    tmin <- input$temp_min
    data <- input$data_examen
    data_inici <- input$periode[1]
    data_final <- input$periode[2]
    num_dies <- data_final-data_inici
    
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
      "<li>La Temperatura màxima = ",tmax," ºC</li>",
      "<li>La Temperatura mínim = ",tmin,"ºC</li>",
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








