# install.packages("shiny")

library(shiny)
library(bslib)
runExample("01_hello")
runExample("07_widgets")


#  ---- FUNCIONAMENT SHINY ------
#  ------------------------------

#    -) Les APP Shiny =  DINS un únic script = app.R.
#    -) APP.R
#          +) UI 
#              // un objecte d'interfície d'usuari
#              // controla el disseny i l'aparença de l'aplicació

#          +) SERVER
#              // una funció de servidor
#              // conté les instruccions que l'ordinador necessita per crear l'aplicació

#          +) SHINY APP
#              // una crida a la shinyApp funció
#              // crea objectes d'aplicació Shiny a partir de
#              // un parell explícit d'interfície d'usuari/servidor.



#  --------- EXEMPLE 01 ---------
#  ------------------------------

#    -) Exemple de la web de SHINY R
#    -) UI
#    -) SERVER
#    -) APPY SHINY


# *******  UI *************
# *************************

#    -) PAGE_SIDEBAR:

#        * Això crea una pàgina amb:
#        * una barra lateral a un costat
#        * una zona principal on es mostraran resultats

#    -) SLIDERINPUT: una barra que pots moure amb el ratolí.

#        * INPUT ID = "dades" => és el valor de la variable a usar
#        * Al SERVER = podràs llegir aquest valor = input$dades

#        * MAX, MIN = Valors de la barra
#        * VALUE = Valor en que comença al obrir la APP

#    -) PLOTOUTPUT: 

#        * espai a la pàgina per mostrar un gràfic
#        * Al SERVER = podràs EXPORTAR aquest valor = output$dades_output


ui <- page_sidebar(

  title = "PROVA Shiny!",

  sidebar = sidebar(

    sliderInput(
      inputId = "dades",
      label = "Number de Dades:",
      min = 1,
      max = 50,
      value = 30
    )
  ),
 
  plotOutput(outputId = "dades_output")
)

# *******  SERVER
# ***************

#    -) output$dades_output <- renderPlot({

#        * el contingut que anirà a DADES_OUTPUT
#        * serà un gràfic
#        * i aquest gràfic es generarà amb renderPlot()


server <- function(input, output) {
  
  output$dades_output <- renderPlot({
    
    x    <- faithful$waiting
    bins <- seq(min(x), max(x), length.out = input$dades + 1)
    
    hist(x, breaks = bins, col = "#007bc2", border = "white",
         xlab = "Titol XLAB",
         main = "Títol PRINCIPAL")
    
  })
  
}


shinyApp(ui = ui, server = server)



#  --------- EXEMPLE 02 ---------
#  ------------------------------

#    -) Exemple de la web de SHINY R
#    -) UI
#    -) SERVER
#    -) APPY SHINY

#    -) Creat amb CHATGPT
#    -) Amb info més clara per mi




ui <- page_sidebar(
  title = "Simulador de notes",
  
  sidebar = sidebar(
    
    sliderInput("n", "Nombre d'alumnes:", min = 10, max = 200, value = 50),
    sliderInput("mitjana", "Nota mitjana:", min = 0, max = 10, value = 5),
    sliderInput("sd", "Dispersió (variabilitat):", min = 0.5, max = 3, value = 1),
    sliderInput("bins", "Nombre de barres:", min = 5, max = 30, value = 10)
  ),
  
  plotOutput("histNotes")
)


server <- function(input, output) {
  
  output$histNotes <- renderPlot({
    
    # 1 Generem notes simulades (distribució normal)
    # 2 Limitem les notes entre 0 i 10
    # 3 Definim els bins segons el slider
    # 4 Fem l'histograma
    # 5 Afegim línia de la mitjana (extra visual TOP 🔥)
    
    
    notes <- rnorm(     
      n = input$n,
      mean = input$mitjana,
      sd = input$sd
    )
    
    notes <- pmax(pmin(notes, 10), 0) 
    bins <- seq(0, 10, length.out = input$bins + 1)
    
    hist(                                      
      notes,
      breaks = bins,
      col = "#2ecc71",
      border = "white",
      main = "Distribució de notes",
      xlab = "Nota",
      ylab = "Nombre d'alumnes"
    )
    
    abline(v = mean(notes), col = "red", lwd = 2) 
    
  })
}

shinyApp(ui = ui, server = server)


#  --------- EXEMPLE 03 ---------
#  ------------------------------

#    -) Fer exemple que AGAFI DADES DE API METEO
#    -) Que les representi
#    -) Max, Min, Dia,...



#  SEGUIIIIIIIIIIIIIIIR!!!!!!








