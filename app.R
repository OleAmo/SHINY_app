# install.packages("shiny")

library(shiny)
library(bslib)
runExample("01_hello")


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


# *******  UI
# ***********

#    -) Creo una PAGE_SIDEBAR
#    -) TITOL
#    -) SIDEBAR = inputs,....



#    -) Guardso TOTA INTERFÍCIE GRÀFICA dins la variable ui.
#    -) 
#    -) 
#    -) 

ui <- page_sidebar(

  title = "Hello Shiny!",

  sidebar = sidebar(

    sliderInput(
      inputId = "bins",
      label = "Number of bins:",
      min = 1,
      max = 50,
      value = 30
    )
  ),
 
  plotOutput(outputId = "distPlot")
)

# *******  SERVER
# ***********

#    -) 
#    -) 
#    -) 
#    -) 
#    -) 


server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    
    x    <- faithful$waiting
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    hist(x, breaks = bins, col = "#007bc2", border = "white",
         xlab = "Waiting time to next eruption (in mins)",
         main = "Histogram of waiting times")
    
  })
  
}




shinyApp(ui = ui, server = server)
