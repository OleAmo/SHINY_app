# install.packages("shiny")

library(shiny)
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








