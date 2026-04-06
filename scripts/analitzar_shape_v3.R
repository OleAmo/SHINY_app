
# install.packages(c("sf", "terra", "tmap", "ggplot2", "dplyr", "readr"))

library(sf)
library(terra)
library(tmap)
library(ggplot2)
library(dplyr)
library(readr)

# llegir un shape

rius_cat <- st_read("data/raw/RIUS/PROVA/catalunya_prova.shp")
comarques <- st_read("data/raw/TERRITORI/comarques_prova.shp")
urbanisme <- st_read("data/raw/TERRITORI/URBANISME_Cat_prova.shp")



#    --------------------------------------
#    --------------------------------------
#         OBJECTIU: " MODIFICAR FILES "
#    --------------------------------------
#    --------------------------------------


#    FER INTERSECT 
#    FER SUMARIZE amb lo INTERSECTAT
#    -------------------------------

#    VUll crear INTERSECCIÓ
#    RIUS amb COMARQUES

intersection <- st_intersection(rius_cat, comarques)


#    ARA HE D'ANALITZAR la intersecció
#    He de mirar quine columnes tinc i que signigiquen

names(intersection)


#   ---- SUMARIZE ----
#   ------------------

#   AGRUPO per COMARQUES
#   CALCULO 
#      .-) la MITJA de LONGITUDS
#      .-) la SUMA de LONGITUDS x COMARCA
#      .-) El NÚMERO de FILES x selecció =  n()

#      .-) Ho transformo a DATAFRAME
#      .-) Li TREC GEOMETRIA 

intersection_COMARAQUES <- intersection %>%
  group_by(NOMCOMAR) %>%
  summarise(
    suma_longitud = sum(longitud),      # SUMA
    mitjana_longitud = mean(longitud),  # MITJA
    numero = n()                        # NÚMERO de FILES x = COUNT() de SQL
  ) %>% data.frame()%>%
  select(-geometry)                     # Li TREC GEOMETRY


#   AGRUPO per RIUS
#   CALCULO 
#      .-) la MITJA de LONGITUDS
#      .-) la SUMA de LONGITUDS x COMARCA

#      .-) Ho transformo a DATAFRAME
#      .-) Li TREC GEOMETRIA

intersection_RIUS <- intersection %>%
  group_by(nom_final) %>%
  summarise(
    numero = n()
  ) %>% data.frame()%>%
  select(-geometry)

intersection_COMARAQUES
intersection_RIUS


#   CALCULO %
#      .-) % de LONGITUD de RIU x COMARCA
#      .-) % de NUMERO de RIUS x COMARCA

#      .-) NECESSITO 1r un DATA FRAME TOTAL
#      .-) Es la SUMA TOTAL de LONGITUDS i NUMERO DE TRAMS

#      .-) DESPRÉS amb MUTATES
#      .-) CALCULO els %
  
intersection_suma <- intersection_COMARAQUES %>%
  summarise(
    total_long = sum(suma_longitud),
    total_trams = sum(numero)
  ) %>% data.frame()


intersection_COMARAQUES %>%
  mutate(
    long_p = round((suma_longitud/as.integer(intersection_suma[1]))*100,2),
    trams_p = round((numero/as.integer(intersection_suma[2]))*100,2),
  ) 


#  URBANITZACIONS
#  CALCULO:
#    -) Número de URBS x COMARCA
#    -) Area_m2
#    -) Area_ha

urb_select <- urbanisme %>% 
  select(NOM, NOMCOMAR) %>%
  group_by(NOMCOMAR) %>%
  summarise( num_muni = n() ) %>% 
  mutate ( 
    area_m2 = st_area(geometry),
    area_ha = round(area_m2/10000,2)) %>%
  data.frame() %>%
  select(-geometry)

#  CALCULO:
#    -) TOTAL= Número de URBS x COMARCA
#    -) TOTAL= suma Area_m2
#    -) TOTAL= suma Area_ha

urb_select_sumes <- urb_select %>%
  summarise( 
    num_muni_total = sum(num_muni),
    area_m2_total = sum(area_m2),
    area_ha_total = sum(area_ha))

urb_select_final <- urb_select %>%
  mutate(
    num_muni_p = round((num_muni/as.numeric(urb_select_sumes[1]))*100,2),
    area_m2_p = round((area_m2/as.numeric(urb_select_sumes[2]))*100,2),
    area_ha_p = round((area_ha/as.numeric(urb_select_sumes[3]))*100,2)
  )

urb_select_final

urb_select_final$area_m2_p
