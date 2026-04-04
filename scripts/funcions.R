calcul_temp <- function(temp,rang){
  temp_max <- temp + rang
  temp_min <- temp - rang
  
  return(list(
    max = temp_max,
    min = temp_min
  ))
}