#' Una función que limpia una cadena de caracteres, removiendo puntuación y números y tokenizándola
#'
#' @param str Una cadena de caracteres de entrada tal como "This is a cool function!"
#' @return Un vector que contiene todos los tokens válidos de la cadena de caracteres de entrada original
#' @export
Clean_String <- function(str){
  # minúsculas
  temp <- tolower(str)
  # Remueve todo lo que no sea una letra
  temp <- stringr::str_replace_all(temp,"[^a-zA-Z\\s]", " ")
  # Reduce todo a un solo espacio en blanco
  temp <- stringr::str_replace_all(temp,"[\\s]+", " ")
  # Divide
  temp <- stringr::str_split(temp, " ")[[1]]
  # Se deshace de los últimos "", si fuera necesario
  indexes <- which(temp == "")
  if(length(indexes) > 0){
    temp <- temp[-indexes]
  }
  return(temp)
}
