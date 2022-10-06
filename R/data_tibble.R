#' The summary of variables by chart
#' @description This function performs a chart analysis of variables
#' @param data a data frame
#' @import tidyverse
#' @return a table with data variables and name/class/number of unique values/number of NA of variables
#' @export
#'
#' @examples
#' data_tibble(starwars)
#' data_tibble(billboard)
#'

data_tibble <- function(data){
  if (nrow(data) > 0){
    var_name <- ls(data)
    class1 <- sapply(data,class)
    var_unique <- lengths(sapply(data, unique))
    var_na <- colSums(is.na(data))
    result <- tibble(var_name, class1, var_unique, var_na)
  }else{
    result <- stop("Your data does not match requirement")
  }
  return(result)
}


#test it
#data_tibble(mtcars)
data_tibble(dplyr::starwars)
data_tibble(dplyr::band_members)
