data_tibble <- function(data){
  var_name <- ls(data)
  class1 <- sapply(data,class)
  var_unique <- lengths(sapply(data, unique))
  var_na <- colSums(is.na(data))
  result <- tibble(var_name, class1, var_unique, var_na)
  return(result)
}
