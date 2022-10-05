data_tibble <- function(data){
  if (ncol(data) > 5 & nrow(data) > 9 & length(unique(sapply(data,class)))){
    var_name <- ls(data)
    class1 <- sapply(data,class)
    var_unique <- lengths(sapply(data, unique))
    var_na <- colSums(is.na(data))
    result <- tibble(var_name, class1, var_unique, var_na)
  }else{
    result <- "Stop, your data does not match requirement."
  }
  return(result)
}
