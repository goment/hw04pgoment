#' The summary of variables by plot
#' @description This function performs a plot analysis of variables
#' @param data a data frame
#' @param data_class a variable class
#' @import ggplot2
#' @importFrom tidyr gather
#' @importFrom purrr keep
#' @importFrom rlang .data
#' @return a plot
#' @export
#'
#' @examples
#' data_plot(mtcars, "double")
#'
data_plot <- function(data, data_class){
  if (nrow(data) > 0 &
      data_class == "character" | data_class == "double" |
      data_class == "factor" | data_class == "logical" |
      data_class == "intiger" | data_class == "numeric"){

    data_type <- sapply(data, typeof)
    input_same_count = 0
    input_not_same = 0

    for (i in data_type){
      if (i == data_class){
        input_same_count = input_same_count + 1
        #one or more
        if (input_same_count > 0){
          if(data_class == "character" | data_class == "factor" | data_class == "logical"){
            data %>%
              #keep(is.factor)%>%
              gather() %>%
              ggplot(aes(x=value)) +
              facet_wrap( ~ key, scales = "free") +
              ggtitle(label = deparse(substitute(data)),
                      subtitle = deparse(substitute(data_class)))-> plot
            result <- plot + geom_bar()
          }else{
            data %>%
              keep(is.numeric) %>%
              gather() %>%
              ggplot(aes(x=value)) +
              facet_wrap(~ key, scales = "free") +
              ggtitle(label = deparse(substitute(data)),
                      subtitle = deparse(substitute(data_class)))-> plot
            result <- plot + geom_histogram()
          }
        }#none
        else{
          result <- stop("data has no variables of class character.")
        }
      }else{
        input_not_same = input_not_same + 1
        if(input_not_same == ncol(data)){
          result <- stop("data has no variables of class character.")
        }
      }
    }
  }else{
    result <- stop("Your data does not match requirement")
  }
  return(result)
}
