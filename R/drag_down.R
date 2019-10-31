#' @export
#' @title Drag down ID variables when only the 
#' first row of subjects data is identified.
#'
#' @description
#' \code{drag_down} returns data frame with filled in ID variable.
#'
#' @details
#' Expand here
#'
#' @examples
#' drag_down(data, "variable")
#'
drag_down <- function(data, name){
  temp <- data[[name]]
  for(i in 1:length(temp)){
    if(is.na(temp[i])) {temp[i] = temp[i-1]}
  }
  data[[name]] <- temp
  return(data)
}