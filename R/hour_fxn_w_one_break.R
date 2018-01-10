#' @export
hour_fxn_w_one_break <- function(x, cut){
  hours <- vector(mode = "numeric", length = length(x))
  for(i in 1:length(x)){
    hours[i]  <- return.hour(x[i])
  }
  hours <- ifelse(hours < cut, hours + 12, hours)
  hours <- ifelse(hours == 0, NA, hours)
  return(hours)
}
