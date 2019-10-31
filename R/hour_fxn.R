#' @export
#' @imports modes
hour_fxn <- function(x){
  hours <- vector(mode = "numeric", length = length(x))
  for(i in 1:length(x)){
    hours[i]  <- return.hour(x[i])
  }
  if(modes::bimodality_coefficient(na.omit(hours),fig = FALSE) > 6/9) warning("Output likely bimodal; check input")
  # if(sum(3 * IQR(hours, na.rm = TRUE) + quantile(hours, probs = 0.75, na.rm = TRUE) < hours) != 0) {
  #   warning(paste("There are", sum(3 * IQR(hours, na.rm = TRUE) + quantile(hours, probs = 0.75, na.rm = TRUE) < hours),
  #                 "likely high outliers for this variable"))
  #   } #This warning was added 12/26, not yet tested
  # if(sum(quantile(hours, probs = 0.75, na.rm = TRUE) - 3 * IQR(hours, na.rm = TRUE) > hours) != 0) {
  #   warning(paste("There are", sum(quantile(hours, probs = 0.75, na.rm = TRUE) - 3 * IQR(hours, na.rm = TRUE) > hours),
  #                 "likely low outliers for this variable"))
  #           } #This warning was added 12/26, not yet tested
  return(hours)
}
