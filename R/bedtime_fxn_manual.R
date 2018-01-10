#' @export
#' @import modes
bedtime_fxn_manual <- function(x, low_cut, high_cut){
  hours <- vector(mode = "numeric", length = length(x))
  for(i in 1:length(x)){
    hours[i]  <- return.hour(x[i])
  }
  temp <- as.data.frame(hours)
  temp$group <- as.factor(ifelse(temp$hours < low_cut, "24hr_am",
                                 ifelse(temp$hours > high_cut, "24hr_pm", "12hr")))

  times <- ifelse(temp$group == "24hr_am", temp$hours + 24,
                  ifelse(temp$group == "12hr", temp$hours + 12, temp$hours))
  if(modes::bimodality_coefficient(na.omit(times),fig = FALSE) > 5/9) stop("Output likely bimodal; check input")
  return(times)
}
