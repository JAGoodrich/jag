#' @export
naptime_fxn <- function(x){
  x <- gsub("1/2", ".5", x)
  x <- gsub("[^0-9, :, .]", "", x)
  x <- ifelse(x == 0, NA, x)
  times <- vector(mode = "numeric", length = length(x))
  for(i in 1:length(x)){
    if(grepl(pattern = ":", x = x[i])){
      times[i]  <- return.hour(x[i])
    }
    else times[i]  <- as.numeric(x[i])
  }
  times <- ifelse(times > 10, times/60, times)
  times <- ifelse(times == 0, NA , times)
  return(times)
}
