#' @export
#' 
#' 
#' 
create_norm_range <- function(minimum, maximum, n_points, width, offset) {
  ymin <- minimum
  ymax <- maximum
  n <- n_points 
  norm_range <- data.frame(tp = c(n + offset - width/2, n + offset + width/2), min = as.vector(ymin), max = as.vector(ymax))
  return(norm_range)
}