#' @export
bmi <- function(lbs, inches){
  kg = lbs/2.20462
  m = inches*0.0254
  bmi = kg/m^2
  return(bmi)
}