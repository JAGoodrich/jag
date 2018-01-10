#' @export
#' @title Function to be used by summaryBy
#'
#' @description
#' Convenience function for the \code{summaryBy} function from
#' doBy library.
#'
#' @details
#' Convenience function for the \code{summaryBy} function from
#' doBy library.
#'
#' @examples
#'
#' #Summary without sample size:
#' doBy::summaryBy(age ~ group, data = sleep,
#'                 FUN = avg_sd_fxn,
#'                 keep.names = TRUE)
#'
#' #To include sample size:
#' doBy::summaryBy(age ~ group, data = sleep,
#'                 FUN = function(x) {avg_sd_fxn(x, include.n = TRUE)},
#'                 keep.names = TRUE)
#'
avg_sd_fxn <- function(x, n.digits = 1, include.n = FALSE) {
  if(include.n == FALSE) {paste(round(mean(x, na.rm = T), n.digits),
                               round(sd(x, na.rm = T), n.digits), sep = " ± ") }
  else { paste(paste(round(mean(x, na.rm = T), n.digits),
             round(sd(x, na.rm = T), n.digits), sep = " ± "),
       paste(length(which(!is.na(x))), ")", sep = ""), sep = "   (") }

}
