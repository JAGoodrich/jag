#' @import ggplot2
#' @import ggthemes
#' @export
jg_ggplot_theme <- function(rel.size){
  ggplot2::theme_set(theme_tufte())
  n <- rel.size
  theme_update(axis.line = element_line(size = n/4, colour = "black", linetype = "solid"),
               axis.line.y = element_line(size = n/4, colour = "black", linetype = "solid"),
               axis.line.x = element_line(size = n/4, colour = "black", linetype = "solid"),
               axis.title.y = element_text(size = rel(n)*1.2, angle = 90),# face = "bold",  family = "Helvitica"),
               axis.title.x =  element_text(size = rel(n)*1.2, angle = 0), #face = "bold",  family = "Helvitica"),
               axis.text=element_text(size=rel(n)*1, color = "black"),  #family = "Helvitica"),
               legend.text = element_text(size = rel(n)*.75, angle = 0), #face = "bold",  family = "Helvitica"),
               legend.title = element_text(size = rel(n)*.9, angle = 0)) #face = "bold",  family = "Helvitica"))
}
