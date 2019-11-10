#' @export
#' @title Jesse ggplot theme
#'
#' @description
#' \code{jg_ggplot_theme} sets the ggplot theme
#'
#'
#' @imports ggplot2 ggthemes
jg_ggplot_theme <- function(rel.size, bold = FALSE){
  ggplot2::theme_set(theme_tufte())
  if(bold == FALSE){
  n <- rel.size
  theme_update(axis.line = element_line(size = n/4, colour = "black", linetype = "solid"),
               axis.line.y = element_line(size = n/4, colour = "black", linetype = "solid"),
               axis.line.x = element_line(size = n/4, colour = "black", linetype = "solid"),
               plot.title = element_text(size = rel(n)*1.2, face = "bold"),
               strip.text.x = element_text(size = rel(n)),
               axis.title.y = element_text(size = rel(n)*1.2, angle = 90),# face = "bold",  family = "Helvitica"),
               axis.title.x =  element_text(size = rel(n)*1.2, angle = 0), #face = "bold",  family = "Helvitica"),
               axis.text=element_text(size=rel(n)*1, color = "black"),  #family = "Helvitica"),
               legend.text = element_text(size = rel(n)*.75, angle = 0), #face = "bold",  family = "Helvitica"),
               legend.title = element_text(size = rel(n)*.9, angle = 0)) #face = "bold",  family = "Helvitica"))
  }
  else{
    n <- rel.size
    theme_update(axis.line = element_line(size = n/2, colour = "black", linetype = "solid"),
                 axis.line.y = element_line(size = n/2, colour = "black", linetype = "solid"),
                 axis.line.x = element_line(size = n/2, colour = "black", linetype = "solid"),
                 plot.title = element_text(size = rel(n)*1.2, face = "bold"),
                 strip.text.x = element_text(size = rel(n), face = "bold"),
                 axis.title.y = element_text(size = rel(n)*1.2, angle = 90, face = "bold"),
                 axis.title.x =  element_text(size = rel(n)*1.2, angle = 0 ,face = "bold"),
                 axis.text=element_text(size=rel(n)*1, color = "black", face = "bold"),  #family = "Helvitica"),
                 legend.text = element_text(size = rel(n)*.75, angle = 0,face = "bold"), #face = "bold",  family = "Helvitica"),
                 legend.title = element_text(size = rel(n)*.9, angle = 0, face = "bold")) #face = "bold",  family = "Helvitica"))
  }
}
