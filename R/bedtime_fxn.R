#' @export
#' @import modes
bedtime_fxn <- function(x){
  hours <- vector(mode = "numeric", length = length(x))
  for(i in 1:length(x)){
    hours[i]  <- return.hour(x[i])
  }
  temp <- as.data.frame(hours)
  solution <- kmeans(na.omit(hours), centers = 3)

  temp$cluster[which(!is.na(temp$hours))] <- unlist(solution[1])
  centroids <- unlist(solution[2])
  temp$group <- case_when(temp$cluster == which(centroids == min(centroids)) ~ "24hr_am",
                          temp$cluster == which(centroids == max(centroids)) ~ "24hr_pm",
                          TRUE ~ "12hr") %>% as.factor

  times <- ifelse(temp$group == "24hr_am", temp$hours + 24,
                  ifelse(temp$group == "12hr", temp$hours + 12, temp$hours))
  if(modes::bimodality_coefficient(na.omit(times),fig = FALSE) > 5/9) stop("Output likely bimodal; check input")
  return(times)
}

# #Needs Work
# bedtime_fxn_w_two_pred <- function(x, y){
#   hours <- vector(mode = "numeric", length = length(x))
#   for(i in 1:length(x)){
#     hours[i]  <- return.hour(x[i])
#   }
#   temp <- as.data.frame(hours)
#   solution <- kmeans(c(na.omit(hours), na.omit(y)), centers = 3)
#
#   temp$cluster[which(!is.na(temp$hours))] <- unlist(solution[1])
#   centroids <- unlist(solution[2])
#   temp$group <- as.factor(ifelse(temp$cluster == which(centroids == min(centroids)), "24hr_am",
#                             ifelse(temp$cluster == which(centroids == max(centroids)), "24hr_pm", "12hr")))
#   times <- ifelse(temp$group == "24hr_am", temp$hours + 24,
#                   ifelse(temp$group == "12hr", temp$hours + 12, temp$hours))
#   return(times)
# }
