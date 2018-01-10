return.hour <- function(x){
  if(grepl(pattern = "-", x)) {return(strsplit(x, split = "-") %>%  unlist %>% as.numeric %>%  mean)}
  if(grepl(pattern = " ", x) & grepl(pattern = "/", x)) {
    temp <- strsplit(x, split = " ") %>%
      unlist %>%
      strsplit(x, split = "/") %>%
      unlist %>%
      as.numeric
    return(temp[1] + temp[2]/temp[3])}
  x <- gsub("[^0-9., ,:]", "", x)
  temp <- as.numeric(unlist(strsplit(x, split = ":")))
  if(is.na(temp[2])) hour <- temp[1] else hour <- temp[1] + temp[2]/60
  if(is.na(temp[1]) & !is.na(temp[2])) hour <- temp[2]/60
  return(hour)
}
