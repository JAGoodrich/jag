#' @export
#' @title Analyze AW Sleep Data
#'
#' @description
#' \code{analyze_AW_sleep_results} runs the standard analyisis on results from the 
#' read in AW function. 
#'
#' @details Analizes sleep data, classifies naps vs. regular sleep periods, and 
#' calls friday night and saturday night sleep periods "WEEKEND SLEEP"
#'
#'
#' @importFrom stringr str_pad
#' @imports dplyr stringr hms activpalProcessing lubridate 
analyze_AW_sleep_results <- function(data, cutoff_hrs_for_mutiple_sleep_periods) {
  if(is.null(cutoff_hrs_for_mutiple_sleep_periods)) cutoff_hrs_for_mutiple_sleep_periods = 2
  
  data <- data %>%
    rename(original_file_name = sub,
           Group = group,
           ID = subject_id) %>%
    filter(type == "SLEEP", !is.na(Duration), !is.na(end_time)) %>%
    mutate(date_of_interval = (end_date - 1) %>% lubridate::date(),
           day_of_week = weekdays(date_of_interval, abbreviate = TRUE) %>% as.factor,
           weekday_weekend = case_when(day_of_week == "Fri"  | day_of_week == "Sat" ~ "Weekend",
                                       TRUE ~ "Weekday") %>% as.factor) %>%
    droplevels %>% select(-type) 
  
  # Determine Naps ----------------------------------------------------------
  
  naps <- data.frame(data$Duration, as.numeric(data$end_time))
  naps <- as.matrix(naps)
  naps <- scale(naps)
  
  fit <- kmeans(naps[,1:2], centers = 2, nstart = 10)
  #aggregate(naps,by=list(fit$cluster),FUN=mean)
  naps <- data.frame(naps, as.vector(fit[["cluster"]]))
  naps <- naps %>% rename(Duration = data.Duration,
                          end_time = as.numeric.data.end_time.,
                          cluster = as.vector.fit...cluster....)
  reg_sleep_period <- ifelse(table(naps$cluster)[1] < table(naps$cluster)[2], 2, 1 )
  data <- data %>%
    mutate(interval_type = case_when(naps$cluster ==  reg_sleep_period ~ "Not a nap",
                                     TRUE ~ "Nap") %>% as.factor)
  
  plot(data$end_time/60/60, data$Duration/60, col = as.factor(naps$cluster), xlab = "Wake Time", ylab = "Duration (hrs)")
  
  # New section -------------------------------------------------------------
  data <- data %>% 
    # filter(interval_type == "Not a nap") %>% 
    arrange(ID, start) %>% 
    group_by(ID) %>% 
    mutate(next_sleep = abs(lead(start) - end), 
           previous_sleep = abs(lag(end) - start), 
           closest_sleep = pmin(next_sleep, previous_sleep, 86400, na.rm = TRUE)) %>% 
    ungroup() %>% 
    mutate(multiple_sleep_periods = if_else(closest_sleep < 60*60*cutoff_hrs_for_mutiple_sleep_periods,
                                            "Check", "OK")) %>% #select periods with less than 6 hours between them
    select(-next_sleep, -previous_sleep)
  
  # Summarize Sleep ---------------------------------------------------------------
  
  data$Duration <- data$Duration/60
  data$midpoint <- with(data,
                        ifelse(interval_type == "Not a nap",
                               (start + ((end - start)/2)), NA_real_)) %>%
    lubridate::as_datetime()
  data$midpoint <- hms::hms(seconds = lubridate::second(data$midpoint),
                            minutes = lubridate::minute(data$midpoint),
                            hours = lubridate::hour(data$midpoint))
  data <- data %>%
    rename(Date = date_of_interval,
           Duration_hrs = Duration,
           Duration_mins = delta)
  
  data <- data %>% 
    select(Group, 
           ID, Interval, Date, start, 
           start_date, start_time, end, end_date,
           end_time, midpoint, interval_type, 
           day_of_week, weekday_weekend, Duration_hrs, 
           Duration_mins, Efficiency, everything())
  
  return(data)
}
