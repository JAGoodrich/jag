#' @export
#' @title Analyze AW Sleep Data Basic
#'
#' @description
#' \code{analyze_AW_sleep_results_basic} runs the standard analyisis on results from the 
#' read in AW function. 
#'
#' @details Analizes sleep data
#'
#'
#' @importFrom stringr str_pad
#' @imports dplyr stringr hms activpalProcessing lubridate 
analyze_AW_sleep_results_basic <- function(data, cutoff_hrs_for_mutiple_sleep_periods) {
  
  data <- data %>%
    rename(original_file_name = sub,
           Group = group,
           ID = subject_id) %>%
    filter(type == "SLEEP", !is.na(Duration), !is.na(end_time)) %>%
    mutate(
      date_of_interval = (end_date) %>% lubridate::date(),
           day_of_week = weekdays(date_of_interval, abbreviate = TRUE) %>% as.factor,
           weekday_weekend = case_when(day_of_week == "Fri"  | day_of_week == "Sat" ~ "Weekend",
                                       TRUE ~ "Weekday") %>% as.factor) %>%
    droplevels %>% select(-type) 
  
  
  
  data$Duration <- data$Duration/60
  data <- data %>% 
    mutate(midpoint = (start + ((end - start)/2)) %>%  lubridate::as_datetime())
    
  
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
           end_time, midpoint, 
           day_of_week, weekday_weekend, Duration_hrs, 
           Duration_mins, Efficiency, everything())
  
  return(data)
}
