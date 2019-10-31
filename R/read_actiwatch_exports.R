#' @export
#' @title Read in actiwatch sleep data
#'
#' @description
#' \code{read_actiwatch_exports} Reads in to R all of the data from a batch
#' export of actiwatch sleep data.
#'
#' @details
#' Path should be to a folder where all of
#' the exported actiwatch .csv files are located.
#'
#' @importFrom lubridate parse_date_time
#' @imports dplyr stringr hms testit
read_actiwatch_exports <- function(path) {
  #Make sure that path is correct format:
  if(stringr::str_sub(path, start = -1) != "/") {
    path <- paste(path, "/*.csv", sep = "")} else {
      path <- paste(path, "*.csv", sep = "")
    }
  #Find all .csv files in the folder:
  files <- Sys.glob(paths = path)
  
  data <- tibble() #initialize empty data frame
  #Read in all .csv files and merge:
  
  for (i in 1:length(files)) {
    temp <- readLines(files[i], n = 500)
    start <- grep("Statistics", temp, value = FALSE)
    end <- grep("Marker/Score List", temp, value = FALSE)
    
    try({temp <- temp[(start + 2):(end - 2)]
    temp <- str_split_fixed(temp, ",", n = Inf) %>%
      apply(X = ., MARGIN = 2, function(x)
        gsub("\"", x = x, replacement = ""))
    colnames(temp) <- temp[1, ]
    temp <- as_tibble(temp)
    NCOL(temp)
    temp <- temp[, 1:NCOL(temp)]
    
    temp <- temp %>% filter(
      `Interval Type` == "ACTIVE" |
        `Interval Type` == "DAILY" |
        `Interval Type` == "EXCLUDED" |
        `Interval Type` == "REST" |
        `Interval Type` == "SLEEP")
    temp[temp == NaN] <- NA
    temp[temp == ""] <- NA
    temp$sub <- as.character(sub(".*/ *(.*?) *.csv*", "\\1", files[i]))
    data <- bind_rows(data, temp)}) 
    rm(temp)
    
  }
  
  data <- data %>%
    rename(
      type = `Interval Type`,
      Interval = `Interval#`,
      off_wrist = `Off-Wrist`,
      pct_off_wrist = `%Off-Wrist`,
      pct_invalid_sw = `%Invalid SW`,
      wake_time = `Wake Time`,
      pct_wake = `%Wake`) %>%
    mutate(
      start = str_c(as.character(`Start Date`), as.character(`Start Time`), sep = " ") %>% lubridate::parse_date_time(., "%m/%d/%Y  %I:%M:%S %p"),
      end = str_c(as.character(`End Date`), as.character(`End Time`), sep = " ") %>% lubridate::parse_date_time(., "%m/%d/%Y  %I:%M:%S %p"),
      delta = end - start,
      group = case_when(
        grepl(pattern = "Cont", x = sub, ignore.case = TRUE) ~ "CONT",
        grepl(pattern = "FB", x = sub, ignore.case = TRUE) ~ "FB",
        grepl(pattern = "VB", x = sub, ignore.case = TRUE) ~ "VB",
        grepl(pattern = "GOLF", x = sub, ignore.case = TRUE) ~ "GOLF",
        grepl(pattern = "Ski", x = sub, ignore.case = TRUE) ~ "SKI",
        grepl(pattern = "Tenn", x = sub, ignore.case = TRUE) ~ "TENN",
        grepl(pattern = "MBB", x = sub, ignore.case = TRUE) ~ "MBB",
        grepl(pattern = "MBBT", x = sub, ignore.case = TRUE) ~ "BB_TRAINERS",
        grepl(pattern = "LAX", x = sub, ignore.case = TRUE) ~ "LAX",
        grepl(pattern = "SOC", x = sub, ignore.case = TRUE) ~ "SOC",
        grepl(pattern = "XC", x = sub, ignore.case = TRUE) ~ "XC",
        TRUE ~ NA_character_) %>% as.factor,
      sub_char_location = case_when(
        group == "CONT" ~ (str_locate(str_to_upper(sub), pattern = "CONT")[, 2] + 1),
        group == "FB" ~ (str_locate(str_to_upper(sub), pattern = "FB")[, 2] + 1),
        group == "VB" ~ (str_locate(str_to_upper(sub), pattern = "VB")[, 2] + 1),
        group == "GOLF" ~ (str_locate(str_to_upper(sub), pattern = "GOLF")[, 2] + 2),
        group == "SKI" ~ (str_locate(str_to_upper(sub), pattern = "SKI")[, 2] + 1),
        group == "TENN" ~ (str_locate(str_to_upper(sub), pattern = "TENN")[, 2] + 1),
        group == "MBB" ~ (str_locate(str_to_upper(sub), pattern = "MBB")[, 2] + 1),
        group == "BB_TRAINERS" ~ (str_locate(str_to_upper(sub), pattern = "BB_TRAINERS")[, 2] + 1),
        group == "LAX" ~ (str_locate(str_to_upper(sub), pattern = "LAX")[, 2] + 1),
        group == "SOC" ~ (str_locate(str_to_upper(sub), pattern = "SOC")[, 2] + 1),
        group == "XC" ~ (str_locate(str_to_upper(sub), pattern = "XC")[, 2] + 1),
        TRUE ~ NA_real_),
      subject_number = str_sub(sub, start = sub_char_location, end = sub_char_location + 1),
      subject_id = str_c(group, subject_number, sep = "") %>% as.factor,
      Duration = as.numeric(Duration),
      type = as.factor(type),
      start_date = as.Date(`Start Date`, format = "%m/%d/%Y"),
      start_time = as.hms(format(
        strptime(`Start Time`, "%I:%M:%S %p"), format = "%H:%M:%S"
      ), format = "h:m:s"),
      end_date = as.Date(`End Date`, format = "%m/%d/%Y"),
      end_time = as.hms(format(
        strptime(`End Time`, "%I:%M:%S %p"), format = "%H:%M:%S"
      ), format = "h:m:s")
    ) %>%
    select(-sub_char_location,
           -`Start Date`,
           -`End Date`,
           -`Start Time`,
           -`End Time`) %>%
    select(
      type,
      subject_id,
      group,
      Interval,
      start_date,
      start_time,
      end_date,
      end_time,
      start,
      end,
      everything()
    ) %>%
    mutate(
      Interval = as.numeric(Interval),
      off_wrist = as.numeric(off_wrist),
      pct_off_wrist = as.numeric(pct_off_wrist),
      pct_invalid_sw = as.numeric(pct_invalid_sw),
      Efficiency = as.numeric(Efficiency),
      wake_time = as.numeric(wake_time),
      pct_wake = as.numeric(pct_wake)
    )
  return(data)
  
}
