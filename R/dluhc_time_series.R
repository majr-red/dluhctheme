#' Add dluhc theme to ggplot chart
#'
#' This function creates a times series graph on a standard format based on receiving data where the date code is in the format
dluhc_time_series <- function(.data,xcol,ycol){
  .data <- .data %>%
    mutate(Date = as.Date({{xcol}},tryFormats = c("%Y-%m-%d","%d/%m/%Y","%d/%m/%y","%e/%m/%Y","%d-%b-%Y","%d-%b-%y"))) %>%
    mutate(value = {{ycol}})

  ggplot2::ggplot(data = .data,aes(x = Date,y = value)) +
    geom_line(size = 1.5, color = "#012169") +
    dluhctheme::dluhc_theme()
}
