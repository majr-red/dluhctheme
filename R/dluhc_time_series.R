#' Create a single line time series graph in a dluhc theme
#'
#' @param .data A dataframe in long format with 2 columns necesarry: Date and value
#' @param xcol The column name which contains the date value in a widely used date format: "%Y-%m-%d","%d/%m/%Y","%d/%m/%y","%e/%m/%Y","%d-%b-%Y","%d-%b-%y"
#' @param ycol The column name which contains the values
#'
#' @return
#' @export
#'
#' @examples
dluhc_time_series <- function(.data,xcol,ycol){
  .data <- .data %>%
    mutate(Date = as.Date({{xcol}},tryFormats = c("%Y-%m-%d","%d/%m/%Y","%d/%m/%y","%e/%m/%Y","%d-%b-%Y","%d-%b-%y"))) %>%
    mutate(value = {{ycol}})

  ggplot2::ggplot(data = .data,aes(x = Date,y = value)) +
    geom_line(size = 1.5, color = "#012169") +
    dluhctheme::dluhc_theme()
}
