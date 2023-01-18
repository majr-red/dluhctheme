#' Create a single line time series graph in a dluhc theme
#'
#' @param .data A dataframe in long format with 2 columns necesarry: Date and value
#' @param datecol The column name which contains the date value in a widely used date format: "%Y-%m-%d","%d/%m/%Y","%d/%m/%y","%e/%m/%Y","%d-%b-%Y","%d-%b-%y"
#' @param ycol The column name which contains the values
#' @param dateformat is the format which the date is presented in using the standard R date format, see here for more detail https://www.statology.org/r-date-format/
#'
#' @return
#' @export
#'
#' @examples
#' df <- dplyr::filter(gapminder::gapminder,country=="China")
#' dluhc_time_series(.data=df,datecol = year, ycol = lifeExp, groupcol = country, dateformat = "%Y")

dluhc_time_series <- function(.data,datecol,ycol,dateformat="%Y-%m-%d"){
  library(tidyverse)
  is.convertible.to.date <- function(x) !is.na(as.Date(as.character(x), tz = 'UTC', format = dateformat))

  if(any(is.convertible.to.date(pull(.data,{{datecol}}))==FALSE)){
    stop("Date column not in format specified. Check the dateformat argument in the function")
  }

  .data <- .data %>%
    mutate(Date = as.Date({{datecol}},tryFormats = dateformat)) %>%
    mutate(value = {{ycol}})

  ggplot2::ggplot(data = .data,aes(x = Date,y = value)) +
    geom_line(size = 1.5, color = "#012169") +
    dluhctheme::dluhc_theme()
}
