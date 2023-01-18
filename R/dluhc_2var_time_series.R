#' Create a 2 line time series graph in a DLUHC style
#'
#' @param .data A dataframe with the data in a long format with one column for the date and one column for the variable value
#' @param datecol The column name of the dataframe which contains the date variable
#' @param ycol The column name of the dataframe which contains the value
#' @param groupcol The column name which contains the grouping variable (such as country, region or type)
#'
#' @return A ggplot2 object
#' @export
#'
#' @examples
#' df <- dplyr::filter(gapminder::gapminder,country=="Australia"|country=="China")
#' dluhc_2var_time_series(.data=df,datecol = year, ycol = lifeExp, groupcol = country, dateformat = "%Y")

dluhc_2var_time_series <- function(.data,datecol,ycol,groupcol,dateformat = "%Y-%m-%d"){
  library(tidyverse)

  is.convertible.to.date <- function(x) !is.na(as.Date(as.character(x), tz = 'UTC', format = dateformat))

  if(any(is.convertible.to.date(pull(.data,{{datecol}}))==FALSE)){
    stop("Date column not in format specified. Check the dateformat argument in the function")
    }

  if(any(replace_na(is.numeric(pull(.data,{{ycol}})),TRUE)==FALSE)){
    stop("Value column contains non-numeric values. Check your data and try again")
  }


  .data <- .data %>%
    mutate(Date = as.Date(as.character({{datecol}}),tryFormats = dateformat)) %>%
    mutate(value = {{ycol}}) %>%
    mutate(variable = {{groupcol}})

  .data %>%
    ggplot(aes(x = Date,y = value)) +
    geom_line(size = 1, aes(color = variable)) +
    scale_color_manual(values=c("#012169","#6b98fe")) +
    theme_classic(base_size = 18) +
    scale_y_continuous(expand = c(0,0)) +
    dluhctheme::dluhc_theme()
}

