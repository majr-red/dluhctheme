#' Create a 2 line time series graph in a DLUHC style
#'
#' @param .data A dataframe with the data in a long format with one column for the date and one column for the variable value
#' @param xcol The column name of the dataframe which contains the date variable
#' @param ycol The column name of the dataframe which contains the value
#' @param groupcol The column name which contains the grouping variable (such as country, region or type)
#'
#' @return A ggplot2 object
#' @export
#'
#' @examples
dluhc_2var_time_series <- function(.data,xcol,ycol,groupcol){
  library(tidyverse)
  .data <- .data %>%
    mutate(Date = as.Date({{xcol}},tryFormats = c("%Y-%m-%d","%d/%m/%Y","%d/%m/%y","%e/%m/%Y","%d-%b-%Y","%d-%b-%y"))) %>%
    mutate(value = {{ycol}}) %>%
    mutate(variable = {{groupcol}})

  .data %>%
    ggplot(aes(x = Date,y = value)) +
    geom_line(size = 1.5, aes(color = variable)) +
    scale_color_manual(values=c("#012169","#6b98fe")) +
    theme_classic(base_size = 18) +
    scale_y_continuous(expand = c(0,0)) +
    dluhctheme::dluhc_theme()
}
