#' Highlighted faceted graphs in the dluhc style
#'
#' @param .data The dataframe used for the analysis, ideally a 3 column dataframe
#' @param xcol The column name which has the date variables in. Date needs to be in one of three formats: "2021-01-23", "23/01/2021" or "21-Jan-2021"
#' @param ycol The column name which has the numeric values, ensure these are in numeric format
#' @param groupcol The column name which will split the data into individual graphs
#' @param textsize A numeric variable for text size, this isnt the actual text size, but will scale the text size in the facets correctly
#'
#' @return
#' @export
#'
#' @examples
dluhc_highlighted_time_series <-
  function(.data,xcol,ycol,groupcol,textsize = 1){
    library(gghighlight)
    .data <- mutate(.data,GROUP = {{groupcol}})
  ggplot(.data, mapping=aes(x={{xcol}}, y={{ycol}})) +
    geom_line(show.legend = FALSE, col = "#012169" , size = 1.5) +
    geom_line(aes({{xcol}},{{ycol}},colour=GROUP), colour = "#012169")+
    facet_wrap(~ GROUP, scales='free',strip.position="top") +
    gghighlight(use_direct_label = FALSE) +
    dluhc_theme(size = textsize)
  }
