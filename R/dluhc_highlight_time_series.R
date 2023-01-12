dluhc_highlighted_time_series <-
  function(.data,xcol,ycol,groupcol,size = 1){
    library(gghighlight)
    .data <- mutate(.data,GROUP = {{groupncol}})
  ggplot(.data, mapping=aes(x={{xcol}}, y={{ycol}})) +
    geom_line(show.legend = FALSE, col = "#012169" , size = 1.5) +
    geom_line(aes({{xcol}},{{ycol}},colour=GROUP), colour = "#012169")+
    facet_wrap(~ GROUP, scales='free',strip.position="top") +
    gghighlight(use_direct_label = FALSE) +
    dluhc_theme(size = size)
  }
