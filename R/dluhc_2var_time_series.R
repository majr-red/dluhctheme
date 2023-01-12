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
