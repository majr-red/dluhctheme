facet_barchart <- function(.data,xcol,ycol,groupcol,textsize=1){
.data <- mutate(.data,variable = {{groupcol}})

ggplot(data = .data,aes(x={{xcol}},y={{ycol}},fill=variable)) +
  geom_bar(stat="identity") +
  facet_wrap(~factor(variable), scales='fixed',strip.position="top") +
    dluhc_theme(size = textsize) +
  theme(legend.position = "none") +
  scale_fill_manual = c("#012169","blue","orange","purple","black","red","green")
}

