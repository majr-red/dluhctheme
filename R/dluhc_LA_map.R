
#' Create a standard theme map from an sf object
#'
#' @param .data An sf object which has data broken down by local authority. This function only works for data in local authority breakdown only.
#' @param variable The column in the .data which has the numeric value to be mapped
#' @param LA_var The column which contains the Local Authority code
#' @param map_colour The colour which you want to represent the high value, the low value will always be white
#'
#' @return
#' @export
#'
#' @examples
dluhc_LA_map <- function(.data,variable,LA_var,map_colour = "#012169"){
  library(cowplot)
  map <-
    ggplot(.data) +
    geom_sf(aes(fill = {{variable}}),colour = "black",size=0.02) +
    theme_void() +
    scale_fill_gradient(low="#FFFFFF",high = map_colour) +
    theme(
      plot.title=element_blank(),
    legend.position = "bottom",
    legend.title = element_text(color = map_colour, face = "bold", vjust = 0.8),
    plot.margin = margin(10, 0, 10, 0)) #add space around the plot


    map_London <- .data %>%
      mutate(Code_temp = substr({{LA_var}}, 1,3))%>%
      filter(Code_temp == "E09")



    map <- map +

      geom_rect( xmin = sf::st_bbox(map_London)[[1]],
                 ymin = sf::st_bbox(map_London)[[2]],
                 xmax = sf::st_bbox(map_London)[[3]],
                 ymax = sf::st_bbox(map_London)[[4]],
                 fill=NA,
                 colour = "black",
                 size = 0.8)

    map <- map %>%
      cowplot::ggdraw() +
      draw_plot(
        {
          map +
            coord_sf(   xlim = sf::st_bbox(map_London)[c(1,3)],
                        ylim = sf::st_bbox(map_London)[c(2,4)],
                        expand = FALSE) +
            theme(legend.position = "none",
                  plot.title = element_blank())
        },
        x=0.70,
        y=0.56,
        width=0.20,
        height =0.20
      )
    map
}

  # a <- sf::st_read("Q:/GI_Data/Boundaries/Districts/2021/Local_Authority_Districts_(May_2021)_UK_BUC/Local_Authority_Districts_(May_2021)_UK_BUC.shp") %>%
  #   filter(!str_detect(LAD21CD,"S")&!str_detect(LAD21CD,"N")) %>%
  #   rename("Length of Shape"=Shape__Len)
  #
  # dluhc_LA_map(a,`Length of Shape`,LAD21CD) +
  #   theme(legend.title = element_text("R"))
