
#' Create a standard theme map from an sf object
#'
#' @param .data An sf object which has data broken down by local authority. This function only works for data in local authority breakdown only.
#' @param variable The column in the .data which has the numeric value to be mapped
#' @param LA_col The column which contains the Local Authority code
#' @param map_colour The colour which you want to represent the high value, the low value will always be white
#' @param year The year which the LA codes you are using relate to
#' @param countries The countries which you wish to appear on the map. These are E, E+W, GB and UK for England, England+Wales, Great Britain and United Kingdom respectively
#' @param save A TRUE/FALSE statement if you want the file to be exported as a png file. If TRUE, the filepath must be defined
#' @param filepath If save is TRUE, filepath is the save location of the png output must be in the form of quotations
#'
#' @return a ggplot map, or a png file if save is TRUE
#' @export
#'
#' @examples
#' rawdata <- read.csv("https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/1084731/LAHS_all_data_2020_2021_-_06_2022.csv")
#' df <- select(rawdata,organisation.id,a1a)
#' dluhc_LA_map(df,a1a,organisation.id,year = 2020,countries = "E",save = TRUE, filepath = "Council_Housing_Stock_map.png")
dluhc_LA_map <- function(.data,variable,LA_col,map_colour = "#012169",year = 2022,countries = "E",save = FALSE,filepath = NULL){

  if(countries %in% c("E","E+W","GB","UK")==FALSE){
    stop("The country variable you supplied is invalid. It must be one of:
         E, E+W, GB, UK. These represent England, England and Wales, Great Britain and United Kingdom respectively")
  }

  codes_match <-
    data.frame(
      country = c("E","E+W","GB","UK"),
      codes = c("E0","E0|W","E0|W|S","E0|W|S|N"),
      x_adjust = c(0.70,0.70,0.66,0.56),
      y_adjust = c(0.56,0.56,0.35,0.4),
      width = c(0.2,0.2,0.15,0.15),
      height = c(0.2,0.2,0.15,0.15)
    )

  map_match <-
    data.frame(
      year = c(2020,2021,2022),
      filenames = c("Local_Authority_Districts_(December_2020)_UK_BUC.shp","Local_Authority_Districts_(May_2021)_UK_BUC.shp","LAD_DEC_2022_UK_BUC.shp"),
      LA_Column = c("LAD20CD","LAD21CD","LAD22CD")
    )

  if(year>max(map_match$year)|year<min(map_match$year)){
    stop("The year you have selected is outside the range of maps available. Please contact the package owner to update the maps available")
  }



  library(cowplot)

  LA_map_data <- sf::st_read(paste0("github.com/communitiesuk/dluhctheme/raw/master/data/",map_match$filenames[which(map_match$year==year)]))



  LA_map_colname <- map_match$LA_Column[which(map_match$year==year)]

  LA_map_data <- LA_map_data %>%
    rename("LA_Code"=all_of(LA_map_colname)) %>%
    filter(str_detect(LA_Code,codes_match$codes[which(codes_match$country==countries)]))

  .data  <- .data %>%
    mutate(LA_Code = {{LA_col}}) %>%
    left_join(LA_map_data,., by = c("LA_Code"))

  map <-
    ggplot(.data) +
    geom_sf(aes(fill = {{variable}}),colour = "black",size=0.02) +
    theme_void() +
    scale_fill_gradient(low="#FFFFFF",high = map_colour) +
    theme(
      plot.title=element_blank(),
      legend.position = c(0.1,0.5),
      legend.title = element_text(color = map_colour, face = "bold", vjust = 0.8),
      plot.margin = margin(10, 0, 10, 0)) #add space around the plot


  map_London <- .data %>%
    mutate(Code_temp = substr(LA_Code, 1,3))%>%
    filter(Code_temp == "E09")

  limits = sf::st_bbox(map_London)
  limits_adj <- limits

  limits_adj[1] <- limits[1] - abs(0.03*(limits[3]-limits[1]))
  limits_adj[2] <- limits[2] - abs(0.03*(limits[4]-limits[2]))
  limits_adj[3] <- limits[3] + abs(0.03*(limits[3]-limits[1]))
  limits_adj[4] <- limits[4] + abs(0.03*(limits[4]-limits[2]))


  map <- map +

    geom_rect( xmin = limits_adj[[1]],
               ymin = limits_adj[[2]],
               xmax = limits_adj[[3]],
               ymax = limits_adj[[4]],
               fill=NA,
               colour = "black",
               size = 0.8)

  finalmap <- map %>%
    cowplot::ggdraw() +
    draw_plot(
      {
        map +
          coord_sf(   xlim = limits_adj[c(1,3)],
                      ylim = limits_adj[c(2,4)],
                      expand = FALSE) +
          theme(legend.position = "none",
                plot.title = element_blank())
      },
      x=codes_match$x_adjust[which(codes_match$country==countries)],
      y=codes_match$y_adjust[which(codes_match$country==countries)],
      width=codes_match$width[which(codes_match$country==countries)],
      height =codes_match$height[which(codes_match$country==countries)]
    )
  finalmap

  if(save){ggsave(finalmap,filename = filepath)}
}
