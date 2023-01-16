#' Create a single line time series graph in a dluhc theme for a line which has a predicted value
#'
#' @param .data A dataframe in long format with 2 columns necesarry: Date and value
#' @param xcol The column name which contains the date value in a widely used date format: "%Y-%m-%d","%d/%m/%Y","%d/%m/%y","%e/%m/%Y","%d-%b-%Y","%d-%b-%y"
#' @param ycol The column name which contains the values
#' @param cutdate The date which the predicted values begin from
#' @param dottedline A TRUE/FALSE statement to decide if you want a vertical dotted line on the graph to split the prediction and the recorded values
#' @param label_names A vector containing the 2 words you want as the label for your lines, the default is c("Predicted", "Actual")
#'
#' @return
#' @export
#'
#' @examples
dluhc_actual_predict <- function(.data,xcol,ycol,cutdate,dottedline=TRUE,label_names = c("Predicted","Actual")){
  library(tidyverse)
  cutdate = as.Date(cutdate,tryFormats = c("%Y-%m-%d","%d/%m/%Y","%d/%m/%y","%e/%m/%Y","%d-%b-%Y","%d-%b-%y"))

.data <- .data %>%
    mutate(Date = as.Date({{xcol}},tryFormats = c("%Y-%m-%d","%d/%m/%Y","%d/%m/%y","%e/%m/%Y","%d-%b-%Y","%d-%b-%y"))) %>%
    mutate(value = {{ycol}}) %>%
  mutate(prediction = ifelse(Date>=cutdate,label_names[1],label_names[2]))

duplicate <- .data %>%
  mutate(check = ifelse(prediction!=lead(prediction),1,0)) %>%
  filter(check == 1) %>%
  mutate(prediction = label_names[1]) %>%
  select(!check)

.data <- rbind(.data,duplicate)


  a <- ggplot2::ggplot(data = .data,aes(x = Date,y = value)) +
    geom_line(aes(linetype = factor(prediction,levels = c(label_names[1],label_names[2]))),size = 1.5, color = "#012169") +
    dluhctheme::dluhc_theme() +
    scale_linetype_manual(values = c("dashed","solid")) +
    theme(legend.key.width = unit(2,"cm"))

  if(dottedline){
    a <- a+
      geom_vline(xintercept = cutdate,linetype="dotted",size=1)
  }
  a
}
