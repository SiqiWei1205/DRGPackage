#' Boxplot of DRG
#'
#'This function produces a boxplot to show \code{payment} of different DRG definition number
#'
#' @param payment average money
#'
#' @return a boxplot to show \code{payment} of different DRG definition number
#' @export
#'
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_boxplot
#'
#' @examples
#'DRG_box("Average.Covered.Charges")
#'
DRG_box <-
  function(payment = c("Average.Covered.Charges",
                       "Average.Total.Payments",
                       "Average.Medicare.Payments")) {
    DRG.No <- substring(DRG$DRG.Definition, 1, 3)
    DRG %>% group_by(DRG.Definition) %>% summarize(DRG.No)
    ggplot(DRG, aes(x = factor(DRG.No), y = get(payment))) +
      geom_boxplot(fill = "lightblue") +
      coord_trans(y = "log10") +
      theme(axis.text.x = element_text(
        angle = 90,
        hjust = 1,
        size = 5
      )) +
      xlab("DRG definition number") +
      ylab(payment) +
      ggtitle(paste("Boxplot of", payment))
  }


