#' Statistics of DRG
#'
#'This function calculates statistics over all of the DRG codes for average Medicare payments
#'
#' @param method statistics
#'
#' @return to show \code{method} over all of the DRG codes for average Medicare payments
#' @export
#'
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_boxplot
#' @importFrom dplyr summarize
#' @importFrom dplyr group_by
#'
#' @examples
#' DRG_stat("mean")
#'
DRG_stat <- function(method = c("mean", "median", "standard deviation")) {
  DRG <- DRG %>% group_by(DRG.Definition)
  func <- get(method)
  DRG %>% summarise(method = func(Average.Medicare.Payments))
}
