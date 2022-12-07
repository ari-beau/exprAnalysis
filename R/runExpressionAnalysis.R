#' Launch Shiny App for expressionAnalysis
#'
#' A function that launches the shiny app for the expressionAnalysis package.
#' The shiny app would load gene expression and sample data from files, and can
#' rank differentially expressed genes in cases and controls and produce
#' boxplots to show the visualize this.
#'
#' @return No return value but opens up a Shiny page.
#'
#' @examples
#' \dontrun{
#'
#' expressionAnalysis::runExpressionAnalysis()
#' }
#'
#' @references
#' Chang W, Cheng J, Allaire J, Sievert C, Schloerke B, Xie Y, Allen J, McPherson J, Dipert A,
#' Borges B (2022). _shiny: Web Application Framework for R_. R package version 1.7.3,
#' <https://CRAN.R-project.org/package=shiny>.
#'
#' @export
#' @importFrom shiny runApp


runExpressionAnalysis <- function() {
  appDir <- system.file("shiny-scripts",
                        package = "expressionAnalysis")
  actionShiny <- shiny::runApp(appDir, display.mode = "normal")
  return(actionShiny)
}


# [END]

