#' Calculates and Produces Correlation Plot of Genes
#'
#' A function that calculates the pairwise-correlation of genes, given a matrix
#' of gene expression in different samples, helping to identify correlated
#' genes. The function uses the `corrplot` package to create the plot.
#'
#'
#' @param expressionData A dataframe of gene expression data, with genes as
#'    rows and samples as columns.
#' @param method A parameter to specify correlation coefficient to compute.
#'    The options are:
#' \itemize{
#'   \item "spearman" (default)
#'   \item "pearson"
#'   \item "kendall"
#' }
#'
#' @return Returns a plot of pairwise correlations between genes
#'
#' @examples
#' # Using OVExpression dataset in package
#' # Example 1:
#' # Using default correlation coefficient "spearman"
#' correlationPlot(expressionData = OVExpression)
#'
#' # Example 2:
#' # Using pearson as method
#' correlationPlot(expressionData = OVExpression, method = "pearson")
#'
#' @export
#' @import corrplot
#' @importFrom stats cor
#' @importFrom assertthat assert_that
#'
#' @references
#' Wei T, Simko V (2021). R package 'corrplot': Visualization of a Correlation
#' Matrix. (Version 0.92), <https://github.com/taiyun/corrplot>.
#'
#' Wickham H (2019). _assertthat: Easy Pre and Post Assertions_. R package version 0.2.1,
#' <https://CRAN.R-project.org/package=assertthat>.

correlationPlot <- function(expressionData, method = "spearman") {
  # check if method input is valid
  assertthat::assert_that(method == "spearman" | method == "pearson" | method == "kendall",
                          msg = "Invalid input to method. Valid inputs for method are \"spearman\", \"pearson\" or \"kendall\".")


  # correlation matrix of genes
  corrMat <- stats::cor(t(expressionData), method = method)
  # correlation plot
  plot <- corrplot::corrplot(corrMat, method = 'number', order = 'AOE')

  return(invisible(NULL))
}


# [END]
