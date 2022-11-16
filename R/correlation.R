#' Calculates and Produces Correlation Plot of Genes
#'
#' A function that calculates the pairwise-correlation of genes, given a matrix
#' of gene expression in different samples, helping to identify correlated
#' genes. The function uses the `corrplot` package to create the plot.
#'
#'
#' @param expressionData A dataframe of gene expression data, with genes as
#'    rows and samples as columns.
#'
#' @return Returns a plot of pairwise correlations between genes
#'
#' @examples
#' # Using OVExpression dataset in package
#' correlationPlot(expressionData = OVExpression)
#'
#' @export
#' @import corrplot
#' @importFrom stats cor
#'
#' @references
#' Wei T, Simko V (2021). R package 'corrplot': Visualization of a Correlation
#' Matrix. (Version 0.92), <https://github.com/taiyun/corrplot>.

correlationPlot <- function(expressionData) {
  # correlation matrix of genes
  corrMat <- cor(t(expressionData))
  # correlation plot
  plot <- corrplot(corrMat, method = 'number', order = 'AOE')
}


# [END]
