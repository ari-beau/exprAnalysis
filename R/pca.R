#' Create a PCA Plot
#'
#' A function that creates a PCA plot based on the expression data.
#'
#' @param expressionData A dataframe of gene expression data, with genes as
#'    rows and samples as columns.
#' @param sampleData A dataframe of sample information, with samples as rownames,
#'    and a column called "type" identifying if the sample is case or control.
#'
#' @return Plot
#'
#' @examples
#' # Example
#' exprPCA(OVExpression, OVSample)
#'
#' @export
#' @importFrom FactoMineR PCA
#' @importFrom factoextra fviz_pca_ind
#'
#' @references
#' Kassambara A, Mundt F (2020). _factoextra: Extract and Visualize the Results of Multivariate
#' Data Analyses_. R package version 1.0.7, <https://CRAN.R-project.org/package=factoextra>.
#'
#' Sebastien Le, Julie Josse, Francois Husson (2008). FactoMineR: An R Package
#' for Multivariate Analysis. Journal of Statistical Software, 25(1), 1-18.
# '10.18637/jss.v025.i01

exprPCA <- function(expressionData, sampleData) {
  # pca
  result <- FactoMineR::PCA(t(expressionData), graph = FALSE)

  plot <- factoextra::fviz_pca_ind(result,
               geom.ind = "point",
               col.ind = sampleData$type,
               palette = c("#FC4E07", "#00AFBB"),
               addEllipses = TRUE,
               ellipse.type = "confidence",
               legend.title = "Type")

  return(plot)
}


# [END]
