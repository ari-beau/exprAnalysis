#' Rank Differentially Expressed Genes
#'
#' Identify and rank differentially expressed genes using expression data to
#' identify potential biomarkers.
#'
#' @param expressionData A dataframe of gene expression data, with genes as
#'    rows and samples as columns.
#' @param sampleData A dataframe of sample information, with two columns; one
#'    for samples and a second identifying if the sample is case or control.
#' @param case A string representing how samples are identified as cases in
#'    sampleData.
#' @param control A string representing how samples are identified as controls
#'    in sampleData.
#' @param method A parameter to specify the method used to calculate
#'    differential expression. The possible parameters are:
#' \itemize{
#'   \item "t" - t-test (default)
#'   \item "wilcoxon" - Wilcoxon rank sum test
#' }
#'
#' @return Returns a dataframe with genes and P-values, in increasing order.
#'
#' @examples
#' # Using OVExpression and OVSample datasets in package
#'
#' # Example 1:
#' # Using default method t-test
#'
#' genesTTest <- rankDEG(expressionData = OVExpression,
#'                       sampleData = OVSample,
#'                       case = "Ovarian cancer",
#'                       control = "Normal")
#' head(genesTTest)
#'
#'
#' # Example 2:
#' # Using Wilcoxon rank sum test
#' genesWilcoxon <- rankDEG(expressionData = OVExpression,
#'                       sampleData = OVSample,
#'                       case = "Ovarian cancer",
#'                       control = "Normal",
#'                       method = "wilcoxon")
#' head(genesWilcoxon)
#'
#' @export
#' @importFrom stats t.test wilcox.test

rankDEG <- function(expressionData,
                    sampleData,
                    case,
                    control,
                    method = "t") {


  # two vectors of case and control samples
  cases <- row.names(subset(sampleData, type == case))
  controls <- row.names(subset(sampleData, type == control))

  # vector of genes
  genes <- row.names(expressionData)

  # prepare vector for results of t-test
  results <- numeric(length = length(genes))

  for (i in seq_along(genes)){
    caseExpr <- expressionData[genes[i], cases]
    controlExpr <- expressionData[genes[i], controls]
    if (method == "t"){
      result <- t.test(as.numeric(caseExpr), as.numeric(controlExpr))
    } else if (method == "wilcoxon"){
      result <- wilcox.test(as.numeric(caseExpr), as.numeric(controlExpr))
    } else {
      stop("Invalid input to method. Valid inputs for method are \"t\" or \"wilcoxon\".")
    }
    results[i] <- result$p.value
  }

  resultsDF <- data.frame(Gene = genes, Pval = results)
  resultsDF <- resultsDF[order(resultsDF$Pval), ]
  rownames(resultsDF) <- seq(length=nrow(resultsDF))
  return(resultsDF)

}



#' Plot Differential Gene Expression of Case vs Control
#'
#' Produces boxplots of gene expression data for specified genes. The function
#' uses the `melt` function from the `reshape` package to manipulate data, and
#' the `ggplot2` package to create the plot.
#'
#' @param expressionData A dataframe of gene expression data, with genes as
#'    rows and samples as columns.
#' @param sampleData A dataframe of sample information, with two columns; one
#'    for samples and a second identifying if the sample is case or control.
#' @param genes A string or character vector of genes to include in the plot.
#'    This argument is optional; if not included, all genes will be included
#'    in the plot. If input is not a string or character vector, all genes will
#'    be included in the plot.
#'
#'
#' @return Returns a dataframe with genes sorted
#'
#' @examples
#' # Using OVExpression and OVSample datasets in package
#'
#' # Example 1:
#' # Exclude optional genes argument
#'
#' # all genes included
#' exprPlot(expressionData = OVExpression,
#'         sampleData = OVSample)
#'
#'
#' # Example 2:
#' # Include genes argument
#'
#' # Only genes PAX8 and DDR1 are included
#' exprPlot(expressionData = OVExpression,
#'         sampleData = OVSample,
#'         genes = c("PAX8", "DDR1"))
#'
#' @export
#' @importFrom reshape melt
#' @import ggplot2
#'
#' @references
#' Wickham H (2016). ggplot2: Elegant Graphics for Data Analysis.
#' Springer-Verlag New York. ISBN 978-3-319-24277-4, <https://ggplot2.tidyverse.org>.
#'
#' Wickham H (2007). “Reshaping data with the reshape package.” Journal of
#' Statistical Software, 21(12). <https://www.jstatsoft.org/v21/i12/>.

exprPlot <- function(expressionData,
                     sampleData,
                     genes = NULL) {

  if (is.character(genes)){
    expressionData <- expressionData[rownames(expressionData) %in% genes, ]
  } else {
    ; # does nothing
  }

  # create merged dataframe with both expression data and sample data
  allData <- merge(t(expressionData), sampleData, by = "row.names")

  row.names(allData) <- allData[, 1]
  allData <- allData[, -1]

  # melt data to prepare for plot
  meltedData <- reshape::melt(allData)

  # boxplot of data
  plot <- ggplot2::ggplot(meltedData, aes(x = variable, y = value, col = type)) +
    geom_boxplot() +
    labs(x = "Gene",  y = "Expression Level") +
    ggtitle("Boxplot of gene expression") +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())

  return(plot)

}


# [END]
