#' Rank Differentially Expressed Genes
#'
#' Identify and rank differentially expressed genes using expression data to
#' identify potential biomarkers.
#'
#' @param expressionData A dataframe of numeric gene expression data, with genes as
#'    rows and samples as columns.
#' @param sampleData A dataframe of sample information, with samples as rownames,
#'    and a column called "type" identifying if the sample is case or control.
#' @param case A string representing how samples are identified as cases in
#'    sampleData (case sensitive).
#' @param control A string representing how samples are identified as controls
#'    in sampleData (case sensitive).
#' @param method A parameter to specify the method used to calculate
#'    differential expression. The possible parameters are:
#' \itemize{
#'   \item "t" - t-test (default)
#'   \item "wilcoxon" - Wilcoxon rank sum test
#' }
#' @param siglevel A number to specify the significance level of the test. The
#'    default is 0.05. This is used to determine potential biomarkers.
#'
#' @return Returns a dataframe with genes, test statistic, P-values, and if
#' there is expression is significantly different, ordered by increasing P-values.
#'
#' @examples
#' # Using OVExpression and OVSample datasets in package
#'
#' # Example 1:
#' # Using default method t-test and default significance value
#'
#' genesTTest <- rankDEG(expressionData = OVExpression,
#'                       sampleData = OVSample,
#'                       case = "Ovarian cancer",
#'                       control = "Normal")
#' head(genesTTest)
#'
#'
#' # Example 2:
#' # Using Wilcoxon rank sum test and significance value of 0.1
#' genesWilcoxon <- rankDEG(expressionData = OVExpression,
#'                       sampleData = OVSample,
#'                       case = "Ovarian cancer",
#'                       control = "Normal",
#'                       method = "wilcoxon",
#'                       siglevel = 0.1)
#' head(genesWilcoxon)
#'
#' @export
#' @importFrom stats t.test wilcox.test
#' @importFrom assertthat assert_that
#'
#' @references
#' Wickham H (2019). _assertthat: Easy Pre and Post Assertions_. R package version 0.2.1,
#' <https://CRAN.R-project.org/package=assertthat>.

rankDEG <- function(expressionData,
                    sampleData,
                    case,
                    control,
                    method = "t",
                    siglevel = 0.05) {

  # check for invalid method input
  assertthat::assert_that(method == "t" | method == "wilcoxon",
                          msg = "Invalid input to method. Valid inputs for method are \"t\" or \"wilcoxon\".")

  # check for case or control inputs in sampleData
  assertthat::assert_that((case %in% sampleData[, 1]) & (control %in% sampleData[, 1]),
                          msg = "Please ensure the case and control identifiers are correct.")

  cases <- row.names(subset(sampleData, type == case))
  controls <- row.names(subset(sampleData, type == control))

  # vector of genes
  genes <- row.names(expressionData)


  # prepare resultsDF
  resultsDF <- data.frame(Gene = genes,
                          TestStatistic = NA,
                          Pval = NA,
                          SignificantDiff = NA)


  # prepare vector for results of t-test
  results <- numeric(length = length(genes))

  # add comments here
  for (i in seq_along(genes)){
    # separate expressionData for case and controls
    caseExpr <- expressionData[genes[i], cases]
    controlExpr <- expressionData[genes[i], controls]
    if (method == "t"){
      # t-test
      result <- t.test(as.numeric(caseExpr), as.numeric(controlExpr))
    } else if (method == "wilcoxon"){
      # wilcoxon rank sum test
      result <- wilcox.test(as.numeric(caseExpr), as.numeric(controlExpr))
    } else {
      # should not reach here but message in case
      stop("Invalid input to method. Valid inputs for method are \"t\" or \"wilcoxon\".")
    }
    resultsDF[i, ]$TestStatistic <- result$statistic
    resultsDF[i, ]$Pval <- result$p.value
    resultsDF[i, ]$SignificantDiff <-(result$p.value <= siglevel)
  }

  # reorder by increasing p value
  orderedDF <- resultsDF[with(resultsDF, order(Pval)), ]
  row.names(orderedDF) <- NULL

  return(orderedDF)
}



#' Plot Differential Gene Expression of Case vs Control
#'
#' Produces boxplots of gene expression data for specified genes. The function
#' uses the `melt` function from the `reshape` package to manipulate data, and
#' the `ggplot2` package to create the plot.
#'
#' @param expressionData A dataframe of numeric gene expression data, with genes as
#'    rows and samples as columns.
#' @param sampleData A dataframe of sample information, with samples as rownames,
#'    and a column called "type" identifying if the sample is case or control.
#' @param genes A string or character vector of genes to include in the plot.
#'    This argument is optional; if not included, all genes will be included
#'    in the plot. If input is not a string or character vector, all genes will
#'    be included in the plot.
#'
#'
#' @return Returns a boxplot of expression of specified genes in cases and controls.
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
#' # Only genes PAX8 and DDR1 are included
#' exprPlot(expressionData = OVExpression,
#'         sampleData = OVSample,
#'         genes = c("PAX8", "DDR1"))
#'
#'
#' # Example 3:
#' # Using keggDiseaseGenes to get a list of genes associated with a disease
#' # Get genes associated with ovarian cancer (KEGG entry H00027)
#' OVgenes <- keggDiseaseGenes(entryID = "H00027")
#' exprPlot(expressionData = OVExpression,
#'         sampleData = OVSample,
#'         genes = OVgenes)
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

  # print message for incorrect input
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
  # melt prints a message, suppress message
  suppressMessages({
    meltedData <- reshape::melt(allData)
  })

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
