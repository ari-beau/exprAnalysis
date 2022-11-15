#' Normalizes Gene Expression Data
#'
#' A function that calculates normalizes gene expression data, with three
#' methods available: total count normalization, log2 transformation, and
#' z-score standardization. The function uses the `dplyr` package to
#' manipulate the data.
#'
#'
#' @param expressionData A dataframe of gene expression data, with genes as
#'    rows and samples as columns.
#' @param method A parameter to specify normalization method. The options are:
#' \itemize{
#'   \item "total" - total count normalization (default)
#'   \item "log" - log2 transformation
#'   \item "standard" - z-score standardization
#' }
#'
#'
#' @return Returns a dataframe of of normalized gene expression data from
#'    expressionData.
#'
#' @examples
#' # Using OVExpression dataset in package
#'
#' # Example 1:
#' # Using default normalization method "total"
#' normalizedExpr <- exprNormalization(OVExpression)
#' # view first 5 rows and columns
#' normalizedExpr[1:5, 1:5]
#'
#' # sum of each column/sample is 1
#' colSums(normalizedExpr)
#'
#'
#' # Example 2:
#' # Using log2 transformation
#' logExpr <- exprNormalization(OVExpression, method = "log")
#' # view first 5 rows and columns
#' logExpr[1:5, 1:5]
#'
#'
#' # Example 3:
#' # Using standardization
#' standardizedExpr <- exprNormalization(OVExpression, method = "standard")
#' # view first 5 rows and columns
#' standardizedExpr[1:5, 1:5]
#'
#' # mean for each column/sample is 0
#' round(colMeans(standardizedExpr))
#' # standard deviation for each column/sample is 1
#' apply(standardizedExpr,2,sd)
#'
#' @export
#' @import dplyr
#'
#' @references
#' Wickham H, François R, Henry L, Müller K (2022). dplyr: A Grammar of Data
#' Manipulation. <https://dplyr.tidyverse.org>,
#' <https://github.com/tidyverse/dplyr>.

exprNormalization <- function(expressionData, method = "total") {
  if (method == "total"){
    # divide each column (sample) by total sum of expression levels in column
    normData <- sweep(expressionData, 2, colSums(expressionData), `/`)
  } else if (method == "log"){
    # apply log2 transformation to dataframe
    normData <- log(expressionData, base = 2)
  } else if (method == "standard"){
    # standardize each column (sample) to mean = 0, standard deviation = 1
    normData <- expressionData %>%
      dplyr::mutate_all(~(scale(.) %>%
                            as.vector))
  } else {
    stop("Invalid input to method. Valid inputs for method are \"total\", \"log\" or \"standard\".")
  }
  return(normData)
}



# [END]
