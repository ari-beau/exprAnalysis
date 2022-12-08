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
#'   \item "log" - log2 transformation (default)
#'   \item "total" - total count normalization
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
#' # Using default normalization method "log"
#' logExpr <- exprNormalization(OVExpression)
#' head(logExpr[ , 1:5])
#'
#'
#' # Example 2:
#' # Using total count normalization
#' normalizedExpr <- exprNormalization(OVExpression, method = "total")
#' head(normalizedExpr[ , 1:5])
#'
#' # sum of each column/sample is 1
#' colSums(normalizedExpr)
#'
#'
#' # Example 3:
#' # Using standardization
#' standardizedExpr <- exprNormalization(OVExpression, method = "standard")
#' head(standardizedExpr[ , 1:5])
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

exprNormalization <- function(expressionData, method = "log") {

  # check if expressionData has missing values
  if (any(is.na(expressionData))) {
    message("expressionData has missing values, this may lead to unintended side effects")
  } else {
    ;
  }

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
    stop("Invalid input to method. Valid inputs for method are \"log\", \"total\" or \"standard\".")
  }
  return(normData)
}


# [END]
