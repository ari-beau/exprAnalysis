#' Load data from files (csv/tsv/excel??)
#'
#' A function prepares data in the format required for other functions in the
#' expressionAnalysis package, given the path to the files.
#'
#'
#' @param exprFilePath A string of the path to the file containing gene
#'    expression data.
#' @param sampleFilePath A string of the path to the file containing sample
#'    information
#' @param type A parameter to specify the file type. The options are:
#' \itemize{
#'   \item "tsv" - tab-separated values (default)
#'   \item "csv" - comma-separated values
#'   \item "excel" - excel file
#' }
#'
#' @return Returns a list of two dataframes:
#' \itemize{
#'   \item "expressionData" - dataframe of expression values, rows are genes, columns are samples
#'
#'   \item "sampleData" - dataframe of sample information, rows are samples, one column "type" with the type of sample
#' }
#'
#' @examples
#' # TODO!
#'
#' @export
#' @import readxl
#'
#' @references
#' Wickham H, Bryan J (2022). readxl: Read Excel Files.
#' https://readxl.tidyverse.org, https://github.com/tidyverse/readxl.

loadData <- function(exprFilePath, sampleFilePath, type = "tsv") {
  # check type

  # TODO: first column is rownames

  # TODO: specify format in documentation and/or add arg to allow transposed
  # matrix

  # TODO: add sheet number argument (or remove excel)

  if (type == "tsv"){
    # tsv
    exprData <- read.table(exprFilePath, row.names = 1)
    sampleData <- read.table(sampleFilePath, row.names = 1)
  } else if (type == "csv"){
    exprData <- read.csv(exprFilePath, row.names = 1)
    sampleData <- read.csv(sampleFilePath, row.names = 1)
  } else if (type == "excel"){
    exprData <- readxl::read_excel(exprFilePath)
    sampleData <- readxl::read_excel(sampleFilePath)
  } else {
    stop("Invalid type specified. Valid inputs for type are \"tsv\", \"csv\" or \"excel\".")
    # possibly add comment about trying to import by self according to format
    # and/or leaving a issue to add this file type
  }

  # check if sample info for all samples in exprData is included in sampleData
  # if not, give message to user


  dataList <- list(exprData, sampleData)
  names(dataList) <- c("expressionData", "sampleData")
  return(dataList)
}


# [END]
