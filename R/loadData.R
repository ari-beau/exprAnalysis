#' Load data from files (csv/tsv/excel??)
#'
#' A function prepares data in the format required for other functions in the
#' expressionAnalysis package, given the path to the files.
#'
#'
#' @param exprFilePath A string of the path to the file containing gene
#'    expression data. The file should be a matrix with the first row as sample
#'    names, the first column as gene names, and expression values in the rest
#'    of the matrix.
#' @param sampleFilePath A string of the path to the file containing sample
#'    information. The file should have the first column as sample names, and
#'    the second column as their type.
#' @param sep A parameter to specify the the separtor in the file. The options
#' are:
#' \itemize{
#'   \item "tab" - for whitespace/tab-separated values (default)
#'   \item "comma" - for comma-separated values
#'   \item "semicolon" - for semicolon separated values
#' }
#'
#' @return Returns a list of two dataframes:
#' \itemize{
#'   \item "expressionData" - dataframe of expression values, rows are genes, columns are samples
#'
#'   \item "sampleData" - dataframe of sample information, rows are samples, one column "type" with the type of sample
#' }
#'
#' @export
#' @import readxl
#'
#' @references
#' Wickham H, Bryan J (2022). readxl: Read Excel Files.
#' https://readxl.tidyverse.org, https://github.com/tidyverse/readxl.

loadData <- function(exprFilePath, sampleFilePath, type = "tab") {
  if (type == "tab"){
    # tsv
    exprData <- read.table(exprFilePath, row.names = 1)
    sampleData <- read.table(sampleFilePath, row.names = 1)
  } else if (type == "comma"){
    exprData <- read.csv(exprFilePath, row.names = 1)
    sampleData <- read.csv(sampleFilePath, row.names = 1)
  } else if (type == "semicolon"){
    exprData <- read.csv2(exprFilePath, row.names = 1)
    sampleData <- read.csv2(sampleFilePath, row.names = 1)
  } else {
    stop("Invalid type specified. Valid inputs for type are \"tab\", \"commma\" or \"semicolon\".
         You may try to change the file type or import the data as dataframes
         (see OVExpression and OVSample for examples on the file format).")
  }

  colnames(sampleData) <- c("type") # specifying name of column

  if(! setequal(intersect(colnames(exprData), rownames(sampleData)), colnames(exprData))) {
  # checking if sampleData contains info for all samples in exprData
    # missing sample info
    message("The sample information file does not contain information on all
            samples included in the expression data file. This may causes
            unexpected results in other functions.")
  }


  dataList <- list(exprData, sampleData)
  names(dataList) <- c("expressionData", "sampleData")
  return(dataList)
}


# [END]
