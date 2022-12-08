#' Load data from files
#'
#' A function prepares data in the format required for other functions in the
#' expressionAnalysis package, given the path to the files.
#'
#' @param exprFilePath A string of the path to the file containing gene
#'    expression data. The file should be a matrix with the first row as sample
#'    names, the first column as gene names, and numeric expression values in the rest
#'    of the matrix.
#' @param sampleFilePath A string of the path to the file containing sample
#'    information. The file should have the first column as sample names, and
#'    the second column as their type.
#' @param sep A parameter to specify the the separator in the file. The options
#' are:
#' \itemize{
#'   \item "tab" - for whitespace/tab-separated values (default)
#'   \item "comma" - for comma-separated values
#'   \item "semicolon" - for semicolon-separated values
#' }
#'
#' @return Returns a list of two dataframes:
#' \itemize{
#'   \item expressionData - dataframe of expression values, rows are genes, columns are samples
#'
#'   \item sampleData - dataframe of sample information, rows are samples, one column "type" with the type of sample
#' }
#'
#' @examples
#' # Access data made available with this package
#' OVExpresssionPath <- system.file("extdata", "OVExpression.CSV", package = "expressionAnalysis")
#' OVSamplePath <- system.file("extdata", "OVSample.CSV", package = "expressionAnalysis")
#'
#' # Load data with loadData function
#' loadedData <- loadData(OVExpresssionPath, OVSamplePath, sep = "comma")
#'
#' # Access the dataframes
#' loadedData$expressionData    # dataframe for expression data
#' loadedData$sampleData        # dataframe for sample information
#'
#' @export

loadData <- function(exprFilePath, sampleFilePath, sep = "tab") {
  if (sep == "tab"){
    # tsv
    exprData <- read.table(exprFilePath, row.names = 1)
    sampleData <- read.table(sampleFilePath, row.names = 1)
  } else if (sep == "comma"){
    #csv
    exprData <- read.csv(exprFilePath, row.names = 1)
    sampleData <- read.csv(sampleFilePath, row.names = 1)
  } else if (sep == "semicolon"){
    # semicolon
    exprData <- read.csv2(exprFilePath, row.names = 1)
    sampleData <- read.csv2(sampleFilePath, row.names = 1)
  } else {
    stop("Invalid file separator specified. Valid inputs for sep are \"tab\", \"commma\" or \"semicolon\".
         You may try to change the file type or import the data as dataframes
         (see OVExpression and OVSample for examples on the dataframe format).")
  }

  colnames(sampleData) <- c("type") # specifying name of column

  # we will now check if there are missing values
  if(sum(is.na(exprData)) > 0) {
    message("The expression data file has missing values; this may cause
            unexpected results in other functions.")
  } else {
    ;
  }

  if (! setequal(intersect(colnames(exprData), rownames(sampleData)), colnames(exprData))) {
  # checking if sampleData contains info for all samples in exprData
    # missing sample info
    message("The sample information file does not contain information on all
            samples included in the expression data file. This may cause
            unexpected results in other functions.")
  } else {
    ;
  }

  # creating list of dataframes to return
  dataList <- list(exprData, sampleData)
  names(dataList) <- c("expressionData", "sampleData")
  return(dataList)
}


# [END]
