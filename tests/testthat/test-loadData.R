test_that("valid input", {
  OVExpresssionPath <- system.file("extdata", "OVExpression.CSV", package = "expressionAnalysis")
  OVSamplePath <- system.file("extdata", "OVSample.CSV", package = "expressionAnalysis")

  expect_no_error(loadedData <- loadData(OVExpresssionPath, OVSamplePath, sep = "comma"))

  # checking if correct object returned
  expect_type(loadedData, "list")
  expect_equal(length(loadedData), 2L)

  expect_no_error(loadedData$expressionData)
  expect_no_error(loadedData$sampleData)

  # checking type column exists
  expect_no_error(loadedData$sampleData$type)

})



test_that("missing data", {
  OVExpresssionPath <- system.file("extdata", "OVExpression.CSV", package = "expressionAnalysis")
  OVSamplePath <- system.file("extdata", "OVSample.CSV", package = "expressionAnalysis")

  # missing values
  MissingExpresssionPath <- system.file("extdata", "OVExpression_missing.CSV", package = "expressionAnalysis")
  MissingSamplePath <- system.file("extdata", "OVSample_missing.CSV", package = "expressionAnalysis")

  # missing expression data
  expect_message(loadData(MissingExpresssionPath, OVSamplePath, sep = "comma"))

  # missing sample info
  expect_message(loadData(OVExpresssionPath, MissingSamplePath, sep = "comma"))

  # both missing
  expect_message(loadData(MissingExpresssionPath, MissingSamplePath, sep = "comma"))
})




test_that("invalid inputs", {
  OVExpresssionPath <- system.file("extdata", "OVExpression.CSV", package = "expressionAnalysis")
  OVSamplePath <- system.file("extdata", "OVSample.CSV", package = "expressionAnalysis")

  # incorrect sep
  expect_error(loadData(OVExpresssionPath, OVSamplePath, sep = "tab"))
  expect_error(loadData(OVExpresssionPath, OVSamplePath, sep = "semicolon"))

  # invalid input to sep
  expect_error(loadData(OVExpresssionPath, OVSamplePath, sep = csv))
  expect_error(loadData(OVExpresssionPath, OVSamplePath, sep = 1))
  expect_error(loadData(OVExpresssionPath, OVSamplePath, sep = "blah"))
  expect_error(loadData(OVExpresssionPath, OVSamplePath, sep = FALSE))
})
