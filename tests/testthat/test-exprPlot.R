library(expressionAnalysis)

test_that("input to genes argument", {
  exprPlot(expressionData = OVExpression,
           sampleData = OVSample,
           genes = c("PAX8", "DDR1"))

  # no genes given
  expect_no_error(exprPlot(expressionData = OVExpression,
                           sampleData = OVSample))
  expect_no_error(exprPlot(expressionData = OVExpression,
                           sampleData = OVSample,
                           genes = NULL))
  expect_no_error(exprPlot(expressionData = OVExpression,
                           sampleData = OVSample,
                           genes = c()))

  # all/some genes given not included in expressionData
  expect_no_error(exprPlot(expressionData = OVExpression,
                           sampleData = OVSample,
                           genes = "BRCA1"))
  expect_no_error(exprPlot(expressionData = OVExpression,
                           sampleData = OVSample,
                           genes = c("BRCA1")))
  expect_no_error(exprPlot(expressionData = OVExpression,
                           sampleData = OVSample,
                           genes = c("BRCA1", "DDR1")))

  # all genes given included in expressionData
  expect_no_error(exprPlot(expressionData = OVExpression,
                           sampleData = OVSample,
                           genes = c("UBA7", "PAX8", "DDR1")))

})


test_that("invalid input to genes", {
  # arguments not given as strings
  expect_error(exprPlot(expressionData = OVExpression,
                        sampleData = OVSample,
                        genes = PAX8))
  expect_error(exprPlot(expressionData = OVExpression,
                        sampleData = OVSample,
                        genes = c(PAX8, DDR1)))

})


# [END]
