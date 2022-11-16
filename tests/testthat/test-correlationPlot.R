library(expressionAnalysis)

test_that("no error occurs", {
  expect_no_error(correlationPlot(expressionData = OVExpression))
})
