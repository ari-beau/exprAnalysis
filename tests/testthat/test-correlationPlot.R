library(expressionAnalysis)

test_that("no errors with valid input to method", {
  expect_no_error(correlationPlot(expressionData = OVExpression))
  expect_no_error(correlationPlot(expressionData = OVExpression,
                                  method = "spearman"))
  expect_no_error(correlationPlot(expressionData = OVExpression,
                                  method = "pearson"))
  expect_no_error(correlationPlot(expressionData = OVExpression,
                                  method = "kendall"))

  # returns invisible null
  expect_null(correlationPlot(expressionData = OVExpression))
})


test_that("invalid input to method", {
  # arguments not given as strings
  expect_error(correlationPlot(expressionData = OVExpression,
                               method = spearman))
  expect_error(correlationPlot(expressionData = OVExpression,
                               method = pearson))
  expect_error(correlationPlot(expressionData = OVExpression,
                               method = kendall))

  # incorrect inputs
  expect_error(exprNormalization(OVExpression, method = 1))
  expect_error(exprNormalization(OVExpression, method = "blah"))
  expect_error(exprNormalization(OVExpression, method = FALSE))

  # multiple methods given
  expect_error(exprNormalization(OVExpression, method = c("spearman", "pearson",
                                                          "kendall")))

})
