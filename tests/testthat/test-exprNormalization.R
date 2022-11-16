library(expressionAnalysis)

test_that("total count normalization (no argument given)", {
  normalizedExpr <- exprNormalization(OVExpression)

  # dataframe structure does not change
  expect_identical(dim(normalizedExpr), dim(OVExpression))
  expect_identical(rownames(normalizedExpr), rownames(OVExpression))
  expect_identical(colnames(normalizedExpr), colnames(OVExpression))

  # vector of sums of each column/sample
  normColSums <- as.vector(colSums(normalizedExpr))

  # sum of each column/sample is 1
  expect_equal(normColSums, rep(1, ncol(normalizedExpr)))

})

test_that("total count normalization (with argument)", {
  normalizedExpr <- exprNormalization(OVExpression, method = "total")

  # dataframe structure does not change
  expect_identical(dim(normalizedExpr), dim(OVExpression))
  expect_identical(rownames(normalizedExpr), rownames(OVExpression))
  expect_identical(colnames(normalizedExpr), colnames(OVExpression))

  # vector of sums of each column/sample
  normColSums <- as.vector(colSums(normalizedExpr))

  # sum of each column/sample is 1
  expect_equal(normColSums, rep(1, ncol(normalizedExpr)))

})


test_that("log2 transformation", {
  logExpr <- exprNormalization(OVExpression, method = "log")

  # dataframe structure does not change
  expect_identical(dim(logExpr), dim(OVExpression))
  expect_identical(rownames(logExpr), rownames(OVExpression))
  expect_identical(colnames(logExpr), colnames(OVExpression))

  # values should be log base 2
  expect_equal(logExpr, log(OVExpression, base = 2))

})


test_that("z-score standardization", {
  standardizedExpr <- exprNormalization(OVExpression, method = "standard")

  # dataframe structure does not change
  expect_identical(dim(standardizedExpr), dim(OVExpression))
  expect_identical(rownames(standardizedExpr), rownames(OVExpression))
  expect_identical(colnames(standardizedExpr), colnames(OVExpression))


  # vectors of means and standard deviations of columns/samples
  meanVec <- as.vector(colMeans(standardizedExpr))
  sdVec <- as.vector(apply(standardizedExpr,2,sd))

  # mean should be 0, standard deviation should be 1
  expect_equal(meanVec, rep(0, ncol(standardizedExpr)))
  expect_equal(sdVec, rep(1, ncol(standardizedExpr)))

})


test_that("invalid input to method", {
  # arguments not given as strings
  expect_error(exprNormalization(OVExpression, method = total))
  expect_error(exprNormalization(OVExpression, method = log))
  expect_error(exprNormalization(OVExpression, method = standard))

  # incorrect inputs
  expect_error(exprNormalization(OVExpression, method = 1))
  expect_error(exprNormalization(OVExpression, method = "blah"))
  expect_error(exprNormalization(OVExpression, method = FALSE))

  # multiple methods given
  expect_error(exprNormalization(OVExpression, method = c("total", "log",
                                                          "standard")))

})


# [END]
