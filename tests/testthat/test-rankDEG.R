library(expressionAnalysis)

test_that("t-test (no argument given)", {
  genesTTest <- rankDEG(expressionData = OVExpression,
                        sampleData = OVSample,
                        case = "Ovarian cancer",
                        control = "Normal")

  # ensure dimensions are correct
  expect_identical(nrow(genesTTest), nrow(OVExpression))
  expect_identical(ncol(genesTTest), 4L)

  # ensure all genes are included
  expect_identical(sort(genesTTest$Gene), sort(rownames(OVExpression)))

})

test_that("t-test (with argument)", {
  genesTTest <- rankDEG(expressionData = OVExpression,
                        sampleData = OVSample,
                        case = "Ovarian cancer",
                        control = "Normal",
                        method = "t")

  # ensure dimensions are correct
  expect_identical(nrow(genesTTest), nrow(OVExpression))
  expect_identical(ncol(genesTTest), 4L)

  # ensure all genes are included
  expect_identical(sort(genesTTest$Gene), sort(rownames(OVExpression)))

})


test_that("Wilcoxon rank sum test", {
  genesWilcoxon <- rankDEG(expressionData = OVExpression,
                        sampleData = OVSample,
                        case = "Ovarian cancer",
                        control = "Normal",
                        method = "wilcoxon")

  # ensure dimensions are correct
  expect_identical(nrow(genesWilcoxon), nrow(OVExpression))
  expect_identical(ncol(genesWilcoxon), 4L)

  # ensure all genes are included
  expect_identical(sort(genesWilcoxon$Gene), sort(rownames(OVExpression)))

})


test_that("invalid input to method", {
  # arguments not given as strings
  expect_error(genesWilcoxon <- rankDEG(expressionData = OVExpression,
                                        sampleData = OVSample,
                                        case = "Ovarian cancer",
                                        control = "Normal",
                                        method = t))
  expect_error(genesWilcoxon <- rankDEG(expressionData = OVExpression,
                                        sampleData = OVSample,
                                        case = "Ovarian cancer",
                                        control = "Normal",
                                        method = wilcoxon))


  # incorrect inputs
  expect_error(genesWilcoxon <- rankDEG(expressionData = OVExpression,
                                        sampleData = OVSample,
                                        case = "Ovarian cancer",
                                        control = "Normal",
                                        method = 1))
  expect_error(genesWilcoxon <- rankDEG(expressionData = OVExpression,
                                        sampleData = OVSample,
                                        case = "Ovarian cancer",
                                        control = "Normal",
                                        method = "blah"))
  expect_error(genesWilcoxon <- rankDEG(expressionData = OVExpression,
                                        sampleData = OVSample,
                                        case = "Ovarian cancer",
                                        control = "Normal",
                                        method = FALSE))

  # multiple methods given
  expect_error(genesWilcoxon <- rankDEG(expressionData = OVExpression,
                                        sampleData = OVSample,
                                        case = "Ovarian cancer",
                                        control = "Normal",
                                        method = c("t", "wilcoxon")))

})


# [END]
