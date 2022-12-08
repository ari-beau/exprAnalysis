test_that("valid input", {
  expect_no_error(keggDiseaseGenes("H00027"))
  expect_message(keggDiseaseGenes("H00027"))

  expect_type(keggDiseaseGenes("H00027"), "character")
})

test_that("invalid input", {
  # not string
  expect_error(keggDiseaseGenes(H00027))

  # incorrect inputs
  expect_error(keggDiseaseGenes(1))
  expect_error(keggDiseaseGenes("blah"))
  expect_error(keggDiseaseGenes(FALSE))
})
