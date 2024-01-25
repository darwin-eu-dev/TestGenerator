test_that("Reading patients and JSON created successfully", {
  filePath <- testthat::test_path("testPatientsRSV.xlsx")
  outputPath <- tempdir()
  dir.create(outputPath)
  readPatients(filePath = filePath, outputPath = outputPath)
  expect_true(file.exists(file.path(outputPath, "test.json")))
  unlink(outputPath, recursive = TRUE)
})

test_that("Patients to CDM", {
  filePath <- test_path("testPatientsRSV.xlsx")
  outputPath <- tempdir()
  dir.create(outputPath)
  TestGenerator::readPatients(filePath = filePath, outputPath = outputPath)
  cdm <- TestGenerator::patientsCDM(pathJson = outputPath, testName = "test")
  expect_equal(class(cdm), "cdm_reference")
  number_persons <- cdm[["person"]] %>% dplyr::pull(person_id)
  expect_equal(length(number_persons), 20)
  unlink(outputPath, recursive = TRUE)
})


