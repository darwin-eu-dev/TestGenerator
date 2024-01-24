
test_that("Reading patients and JSON created successfully", {
  filePath <- testthat::test_path("testPatientsRSV.xlsx")
  outputPath <- tempdir()
  readPatients(filePath = filePath, outputPath = outputPath)
  expect_true(file.exists(file.path(outputPath, "test.json")))
  unlink(outputPath, recursive = TRUE)
})

# test_that("Reading patients NULL outputpath", {
#   filePath <- testthat::test_path("testPatientsRSV.xlsx")
#   readPatients(filePath = filePath, outputPath = NULL)
#   usethis::use_directory(fs::path("inst", "testCases"))
#   outputPath <- fs::path("inst", "testCases")
#   expect_true(file.exists(file.path(outputPath, "test.json")))
#   unlink(outputPath, recursive = TRUE)
# })

test_that("Patients to CDM", {
  filePath <- test_path("testPatientsRSV.xlsx")
  outputPath <- tempdir()
  dir.create(outputPath)
  TestGenerator::readPatients(filePath = filePath, outputPath = outputPath)
  cdm <- TestGenerator::cdmPatients(pathJson = outputPath, testName = "test")
  expect_equal(class(cdm), "cdm_reference")
  number_persons <- cdm[["person"]] %>% dplyr::pull(person_id)
  expect_equal(length(number_persons), 20)
  unlink(outputPath, recursive = TRUE)
})

test_that("SQL exists proj", {
filePath <- testthat::test_path("TestPatientsTemplateComorbidities.xlsx")
  outputPath <- paste0(proj_path(), "/", fs::path("inst", "testCases"), "/", "test", ".json")
  testDir <- paste0(proj_path(), "/", fs::path("inst", "testCases"))
  readPatients(filePath = filePath, outputPath = NULL)
  patientSQL(pathToTestCases = NULL)
  expect_true(dir.exists(paste0(testDir, "/", "sql")))
  expect_true(file.exists(outputPath))
  unlink(outputPath, recursive = TRUE)
  unlink(testDir, recursive = TRUE)
})

# test_that("SQL exists temp", {
#   filePath <- testthat::test_path("TestPatientsTemplateComorbidities.xlsx")
#   outputPath <- tempfile(fileext = ".json")
#   pathToTestCases <- tempdir()
#   readPatients(filePath = filePath, outputPath = outputPath)
#   patientSQL(pathToTestCases = pathToTestCases)
#   expect_true(dir.exists(paste0(pathToTestCases, "/", "sql")))
#   unlink(outputPath, recursive = TRUE)
# })
