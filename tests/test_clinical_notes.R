# Import the Library
library(testthat)

# Source Obtained From The "Clinician_Notes" Function in 01_generate_dataset.R
source("C:/Users/user/Downloads/Data analyst/orthotic-prosthetic-analytics/scripts/01_generate_dataset.R")

test_that("Mobility rules work correctly", {
  expect_equal(generate_clinical_notes(50, 60, 0, 0, 1, 1), "Bad progress; Stable")
  expect_equal(generate_clinical_notes(70, 60, 0, 0, 1, 1), "Good progress; Stable")
  expect_equal(generate_clinical_notes(60, 60, 0, 0, 1, 1), "Stable")
})

test_that("Pain rules work correctly", {
  expect_equal(generate_clinical_notes(60, 60, 2, 5, 1, 1), "Stable; No pain reported")
  expect_equal(generate_clinical_notes(60, 60, 7, 5, 1, 1), "Stable; Pain reported")
  expect_equal(generate_clinical_notes(60, 60, 5, 5, 1, 1), "Stable")
})

test_that("Satisfaction rules work correctly", {
  expect_equal(generate_clinical_notes(60, 60, 5, 5, 2, 4), "Stable; Need adjustment")
  expect_equal(generate_clinical_notes(60, 60, 5, 5, 5, 4), "Stable; No need adjustment")
  expect_equal(generate_clinical_notes(60, 60, 5, 5, 4, 4), "Stable")
})