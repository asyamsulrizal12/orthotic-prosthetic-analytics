# Import Library
library(testthat)

# Load ML Pipeline
source("C:/Users/user/Downloads/Data analyst/orthotic-prosthetic-analytics/scripts/05_ml_models.R")

test_that("Random Forest model trains successfully", {
  expect_true(exists("model_rf"))
  expect_s3_class(model_rf, "train")
})

test_that("Random Forest predictions return valid labels", {
  expect_true(exists("pred"))
  expect_true(length(pred) > 0)
  expect_true(all(pred %in% levels(trainData$Clinician_Notes)))
})

test_that("Confusion matrix is generated", {
  expect_true(exists("cm"))
  expect_s3_class(cm, "confusionMatrix")
})
