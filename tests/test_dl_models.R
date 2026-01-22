# Import Some Libraries
library(testthat)
library(keras)

# Load DL pipeline
source("C:/Users/user/Downloads/Data analyst/orthotic-prosthetic-analytics/scripts/06_dl_models.R")

test_that("Keras model is created", {
  expect_true(exists("model"))
  expect_true(inherits(model, "python.builtin.object"))
})

test_that("Model has correct output units", {
  expect_equal(model$layers[[length(model$layers)]]$units, ncol(y))
})

test_that("Training history is available", {
  expect_true(exists("history"))
  expect_true("accuracy" %in% names(history$metrics))
})
