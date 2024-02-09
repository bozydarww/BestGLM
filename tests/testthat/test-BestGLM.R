# Load the necessary packages
library(testthat)
library(BestGLM)  # package name is BestGLM

# Define tests for the train_glm function
test_that("train_glm function trains a GLM model", {
  # Create a dummy dataset
  data <- data.frame(
    response_variable = rpois(100, lambda = 2),
    predictor1 = rnorm(100),
    predictor2 = rnorm(100)
  )
  # Train a Poisson GLM model
  model <- train_glm(data, formula = response_variable ~ predictor1 + predictor2, family = "poisson")
  # Check that the returned object is a glm object
  expect_is(model, "glm")
})

# Define tests for the evaluate_glm function
test_that("evaluate_glm function evaluates a GLM model", {
  # Create a dummy dataset
  data <- data.frame(
    response_variable = rpois(100, lambda = 2),
    predictor1 = rnorm(100),
    predictor2 = rnorm(100)
  )
  # Train a Poisson GLM model
  model <- glm(response_variable ~ predictor1 + predictor2, data = data, family = "poisson")
  # Evaluate the model
  evaluation <- evaluate_glm(model, data)
  # Check that the evaluation result is a list containing RMSE and MAE
  expect_is(evaluation, "list")
  expect_named(evaluation, c("RMSE", "MAE"))
})

# Define tests for the select_best_glm function
test_that("select_best_glm function selects the best GLM model", {
  # Create a dummy dataset
  data <- data.frame(
    response_variable = rpois(100, lambda = 2),
    predictor1 = rnorm(100),
    predictor2 = rnorm(100)
  )
  # Define a list of formulas and families to try
  formulas <- list(
    formula1 = response_variable ~ predictor1,
    formula2 = response_variable ~ predictor2
  )
  families <- c("poisson", "negbinomial")
  # Select the best GLM model
  best_model <- select_best_glm(data, formulas, families)
  # Check that the returned object is a glm object
  expect_is(best_model, "glm")
})
