#' Train GLM model
#'
#' This function trains a Generalized Linear Model (GLM) using the provided formula and hospital data.
#'
#' @param data A data frame containing hospital data.
#' @param formula A formula specifying the model to be fitted.
#' @param family The distribution family for the GLM model (e.g., "poisson", "negbinomial", "gamma").
#' @return A trained GLM model object.
#' @importFrom MASS glm.nb
# #' @importFrom glmnet function3 function4
# #' @importFrom pscl function5 function6
#' @export
#' @examples
#' data <- read.csv(system.file("data", "hospital_data.csv", package = "BestGLM"))
#' data<- process_data(data)
#' model <- glm(data, formula = LoS ~ comorbidities, family = "poisson")
#' @export
train_glm <- function(data, formula, family) {
  # Load necessary packages
    # For fitting negative binomial models
  if (!requireNamespace("MASS", quietly = TRUE)) {
    stop("Package 'MASS' is not available.")
  }
  # # For fitting penalized regression models
  # if (!requireNamespace("glmnet", quietly = TRUE)) {
  #   stop("Package 'glmnet' is not available.")
  # }
  # # For count data models
  # if (!requireNamespace("pscl", quietly = TRUE)) {
  #   stop("Package 'pscl' is not available.")
  # }
  # Check if family is negbinomial and load appropriate functions
  if (family == "negbinomial") {
    if (!requireNamespace("MASS", quietly = TRUE)) {
      stop("MASS package is required for negative binomial models but is not installed.")
    }
    # Load MASS package if not already loaded
    if (!is.element("glm.nb", ls("package:MASS"))) {
      library(MASS)
    }
  }

  if (family == "poisson") {
    # Fit a Poisson GLM model
    model <- glm(formula, data = data, family = "poisson")
  } else if (family == "negbinomial") {
    # Fit a negative binomial GLM model
    model <- glm.nb(formula, data = data)
  } else if (family == "gamma") {
    # Fit a gamma GLM model
    model <- glm(formula, data = data, family = "Gamma")
  } else {
    stop("Unsupported distribution family. Supported families: poisson, negbinomial, gamma.")
  }

  return(model)
}

#' Evaluate GLM model
#'
#' This function evaluates the performance of a trained GLM model using evaluation metrics such as RMSE and MAE.
#'
#' @param model A trained GLM model object.
#' @param data A data frame containing hospital data.
#' @return A list containing evaluation metrics (e.g., RMSE, MAE).
#' @export
#' @examples
#' data <- read.csv(system.file("data", "hospital_data.csv", package = "BestGLM"))
#' data <- process_data(data)
#' model <- train_glm(data, formula = LoS ~ comorbidities, family = "poisson")
evaluate_glm <- function(model, data) {
  predicted <- predict(model, newdata = data, type = "response")
  actual <- data$LoS  # Replace "response_variable" with the actual variable name
  # Compute evaluation metrics
  rmse <- sqrt(mean((predicted - actual)^2))
  mae <- mean(abs(predicted - actual))
  # Return evaluation metrics as a list
  return(list(RMSE = rmse, MAE = mae))
}

#' Select best GLM model
#'
#' This function selects the best GLM model from a list of formulas and distribution families based on evaluation metrics.
#'
#' @param data A data frame containing hospital data.
#' @param formulas A list of formulas to try.
#' @param families A list of distribution families to try (e.g., "poisson", "negbinomial", "gamma").
#' @return The best GLM model object.
#' @export
#' @examples
#' data <- read.csv(system.file("data", "hospital_data.csv", package = "BestGLM"))
#' data<- process_data(data)
#' formulas <- list(LoS ~ comorbidities)
#' families <- c("poisson", "negbinomial", "gamma")
#' best_model <- select_best_glm(data, formulas, families)
select_best_glm <- function(data, formulas, families) {
  best_model <- NULL
  best_evaluation <- Inf

  for (formula in formulas) {
    for (family in families) {
      model <- train_glm(data, formula, family)
      evaluation <- evaluate_glm(model, data)

      # Check if evaluation result is not NA
      if (!is.na(evaluation$RMSE)) {
        if (evaluation$RMSE < best_evaluation) {
          best_evaluation <- evaluation$RMSE
          best_model <- model
        }
      }
    }
  }

  return(best_model)
}

