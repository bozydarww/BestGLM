#' Load hospital data from a CSV file
#' @return A data frame containing hospital data
load_data <- function(file_path) {
  # Check if the file exists
  if (!file.exists(file_path)) {
    stop("File not found.")
  }

  # Read the CSV file into a data frame
  data <- read.csv(file_path)

  # Return the loaded data
  return(data)
}

#' Preprocess hospital data
#' @param data A data frame containing hospital data
#' @return Preprocessed data frame
#' Preprocess hospital data
#' @param data A data frame containing hospital data
#' @return Preprocessed data frame
preprocess_data <- function(data) {
  # Convert date columns to proper date format
  data$date_of_admission <- as.Date(data$date_of_admission, format = "%Y-%m-%d")
  data$date_of_discharge <- as.Date(data$date_of_discharge, format = "%Y-%m-%d")
  data$LoS<- as.numeric(data$date_of_discharge - data$date_of_admission) # Length of Stay in the hospital in days
  data$comorbidities<-factor(data$comorbidities)
  # Remove unnecessary columns
  data <- subset(data, select = -c(patient_id,date_of_admission, date_of_discharge))

  # Perform additional preprocessing tasks if needed

  # Return preprocessed data frame
  return(data)
}

