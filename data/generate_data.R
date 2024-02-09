# Load necessary libraries
library(lubridate)

# Set seed for reproducibility
set.seed(123)

# # Generate mock data
# num_records <- 100
# patient_ids <- 1:num_records
# date_of_admission <- sample(seq(as.Date('2022/01/01'), as.Date('2022/12/31'), by="day"), num_records, replace=TRUE)
# date_of_discharge <- date_of_admission + sample(1:30, num_records, replace=TRUE)
# comorbidities <- sample(c("Asthma", "Diabetes", "Hypertension", "Obesity", "Heart Disease"), num_records, replace=TRUE)
#
# # Create data frame
# hospital_data <- data.frame(
#   patient_id = patient_ids,
#   date_of_admission = date_of_admission,
#   date_of_discharge = date_of_discharge,
#   comorbidities = comorbidities
# )

# # Write data frame to CSV file
# write.csv(hospital_data, file = "hospital_data.csv", row.names = FALSE)

#' Generate mock hospital data
#'
#' This function generates mock hospital data for testing purposes.
#' @return A data frame containing mock hospital data
#' @export
generate_data <- function() {
  # Generate mock data
  # Set seed for reproducibility
  set.seed(123)
  num_records <- 100
  patient_ids <- 1:num_records
  date_of_admission <- sample(seq(as.Date('2022/01/01'), as.Date('2022/12/31'), by="day"), num_records, replace=TRUE)
  date_of_discharge <- date_of_admission + sample(1:30, num_records, replace=TRUE)
  comorbidities <- sample(c("Asthma", "Diabetes", "Hypertension", "Obesity", "Heart Disease"), num_records, replace=TRUE)
  # Create data frame
  hospital_data <- data.frame(
    patient_id = patient_ids,
    date_of_admission = date_of_admission,
    date_of_discharge = date_of_discharge,
    comorbidities = factor(comorbidities)# Convert the character variable to a factor


  )

  # Write data frame to CSV file
  write.csv(hospital_data, "data/hospital_data.csv", row.names = FALSE)
  return(hospital_data)
}
