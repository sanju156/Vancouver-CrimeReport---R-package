#' Analysis Of Vancouver Crime Analysis
#'
#' Load Vancouver Crime Dataset
#'
#' This function loads the Vancouver crime dataset from the package's inst directory.
#'
#' @return A data frame containing the crime dataset.
#'
#' @examples
#' # Load the Vancouver crime dataset
#' crime_data <- load_vancouver_crime_data()
#'
#' # Display the first few rows of the dataset
#' print(head(crime_data))
#'
#' # Display the number of rows and columns in the dataset
#' num_rows <- nrow(crime_data)
#' num_cols <- ncol(crime_data)
#' cat("Number of rows:", num_rows, "\n")
#' cat("Number of columns:", num_cols, "\n")
#'
#' @export
load_vancouver_crime_data <- function() {
  # Use system.file to locate the CSV file
  data_path <- system.file("extdata", "crime.csv", package = "CrimeReport")
  crime_data <- readr::read_csv(data_path)
  return(crime_data)
}
