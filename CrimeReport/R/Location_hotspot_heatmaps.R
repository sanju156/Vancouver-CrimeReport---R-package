#' Analyze Crime Incidents Based on Location
#'
#' Analyze crime incidents based on specific locations (hundred block, latitude, longitude)
#' and identify areas with higher crime incidents.
#'
#' @param crime_data A data frame containing the crime dataset.
#' @param threshold The threshold for identifying areas with higher crime incidents.
#'
#' @return A data frame with columns: Hundred_Block, Latitude, Longitude, and Crime_Count.
#'
#' @examples
#' # Load the crime dataset
#' crime_data <- load_vancouver_crime_data()
#'
#' # Analyze crime incidents based on location
#' location_analysis <- analyze_location_analysis(crime_data, threshold = 1000)
#' print(location_analysis)
#'
#' @export
analyze_location_analysis <- function(crime_data, threshold) {
  location_summary <- crime_data %>%
    group_by(HUNDRED_BLOCK, Latitude, Longitude) %>%
    summarize(Crime_Count = n()) %>%
    filter(Crime_Count > threshold) %>%
    arrange(desc(Crime_Count))

  return(location_summary)
}

