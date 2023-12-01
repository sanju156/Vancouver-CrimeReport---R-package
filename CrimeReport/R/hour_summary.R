#' Analyze Crime Incidents by Hour of the Day
#'
#' Analyze crime incidents based on the hour of the day and identify trends in crime occurrence.
#'
#' @param crime_data A data frame containing the crime dataset.
#'
#' @return A data frame with columns: Hour_of_Day and Crime_Count.
#'
#' @examples
#' # Load the crime dataset
#' crime_data <- load_vancouver_crime_data()
#'
#' # Analyze crime incidents by hour of the day
#' hour_of_day_analysis <- analyze_hour_of_day_analysis(crime_data)
#' top_hours <- head(hour_of_day_analysis[order(hour_of_day_analysis$Crime_Count, decreasing = TRUE), ], 5)
#' print(top_hours)
#' @export
analyze_hour_of_day_analysis <- function(crime_data) {
  hour_of_day_summary <- crime_data %>%
    group_by(HOUR) %>%
    summarize(Crime_Count = n())

  return(hour_of_day_summary)
}
