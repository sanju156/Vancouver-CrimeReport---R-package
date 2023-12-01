#' Calculate Total Crime Count by Type in a Neighborhood for a Specific Year and Month
#'
#' Calculate the total number of crimes for each crime type in a specific neighborhood
#' for a given year and month.
#'
#' @param crime_data A data frame containing the crime dataset.
#' @param neighborhood A character string representing the neighborhood.
#' @param year A numeric value representing the year.
#' @param month A numeric value representing the month.
#'
#' @return A data frame with columns: TYPE and count.
#'
#' @examples
#' # Load the crime dataset
#' crime_data <- load_vancouver_crime_data()
#'
#' # Calculate total crime count by type in a neighborhood for a specific year and month
#' crime_count_by_type_neighborhood <- calculate_total_crime_count_by_type_neighborhood(crime_data,
#'                                                                                        neighborhood = "Central Business District",
#'                                                                                        year = 2017,
#' @import ggplot2
#' @import dplyr
#'
#' @export
calculate_total_crime_count_by_type_neighborhood <- function(crime_data, neighborhood, year, month) {
  crime_count <- crime_data %>%
    filter(NEIGHBOURHOOD == neighborhood, YEAR == year, MONTH == month) %>%
    group_by(TYPE) %>%
    summarize(count = n())

  return(crime_count)
}
