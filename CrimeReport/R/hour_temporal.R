#' Calculate Crime Count by Hour and Type for Specific Years and Months
#'
#' Calculate the total crime count for each hour and crime type for specified years and months.
#'
#' @param crime_data A data frame containing the crime dataset.
#' @param years A numeric vector of years.
#' @param months A numeric vector of months (1-12) corresponding to the specified years.
#'
#' @return A data frame with columns: HOUR, TYPE, and count.
#'
#' @examples
#' # Load the crime dataset
#' crime_data <- load_vancouver_crime_data()
#'
#' # Calculate crime count by hour and type for specific years and months
#' crime_count_by_hour_type <- calculate_crime_count_by_hour_type(crime_data,
#'                                                                years = c(2003, 2015, 2017),
#'                                                                months = c(5, 11, 11))
#' @import dplyr
#' @import ggplot2
#' @export
calculate_crime_count_by_hour_type <- function(crime_data, years, months) {
  crime_count <- crime_data %>%
    filter(YEAR %in% years, MONTH %in% months) %>%
    group_by(HOUR, TYPE) %>%
    summarize(count = n())

  return(crime_count)
}
