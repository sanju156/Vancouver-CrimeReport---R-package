#' Find the Theft Type with the Least Count
#'
#' For the specified years and months, this function finds the theft type with the
#' least count in the provided theft dataset.
#'
#' @param theft_data A data frame containing the theft dataset.
#' @param years A numeric vector of years.
#' @param max_months A character vector of maximum crime count months (abbreviated).
#'
#' @return A list containing the least theft type for each year and month.
#'
#' @examples
#' # Load the theft dataset
#' theft_data <- load_vancouver_crime_data()
#'
#' # Find least theft type for specific years and months
#' least_theft_types <- find_least_theft_type(theft_data, c(2003, 2015, 2017), c("May", "Nov", "May"))
#'# Print the least theft types for each year and month
#'for (key in names(least_theft_types)) {
#'  year_month <- key
#'  least_type <- least_theft_types[[key]]$TYPE
#'  count <- least_theft_types[[key]]$count
#'
#'  cat("Year and Month:", year_month, "\n")
#'  cat("Least Theft Type:", least_type, "\n")
#'  cat("Count:", count, "\n\n")
#'}
#' @export
find_least_theft_type <- function(theft_data, years, max_months) {
  results <- list()

  for (i in seq_along(years)) {
    year <- years[i]
    month <- max_months[i]

    # Filter data for the specified year and month
    filtered_data <- theft_data %>%
      dplyr::filter(YEAR == year, MONTH == match(month, month.abb))

    # Find the theft type with the least count
    least_theft_type <- filtered_data %>%
      dplyr::group_by(TYPE) %>%
      dplyr::summarize(count = n()) %>%
      dplyr::arrange(count) %>%
      dplyr::slice(1)

    results[[paste(year, month, sep = "_")]] <- least_theft_type
  }

  return(results)
}
