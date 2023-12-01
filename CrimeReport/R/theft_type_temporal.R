#' Find the Theft Type with the Highest Count
#'
#' For the specified years and months, this function finds the theft type with the
#' highest count in the provided theft dataset.
#'
#' @param theft_data A data frame containing the theft dataset.
#' @param years A numeric vector of years.
#' @param max_months A character vector of maximum crime count months (abbreviated).
#'
#' @return A list containing the highest theft type for each year and month.
#'
#' @examples
#' # Load the theft dataset
#' theft_data <- load_vancouver_crime_data()
#'
#' # Find highest theft type for specific years and months
#' highest_theft_types <- find_highest_theft_type(theft_data, c(2003, 2015, 2017), c("May", "Nov", "May"))
#' # Print the highest theft types for each year and month
#' for (key in names(highest_theft_types)) {
#' year_month <- key
#' highest_type <- highest_theft_types[[key]]$TYPE
#' count <- highest_theft_types[[key]]$count
#'
#'cat("Year and Month:", year_month, "\n")
#' cat("Highest Theft Type:", highest_type, "\n")
#'cat("Count:", count, "\n\n")
#'}
#' @export
find_highest_theft_type <- function(theft_data, years, max_months) {
  results <- list()

  for (i in seq_along(years)) {
    year <- years[i]
    month <- max_months[i]

    # Filter data for the specified year and month
    filtered_data <- theft_data %>%
      dplyr::filter(YEAR == year, MONTH == match(month, month.abb))

    # Find the theft type with the highest count
    highest_theft_type <- filtered_data %>%
      dplyr::group_by(TYPE) %>%
      dplyr::summarize(count = n()) %>%
      dplyr::arrange(desc(count)) %>%
      dplyr::slice(1)

    results[[paste(year, month, sep = "_")]] <- highest_theft_type
  }

  return(results)
}
