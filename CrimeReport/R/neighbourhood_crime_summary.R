#' Neighborhood Analysis
#'
#' Analyze crime distribution across neighborhoods and identify neighborhoods with higher crime rates.
#'
#' @param crime_data A data frame containing the crime dataset.
#' @param top_n The number of top neighborhoods to display with the highest crime rates.
#'
#' @return A data frame with neighborhood crime distribution and the top neighborhoods with high crime rates.
#'
#' @examples
#' # Load the crime dataset
#' crime_data <- load_vancouver_crime_data()
#'
#' # Perform neighborhood analysis
#' neighborhood_analysis_result <- neighborhood_analysis(crime_data, top_n = 10)
#' print(neighborhood_analysis_result)
#'
#' @import dplyr
#' @export
neighborhood_analysis <- function(crime_data, top_n = 10) {


  neighborhood_summary <- crime_data %>%
    group_by(NEIGHBOURHOOD) %>%
    summarize(total_crimes = n()) %>%
    arrange(desc(total_crimes))

  top_neighborhoods <- head(neighborhood_summary, n = top_n)

  return(list(
    neighborhood_summary = neighborhood_summary,
    top_neighborhoods = top_neighborhoods
  ))
}
