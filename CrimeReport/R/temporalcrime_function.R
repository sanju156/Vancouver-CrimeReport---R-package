#' Analyze Temporal Trends in Crime Incidents
#'
#' This function analyzes the temporal trends in crime incidents based on year and month. It generates plots
#' to visualize the yearly trends and monthly trends for specific years.
#'
#' @param crime_data A data frame containing the crime dataset.
#'
#' @return A list containing visualizations for the yearly trends, along with summary statistics for years and months.
#'
#' @examples
#' # Load the Vancouver crime dataset
#' crime_data <- load_vancouver_crime_data()
#'
#' # Analyze temporal trends
#' temporal_trends <- analyze_temporal_trends(crime_data)
#' print(temporal_trends$text_results)
#' #print(temporal_trends$year_plot)
#' for (result in temporal_trends$specific_year_month_results) {
#'   cat(result$month_text_results, "\n")
#'   #print(result$month_plot)
#' }
#'
#' @import ggplot2
#' @import dplyr
#' @export
analyze_temporal_trends <- function(crime_data) {
  # Yearly trends
  year_counts <- crime_data %>%
    group_by(YEAR) %>%
    summarize(count = n())

  year_plot <- ggplot(year_counts, aes(x = YEAR, y = count, fill = factor(YEAR))) +
    geom_bar(stat = "identity", color = "black", alpha = 0.7) +
    labs(x = "Year", y = "Number of Incidents", title = "Yearly Trends in Crime Incidents") +
    scale_x_continuous(breaks = year_counts$YEAR, labels = year_counts$YEAR, expand = c(0, 0)) +
    theme_minimal() +
    theme(legend.position = "none")  # Remove the legend

  # Calculate summary statistics for years
  min_crimes <- min(year_counts$count)
  max_crimes <- max(year_counts$count)
  median_crimes <- median(year_counts$count)

  # Create text results for the yearly trend
  text_results <- paste(
    "Summary Statistics for Yearly Trend:",
    "Minimum number of crimes in a year:",
    paste("  - Year:", year_counts$YEAR[which(year_counts$count == min_crimes)], "Count:", min_crimes),
    "Maximum number of crimes in a year:",
    paste("  - Year:", year_counts$YEAR[which(year_counts$count == max_crimes)], "Count:", max_crimes),
    "Median number of crimes in a year:",
    paste("  - Year:", year_counts$YEAR[which(year_counts$count == median_crimes)], "Count:", median_crimes)
  )

  # Monthly trends for specific years (2003, 2015, 2017)
  specific_years <- c(2003, 2015, 2017)
  specific_year_month_results <- lapply(specific_years, function(year) {
    year_month_counts <- crime_data %>%
      filter(YEAR == year) %>%
      group_by(MONTH) %>%
      summarize(count = n())

    month_plot <- ggplot(year_month_counts, aes(x = MONTH, y = count)) +
      geom_line(color = "blue") +
      labs(x = "Month", y = "Number of Incidents", title = paste("Monthly Trends in Crime Incidents for Year", year)) +
      scale_x_continuous(breaks = 1:12, labels = month.abb, expand = c(0, 0)) +
      theme_minimal() +
      theme(legend.position = "none")

    # Calculate summary statistics for specific year and month
    min_month_crimes <- min(year_month_counts$count)
    max_month_crimes <- max(year_month_counts$count)
    median_month_crimes <- median(year_month_counts$count)

    # Create text results for specific year and month
    month_text_results <- paste(
      "Summary Statistics for Monthly Trend in Year", year,
      "Minimum number of crimes in a month:",
      paste("  - Month:", month.abb[which(year_month_counts$count == min_month_crimes)], "Count:", min_month_crimes),
      "Maximum number of crimes in a month:",
      paste("  - Month:", month.abb[which(year_month_counts$count == max_month_crimes)], "Count:", max_month_crimes),
      "Median number of crimes in a month:",
      paste("  - Month:", month.abb[which(year_month_counts$count == median_month_crimes)], "Count:", median_month_crimes)
    )

    list(
      year = year,
      month_plot = month_plot,
      month_text_results = month_text_results
    )
  })

  list(
    year_plot = year_plot,
    text_results = text_results,
    specific_year_month_results = specific_year_month_results
  )
}
