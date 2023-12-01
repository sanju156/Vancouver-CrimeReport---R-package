## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(CrimeReport)

## -----------------------------------------------------------------------------
load_vancouver_crime_data <- function() {
  # Use system.file to locate the CSV file
  data_path <- system.file("extdata", "crime.csv", package = "CrimeReport")
  crime_data <- readr::read_csv(data_path)
  return(crime_data)
}

## -----------------------------------------------------------------------------
crime_data <- load_vancouver_crime_data()

## -----------------------------------------------------------------------------
print(head(crime_data))

## -----------------------------------------------------------------------------
num_rows <- nrow(crime_data)
num_cols <- ncol(crime_data)
cat("Number of rows:", num_rows, "\n")
cat("Number of columns:", num_cols, "\n")

## -----------------------------------------------------------------------------
library(dplyr)
library(ggplot2)
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

## ----fig.height=6,fig.width=8-------------------------------------------------
# Analyze temporal trends
temporal_trends <- analyze_temporal_trends(crime_data)

# Print text results for the yearly trend plot
cat(temporal_trends$text_results, "\n")

# Print the yearly trend plot
print("Yearly Trend Plot:")
print("This plot displays the trend of crime incidents over the years.")
print(temporal_trends$year_plot)

# Print text results and plots for specific year and month trends
for (i in seq_along(temporal_trends$specific_year_month_results)) {
	cat(temporal_trends$specific_year_month_results[[i]]$month_text_results, "\n")
	print(temporal_trends$specific_year_month_results[[i]]$month_plot)
}

## -----------------------------------------------------------------------------
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


## -----------------------------------------------------------------------------
theft_data <- load_vancouver_crime_data()
years <- c(2003, 2015, 2017)
max_months <- c("May", "Nov", "May")

highest_theft_types <- find_highest_theft_type(theft_data, years, max_months)


# Print the highest theft types for each year and month
for (key in names(highest_theft_types)) {
  year_month <- key
  highest_type <- highest_theft_types[[key]]$TYPE
  count <- highest_theft_types[[key]]$count
  
  cat("Year and Month:", year_month, "\n")
  cat("Highest Theft Type:", highest_type, "\n")
  cat("Count:", count, "\n\n")
}

## -----------------------------------------------------------------------------
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


## -----------------------------------------------------------------------------
years <- c(2003, 2015, 2017)
max_months <- c("May", "Nov", "May")

least_theft_types <- find_least_theft_type(theft_data, years, max_months)

# Print the least theft types for each year and month
for (key in names(least_theft_types)) {
  year_month <- key
  least_type <- least_theft_types[[key]]$TYPE
  count <- least_theft_types[[key]]$count
  
  cat("Year and Month:", year_month, "\n")
  cat("Least Theft Type:", least_type, "\n")
  cat("Count:", count, "\n\n")
}

## -----------------------------------------------------------------------------
neighborhood_analysis <- function(crime_data, top_n = 5) {
  # Load the required package
  library(dplyr)
  
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

## -----------------------------------------------------------------------------
# Perform neighborhood analysis
neighborhood_analysis_result <- neighborhood_analysis(crime_data, top_n = 5)

# View the neighborhood summary
print("Neighborhood Summary:")
print(neighborhood_analysis_result$neighborhood_summary)

# View the top neighborhoods with high crime rates
print("Top Neighborhoods with High Crime Rates:")
print(neighborhood_analysis_result$top_neighborhoods)

## -----------------------------------------------------------------------------
calculate_total_crime_count_by_type_neighborhood <- function(crime_data, neighborhood, year, month) {
  crime_count <- crime_data %>%
    filter(NEIGHBOURHOOD == neighborhood, YEAR == year, MONTH == month) %>%
    group_by(TYPE) %>%
    summarize(count = n())
  
  return(crime_count)
}

## ----fig.height=6,fig.width=8-------------------------------------------------
# Load required packages
library(ggplot2)
  
# Specify the neighborhood and months
neighborhood <- "Central Business District"
years <- c(2003, 2015, 2017)
months <- c(5, 11, 5)

# Create a list to store plots
plots <- list()

# Loop through each combination of year and month
for (i in seq_along(years)) {
  year <- years[i]
  month <- months[i]
  
  # Calculate total crime count by type in the specified neighborhood, year, and month
  crime_count_by_type <- calculate_total_crime_count_by_type_neighborhood(crime_data,
                                                                           neighborhood,
                                                                           year,
                                                                           month)
  
  # Create a bar plot using ggplot2 for the current year and month
  plot <- ggplot(crime_count_by_type, aes(x = reorder(TYPE, count), y = count, fill = TYPE)) +
    geom_bar(stat = "identity") +
    labs(x = NULL, y = "Total Crime Count",
         title = paste("Total Crime Count by Type in", neighborhood, "for", month, "/", year)) +
    theme_minimal() +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  
  plots[[paste(year, month, sep = "_")]] <- plot
}

# Display the plots
for (plot in plots) {
  print(plot)
}

## -----------------------------------------------------------------------------
calculate_crime_count_by_hour_type <- function(crime_data, years, months) {
  crime_count <- crime_data %>%
    filter(YEAR %in% years, MONTH %in% months) %>%
    group_by(HOUR, TYPE) %>%
    summarize(count = n())

  return(crime_count)
}

## ----fig.height=6,fig.width=8-------------------------------------------------
# Load required packages
library(dplyr)
library(ggplot2)


# Specify the years and months
years <- c(2003, 2015, 2017)
months <- c(5, 11, 5)

# Create and display separate heatmaps for each year and month combination
for (i in seq_along(years)) {
  year <- years[i]
  month <- months[i]
  
  # Calculate crime count by hour and type for specific year and month
  crime_count_by_hour_type <- calculate_crime_count_by_hour_type(crime_data, year, month)
  
  # Create a heatmap using ggplot2
  heatmap_plot <- ggplot(crime_count_by_hour_type, aes(x = HOUR, y = TYPE, fill = count)) +
    geom_tile() +
    scale_fill_gradient(low = "white", high = "red") +
    labs(x = "Hour", y = "Crime Type", title = paste("Crime Count by Hour and Type - Year", year, "Month", month)) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Display the heatmap
  print(heatmap_plot)
}

## -----------------------------------------------------------------------------
analyze_location_analysis <- function(crime_data, threshold) {
  location_summary <- crime_data %>%
    group_by(HUNDRED_BLOCK, Latitude, Longitude) %>%
    summarize(Crime_Count = n()) %>%
    filter(Crime_Count > threshold) %>%
    arrange(desc(Crime_Count))

  return(location_summary)
}


## -----------------------------------------------------------------------------
# Specify the threshold for identifying areas with higher crime incidents
threshold <- 1000

# Analyze crime incidents based on location
location_summary <- analyze_location_analysis(crime_data, threshold)

# Print the resulting location summary
print(location_summary)

## -----------------------------------------------------------------------------
analyze_hour_of_day_analysis <- function(crime_data) {
  hour_of_day_summary <- crime_data %>%
    group_by(HOUR) %>%
    summarize(Crime_Count = n())

  return(hour_of_day_summary)
}


## ----fig.height=6,fig.width=8-------------------------------------------------
# Analyze crime incidents by hour of the day
hour_of_day_summary <- analyze_hour_of_day_analysis(crime_data)

# Create a bar plot for crime incidents by hour of the day
hour_plot <- ggplot(hour_of_day_summary, aes(x = HOUR, y = Crime_Count)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "Hour of the Day", y = "Crime Count", title = "Crime Incidents by Hour of the Day")

# Display the plot
print(hour_plot)
hour_of_day_summary <- analyze_hour_of_day_analysis(crime_data)

top_hours <- head(hour_of_day_summary[order(hour_of_day_summary$Crime_Count, decreasing = TRUE), ], 5)
print(top_hours)


