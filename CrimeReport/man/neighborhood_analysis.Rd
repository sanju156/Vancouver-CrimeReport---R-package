% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/neighbourhood_crime_summary.R
\name{neighborhood_analysis}
\alias{neighborhood_analysis}
\title{Neighborhood Analysis}
\usage{
neighborhood_analysis(crime_data, top_n = 10)
}
\arguments{
\item{crime_data}{A data frame containing the crime dataset.}

\item{top_n}{The number of top neighborhoods to display with the highest crime rates.}
}
\value{
A data frame with neighborhood crime distribution and the top neighborhoods with high crime rates.
}
\description{
Analyze crime distribution across neighborhoods and identify neighborhoods with higher crime rates.
}
\examples{
# Load the crime dataset
crime_data <- load_vancouver_crime_data()

# Perform neighborhood analysis
neighborhood_analysis_result <- neighborhood_analysis(crime_data, top_n = 10)
print(neighborhood_analysis_result)

}
