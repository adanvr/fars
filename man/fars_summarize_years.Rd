% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions.R
\name{fars_summarize_years}
\alias{fars_summarize_years}
\title{fars_summarise_years}
\usage{
fars_summarize_years(years)
}
\arguments{
\item{years}{A character or integer vector of the years a user wants to read in.}
}
\value{
Wide data frame with counts by year
}
\description{
This is a function that creates a data frame based on a years vector and summarizes counts by year.
This function needs to import bind_rows, group_by and summarize from the dplyr package as well as spread from tidyr.
}
\examples{
fars_summarize_years(c(2000,2001,2002))

}
