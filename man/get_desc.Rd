% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_desc.r
\name{get_desc}
\alias{get_desc}
\title{get_desc() produces a table with descriptives where items or variables are rows and columns are differerent descriptive values. its largely based on the deprecated function skimr::skim_to_wide}
\usage{
get_desc(x)
}
\arguments{
\item{x}{a data frame, where rows = observations, and columns = variables}
}
\value{
data frame with with descriptive values
}
\description{
get_desc() produces a table with descriptives where items or variables are rows and columns are differerent descriptive values. its largely based on the deprecated function skimr::skim_to_wide
}
\examples{

get_desc(items_data)


}
