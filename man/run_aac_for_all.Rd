% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/run_aac_for_all.R
\name{run_aac_for_all}
\alias{run_aac_for_all}
\title{Run AAC simulations for all townships and matrices.}
\usage{
run_aac_for_all(
  df,
  aac_percentage,
  min_stocking,
  max_harvest = FALSE,
  min_aac = TRUE,
  years = 20
)
}
\arguments{
\item{df}{A dataframe containing the volume data, including columns for 'perAcreHW' and 'perAcreSW' (required).}

\item{aac_percentage}{Numeric, AAC percentage of growth until min stocking is reached (required).}

\item{min_stocking}{Numeric, minimum stocking level for AAC to begin or to reduce AAC to growth (required).}

\item{max_harvest}{Logical, whether to apply maximum harvest to `min_stocking` at period of first entry (default is FALSE).}

\item{min_aac}{Logical, whether to apply minimum AAC > growth once growth rates slow due to density (default is TRUE).}

\item{years}{Integer, number of years to simulate (default is 20).}
}
\value{
A dataframe with the growth and AAC simulation results for all township-matrix combinations over the specified years.
}
\description{
This function runs the AAC simulation for each unique township-matrix combination in the dataset.
Data format must match the export from ArcGIS using the SPA analysis tool. ArcGIS output should be loaded unaltered.

Default `maxvol`, `min_rate`, `sw_max_rate`, `hw_max_rate` from\code{\link{calculate_aac}} are used.
`min_rate` of .5 cords AAC is established once growth rates are below .25 cords/year.
`hw_volume` and `sw_volume` arguments for \code{\link{run_aac_simulation}} extracted from ArcGIS output.

The same `aac_percentage`, `min_stocking`, `max_harvest`, `min_aac` arguments will be applied to all strata.
This can result in behavior where some strata are not harvested until they reach minimum stocking while others have
a `max_harvest` performed in period 1.

Consult documentation for \code{\link{run_aac_simulation}} for further clarification on annualized runs and return values.
}
\examples{
# Simulated data
vols_wide <- data.frame(
  township = c("township1", "township1", "township2", "township2"),
  matrix = c("matrix1", "matrix2", "matrix1", "matrix2"),
  perAcreHW = c(12, 11, 0, 2),
  perAcreSW = c(11, 5, 20, 14)
)

# Example usage
results_combined <- run_aac_for_all(vols_wide, aac_percentage = 1,
min_stocking = 12, years = 30, max_harvest = TRUE)
print(results_combined)


}
\seealso{
\code{\link{calculate_aac}}, \code{\link{run_aac_simulation}}

Other AAC Functions: 
\code{\link{calculate_aac}()},
\code{\link{run_aac_simulation}()}
}
\concept{AAC Functions}
