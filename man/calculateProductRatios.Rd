% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculateProductRatios.R
\name{calculateProductRatios}
\alias{calculateProductRatios}
\title{Calculate Product Ratios by Wood Type, Township, and Matrix}
\usage{
calculateProductRatios(df)
}
\arguments{
\item{df}{A dataframe containing four columns: 'township', 'matrix', 'product', and 'cords'.}
}
\value{
A dataframe with the columns: 'township', 'matrix', 'product', 'woodtype', 'cords', 'HWratio', and 'SWratio'. For HW products, the SWratio will be 0 and vice versa.
}
\description{
This function calculates the percentage of each product within its wood type (HW or SW) for each combination of township and matrix. The function uses the `mapWoodType` function to categorize the products.
}
\examples{
df <- data.frame(
  township = c("T1", "T1", "T1", "T2", "T2"),
  matrix = c("M1", "M1", "M1", "M2", "M2"),
  product = c("ASl", "LVl", "CEl", "BFp", "HVp"),
  cords = c(10, 15, 20, 5, 30)
  acres = c(32, 32, 32, 55, 55)
)

calculateProductRatios(df)

}
\seealso{
Other InputTransform Functions: 
\code{\link{GrowthModelInput}()},
\code{\link{dataPreparation}()},
\code{\link{mapWoodType}()}
}
\concept{InputTransform Functions}
