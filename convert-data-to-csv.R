# Load and install `rio` to 
# import and export .dta files
install.packages("rio")
library(rio)

# Import .dta dataset into R
data <- rio::import("data/ctax_AEJM.dta")

# Export .dta to .csv 
rio::export(data, "data/ctax_AEJM.csv")