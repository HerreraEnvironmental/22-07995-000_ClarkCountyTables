library(readxl)
library(tidyverse)

## Import
raw.data <- read_excel("data_raw/NestedQueries_ExportToRegina_20230616.xlsx")

## Explore
print(dim(raw.data))
print(colnames)
