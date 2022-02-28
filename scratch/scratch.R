library(devtools)
library(roxygen2)
library(testthat)
library(available)
available("flowmalizr")

#create_package("/Users/PabloM/Documents/R_packages/flowmalizr")

df <- flowmalizr(path = path_to_data)


#                             unique cell pop names

df %>% dplyr::pull(name) %>%
  unique()

input cell pup in graph the different groups

separate mouse and treatment groups
