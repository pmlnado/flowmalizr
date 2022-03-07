library(devtools)
library(roxygen2)
library(testthat)
library(available)
library(dplyr)
library(ggplot2)
# available("flowmalizr")
# input cell pup in graph the different groups
# separate mouse and treatment groups

flowmalizr(path = path_to_data)
unique.pops()
view_pops()


df <- flowmalizr(path = path_to_data)
view <- df %>% dplyr::group_by(name) %>% dplyr::summarise_at(vars(percentage_of_total),funs(mean(.,na.rm=TRUE)))

hist(view$percentage_of_total, fill=view$name, srt=90, breaks=20)

#not sure what this is counting
ggplot(view, aes(percentage_of_total, fill = name)) + geom_histogram()
