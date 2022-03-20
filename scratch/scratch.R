library(devtools)
library(roxygen2)
library(testthat)
library(available)
library(dplyr)
library(ggplot2)
library(forcats)
# available("flowmalizr")
# input cell pup in graph the different groups
# separate mouse and treatment groups

flowmalizr(path = path_to_data)
unique.pops()
view_pops()
## plot
df <- flowmalizr(path = path_to_data)
view <- df %>% dplyr::group_by(name) %>%
  dplyr::summarise_at(vars(percentage_of_total),
                      funs(mean(.,na.rm=TRUE)))
view <- view %>%
  mutate(Perc = paste0(round(percentage_of_total, digits = 2), "%"))

ggplot(view, aes(x = fct_reorder(name, percentage_of_total),
                 y = percentage_of_total, fill = name)) +
  geom_bar(stat = "identity") +
  ylim(0, 100) +
  coord_flip() +
  labs(x = "Phenotype", y = "Mean Percent") +
  geom_text(aes(label=Perc), position=position_dodge(width = 0), hjust = -.1,
            size = 3)



