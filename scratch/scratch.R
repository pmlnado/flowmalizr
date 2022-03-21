library(devtools)
library(roxygen2)
library(testthat)
library(available)
library(dplyr)
library(ggplot2)
library(forcats)
library(tidyr)
library(ggmosaic)
# available("flowmalizr")
# input cell pup in graph the different groups
# separate mouse and treatment groups

flowmalizr(path = path_to_data)
sep_groups()
unique_pops()
view_pops()



##################
gg_sep <- df_sep %>% dplyr::group_by(group, name) %>%
  dplyr::summarise_at(vars(percentage_of_total),funs(mean(.,na.rm=TRUE))) %>%
  dplyr::mutate(Perc = paste0(round(percentage_of_total, digits = 2), "%")) %>%
  dplyr::arrange(desc(percentage_of_total))

gg_sep2 <- gg_sep %>%
  filter(!is.na(percentage_of_total)) %>%
  group_by(group) %>%
  summarise(group, name, Perc, percentage_of_total)

gg_sep3 <- gg_sep %>%
  filter(!is.na(percentage_of_total)) %>%
  group_by(group) %>%
  summarise(group, name, Perc, percentage_of_total) %>%
filter(group %in% c(1,2))

gg_sep4 <- gg_sep %>%
  filter(!is.na(percentage_of_total)) %>%
  group_by(group) %>%
  summarise(group, name, Perc, percentage_of_total)


#ALL
ggplot(data = gg_sep4) +
  geom_mosaic(aes(x = product(name), na.rm = TRUE,
                  fill = name, weight = percentage_of_total)) +
  facet_grid(~group) +
  coord_flip() +
  scale_fill_discrete(guide = guide_legend(reverse=TRUE)) +
  labs(x = "", y = "Group", fill = "Phenotype") +
  theme(aspect.ratio = 20,
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.key.size = unit(.3, 'cm'),legend.title = element_text(size=10))



#FACETED
ggplot(gg_sep2, aes(x = fct_reorder(name, percentage_of_total),
                    y = percentage_of_total, fill = group)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x = "Phenotype", y = "Mean Percent") +
  facet_wrap(~group) +
  geom_text(aes(label=Perc), position=position_dodge(width = 0), hjust = -.1,
            size = 3) +
  scale_y_continuous(limits = c(0, max(gg_sep2$percentage_of_total)))

#almost for 1v1 comparison
ggplot(gg_sep3, aes(x = name, y = percentage_of_total, fill = group)) +
  geom_bar(position="fill", stat="identity") +
  coord_flip() +
  labs(x = "Phenotype", y = "Mean Percent") +
  geom_text(aes(label = Perc, y = .5), position = position_stack(vjust = 0.5))

