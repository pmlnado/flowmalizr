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
  dplyr::summarise_at(dplyr::vars(percentage_of_total),dplyr::funs(mean(.,na.rm=TRUE))) %>%
  dplyr::mutate(Perc = paste0(round(percentage_of_total, digits = 2), "%")) %>%
  dplyr::arrange(desc(percentage_of_total))

gg_sep2 <- gg_sep %>%
  dplyr::filter(!is.na(percentage_of_total)) %>%
  dplyr::group_by(group) %>%
  dplyr::summarise(group, name, Perc, percentage_of_total)

gg_sep3 <- gg_sep %>%
  dplyr::filter(!is.na(percentage_of_total)) %>%
  dplyr::group_by(group) %>%
  dplyr::summarise(group, name, Perc, percentage_of_total) %>%
  dplyr::filter(group %in% c(1,2))

gg_sep4 <- gg_sep %>%
  dplyr::filter(!is.na(percentage_of_total)) %>%
  dplyr::group_by(group) %>%
  dplyr::summarise(group, name, Perc, percentage_of_total)

ggplot2::
#ALL
ggplot2::ggplot(data = gg_sep4) +
  ggmosaic::geom_mosaic(ggplot2::aes(x = ggmosaic::product(name), na.rm = TRUE,
                  fill = name, weight = percentage_of_total)) +
  ggplot2::facet_grid(~group) +
  ggplot2::coord_flip() +
  ggplot2::scale_fill_discrete(guide = ggplot2::guide_legend(reverse=TRUE)) +
  ggplot2::labs(x = "", y = "Group", fill = "Phenotype") +
  ggplot2::theme(aspect.ratio = 20,
        axis.text.x = ggplot2::element_blank(),
        axis.ticks.x = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank(),
        legend.key.size = ggplot2::unit(.3, 'cm'),legend.title = ggplot2::element_text(size=10))


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

