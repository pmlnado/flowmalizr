library(devtools)
library(roxygen2)
library(testthat)
library(available)
library(dplyr)


flowmalizr(path = path_to_data)
sep_groups()
unique_pops()
visualize_groups()
group_v_group(1,6)


gg_sep4 <- gg_sep %>%
  dplyr::filter(!is.na(percentage_of_total)) %>%
  dplyr::group_by(group) %>%
  dplyr::summarise(group, name, Perc, percentage_of_total)

#ALL
# ggplot2::ggplot(data = gg_sep4) +
#   ggmosaic::geom_mosaic(ggplot2::aes(x = ggmosaic::product(name),
#   na.rm = TRUE,fill = name, weight = percentage_of_total)) +
#   ggplot2::facet_grid(~group) +
#   ggplot2::coord_flip() +
#   ggplot2::scale_fill_discrete(guide = ggplot2::guide_legend(reverse=TRUE)) +
#   ggplot2::labs(x = "", y = "Group", fill = "Phenotype") +
#   ggplot2::theme(aspect.ratio = 20,
#         axis.text.x = ggplot2::element_blank(),
#         axis.ticks.x = ggplot2::element_blank(),
#         axis.text.y = ggplot2::element_blank(),
#         axis.ticks.y = ggplot2::element_blank(),
#         legend.key.size = ggplot2::unit(.3, 'cm'),
#         legend.title = ggplot2::element_text(size=10))




