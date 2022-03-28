library(devtools)
library(roxygen2)
library(testthat)



flowmalizr(path = path_to_data)
sep_groups()
unique_pops()
gg_view_all()
visualize_groups()
group_v_group(1,2)

# View all phenotype's in each  group

gg_view_all <- function(){
  library(ggplot2)
  library(ggmosaic)
  gg_view_all_arranged <- df_sep  %>% dplyr::group_by(group, name) %>%
    dplyr::summarise_at(dplyr::vars(percentage_of_total),
                        dplyr::funs(mean(.,na.rm=TRUE))) %>%
    dplyr::mutate(Perc = paste0(round(percentage_of_total,
                                      digits = 2), "%")) %>%
    dplyr::arrange(desc(percentage_of_total)) %>%
    dplyr::filter(!is.na(percentage_of_total)) %>%
    dplyr::group_by(group) %>%
    dplyr::summarise(group, name, Perc, percentage_of_total)

  gg_view_all <<- ggplot2::ggplot(data = gg_view_all_arranged) +
    ggmosaic::geom_mosaic(ggplot2::aes(x = ggmosaic::product(name),
                                       na.rm = TRUE,fill = name, weight = percentage_of_total)) +
    ggplot2::facet_grid(~group) +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_discrete(guide = ggplot2::guide_legend(reverse=TRUE)) +
    ggplot2::labs(x = "", y = "Group", fill = "Phenotype") +
    ggplot2::theme(aspect.ratio = 20,
                   axis.text.x = ggplot2::element_blank(),
                   axis.ticks.x = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_blank(),
                   axis.ticks.y = ggplot2::element_blank(),
                   legend.key.size = ggplot2::unit(.3, 'cm'),
                   legend.title = ggplot2::element_text(size=10))
  return(gg_view_all)
}




