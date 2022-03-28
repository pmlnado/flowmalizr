#' Normalize cell counts and compare cell phenotype
#'
#' @param path_to_data
#'
#' @return tidy dataframe made from xls input
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' path_to_data <- system.file("extdata", "example_data.xlsx", package = "flowmalizr")
#' flowmalizr(path = path_to_data)

# First function to normalize cell data

flowmalizr <- function(path){
   xlsx_file <- readxl::read_excel(path)
   imported_df <<- tidyr::pivot_longer(xlsx_file, cols = -c(1:3))
   imported_df <<- imported_df %>%
      dplyr::mutate(cells_from_total = total_cell_count_per_mL*value/live_cells) %>%
      dplyr::select(1, 2, 4, 6) %>%
      dplyr::mutate(percentage_of_total = cells_from_total/total_cell_count_per_mL*100) %>%
      dplyr::arrange(percentage_of_total)

return(imported_df)

}



#Separate Group and Replicate
sep_groups <- function(){
   df_sep <<- imported_df %>%
      tidyr::separate(groups, c("group", "replicate"),
                                 sep = "(?=[A-Za-z])(?<=[0-9])|(?=[0-9])(?<=[A-Za-z])")

   return(df_sep)
}

# Pull unique gated populations
unique_pops <- function(){
   unique_pop <<- df_sep %>% dplyr::group_by(name) %>%
      dplyr::summarise_at(dplyr::vars(percentage_of_total),
                          dplyr::funs(mean(.,na.rm=TRUE))) %>%
      dplyr::mutate(Perc = paste0(round(percentage_of_total,
                                        digits = 2), "%")) %>%
      dplyr::arrange(dplyr::desc(percentage_of_total)) %>%
      dplyr::select(c(1,3))

return(unique_pop)

}

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


# Plot percentage of unique populations
visualize_groups <- function(){
   gg_sep <<- df_sep %>% dplyr::group_by(group, name) %>%
      dplyr::summarise_at(dplyr::vars(percentage_of_total),
                          dplyr::funs(mean(.,na.rm=TRUE))) %>%
      dplyr::mutate(Perc = paste0(round(percentage_of_total,
                                        digits = 2), "%")) %>%
      dplyr::arrange(desc(percentage_of_total))

   gg_visualize <- gg_sep %>%
   dplyr::filter(!is.na(percentage_of_total)) %>%
   dplyr::group_by(group) %>%
   dplyr::summarise(group, name, Perc, percentage_of_total)

   gggroup_visualize <<- ggplot2::ggplot(gg_visualize,
                ggplot2::aes(x = forcats::fct_reorder(name, percentage_of_total),
                             y = percentage_of_total, fill = group)) +
   ggplot2::geom_bar(stat = "identity") +
   ggplot2::coord_flip() +
   ggplot2::labs(x = "Phenotype", y = "Mean Percent") +
   ggplot2::facet_wrap(~group) +
   ggplot2::geom_text(ggplot2::aes(label=Perc),
                      position=ggplot2::position_dodge(width = 0), hjust = -.1,
                      size = 3) +
   ggplot2::scale_y_continuous(limits = c(0, max(gg_sep$percentage_of_total)))

return(gggroup_visualize)

}

# 1v1 comparison

group_v_group <- function(groupA, groupB){

   gg_1v1 <- gg_sep %>%
      dplyr::filter(!is.na(percentage_of_total)) %>%
      dplyr::group_by(group) %>%
      dplyr::summarise(group, name, Perc, percentage_of_total) %>%
      dplyr::filter(group %in% c(groupA, groupB))
   gg1v1_visualize <- ggplot2::ggplot(gg_1v1, ggplot2::aes(x = name,
                                                           y = percentage_of_total,
                                         fill = group)) +
      ggplot2::geom_bar(position="fill", stat="identity") +
      ggplot2::coord_flip() +
      ggplot2::labs(x = "Phenotype", y = "Mean Percent") +
      ggplot2::geom_text(ggplot2::aes(label = Perc, y = .5),
                         position = ggplot2::position_stack(vjust = 0.5))
return(gg1v1_visualize)
}

