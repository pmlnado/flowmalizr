#' Normalize flow data cell counts and compare cell phenotypes
#'
#' \code{flowmalizr()} takes an xls input sheet containing columns listed as
#' groups, total_cell_count_per_mL, live_cells, and phenotypes of interest.
#' Each row contains a group-replicate, cell number per mL, live cell count,
#' and cells identified, respectively. This is used to detemine
#' cells_from_total and percentage_of_total by
#' (total_cell_count_per_mL*cells-of-phenotype/live_cells)*100.
#' The output from this function is a dataframe that can be manipulated for
#' visualization of cell phenotypes from each group
#'
#' @param path character string that leads to path_to_data
#'
#' @return Longer dataframe with data under new "name", "cells_from_total", and
#' "percentage_of_total" columns.
#'
#' @importFrom magrittr %>%
#' @importFrom utils globalVariables
#' @export
#'
#' @examples
#' path_to_data <- system.file("extdata", "example_data.xlsx", package = "flowmalizr")
#' flowmalizr(path_to_data)
#'

flowmalizr <- function(path){

utils::globalVariables("<<-")
   # utils::globalVariables(c("cells_from_total", "total_cell_count_per_mL", "value", "live_cells", "percentage_of_total"))

   xlsx_file <- readxl::read_excel(path)
   imported_df <<- tidyr::pivot_longer(xlsx_file, cols = -c(1:3))
   imported_df <<- imported_df %>%
      dplyr::mutate(cells_from_total = total_cell_count_per_mL*value/live_cells) %>%
      dplyr::select(1, 2, 4, 6) %>%
      dplyr::mutate(percentage_of_total = cells_from_total/total_cell_count_per_mL*100) %>%
      dplyr::arrange(percentage_of_total)

return(imported_df)

}



#' Separate group and replicate into their own columns
#'
#' \code{sep_group()} takes the newly generated \code{imported_df} and separates
#' groups from replicates for more accurate analysis when determining phenotypes
#' observed per group
#'
#' @return New df with group and replicate in their own columns
#'
#' @importFrom magrittr %>%
#' @importFrom utils globalVariables
#' @export
#'
#' @examples
#' sep_groups()
#'
sep_groups <- function(){

utils::globalVariables("<<-")
   # utils::globalVariables(c("groups", "imported_df", "def_sep"))

   df_sep <<- imported_df %>%
      tidyr::separate(groups, c("group", "replicate"),
                                 sep = "(?=[A-Za-z])(?<=[0-9])|(?=[0-9])(?<=[A-Za-z])")

   return(df_sep)
}


#' Pull unique gated populations
#'
#' \code{unique_pops()} provides a quick view of all phenotypes to be analyzed using this package
#'
#' @return A list of all populations and their percentages found in the experiment
#'
#' @importFrom magrittr %>%
#' @importFrom utils globalVariables
#' @export
#'
#' @examples
#' unique_pops()
#'
unique_pops <- function(){

utils::globalVariables("<<-")
   # utils::globalVariables(c("name", "imported_df", "def_sep", "percentage_of_total", ".", "unique_pop"))

   unique_pop <<- df_sep %>% dplyr::group_by(name) %>%
      dplyr::summarise_at(dplyr::vars(percentage_of_total),
                          dplyr::funs(mean(.,na.rm=TRUE))) %>%
      dplyr::mutate(Perc = paste0(round(percentage_of_total,
                                        digits = 2), "%")) %>%
      dplyr::arrange(dplyr::desc(percentage_of_total)) %>%
      dplyr::select(c(1,3))

return(unique_pop)

}

#' Plot percentage of unique populations
#'
#' \code{visualize_groups()} plots and averages biological replicates for visualization of
#' all groups and phenotypes
#'
#' @return A group facet wrapped series of graphs plotted by averaging biological
#' replicates to demonstrate percentage of phenotype populations
#'
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' visualize_groups()
visualize_groups <- function(){

utils::globalVariables("<<-")
   # utils::globalVariables(c("name", "imported_df", "def_sep", "percentage_of_total", ".", "unique_pop", "gg_sep", "Perc", "gggroup_visualize"))

   gg_sep <<- df_sep %>% dplyr::group_by(group, name) %>%
      dplyr::summarise_at(dplyr::vars(percentage_of_total),
                          dplyr::funs(mean(.,na.rm=TRUE))) %>%
      dplyr::mutate(Perc = paste0(round(percentage_of_total,
                                        digits = 2), "%")) %>%
      dplyr::arrange(dplyr::desc(percentage_of_total))

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
   ggplot2::theme(legend.position = "none") +
   ggplot2::geom_text(ggplot2::aes(label=Perc),
                      position=ggplot2::position_dodge(width = 0), hjust = -.1,
                      size = 3) +
   ggplot2::scale_y_continuous(limits = c(0, 90))

return(gggroup_visualize)

}



#' 1v1 comparison
#'
#' \code{roup_v_group()} allows for visual group vs group comparison of analyzed phenotypes
#'
#' @param groupA is group x
#' @param groupB is group y
#'
#' @return A 1v1 visual of phenotype per group
#'
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' group_v_group(1, 4)
group_v_group <- function(groupA, groupB){

utils::globalVariables("<<-")
   # utils::globalVariables(c("name", "group", "percentage_of_total", "gg_sep", "Perc"))

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

