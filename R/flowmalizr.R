#' make_df
#'
#' @param path
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

sep_groups()




# Pull unique gated populations
unique_pops <- function(){
   unique_pop <<- imported_df %>% dplyr::pull(name) %>% unique()

return(unique_pop)
}

# Determine mean populations of experiment
view_pops <- function(){
   vizualise <<-
      imported_df %>% dplyr::group_by(name) %>%
      dplyr::summarise_at(vars(percentage_of_total),funs(mean(.,na.rm=TRUE))) %>%
      dplyr::mutate(Perc = paste0(round(percentage_of_total, digits = 2), "%")) %>%
      dplyr::arrange(desc(percentage_of_total)) %>%
      dplyr::select(c(1,3))

return(vizualise)
}

# Plot percentage of unique populations



