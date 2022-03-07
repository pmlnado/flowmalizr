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
   df <- tidyr::pivot_longer(xlsx_file, cols = -c(1:3))
   df <- df %>%
      dplyr::mutate(cells_from_total = total_cell_count_per_mL*value/live_cells) %>%
      dplyr::select(1, 2, 4, 6) %>%
      dplyr::mutate(percentage_of_total = cells_from_total/total_cell_count_per_mL*100) %>%
      dplyr::arrange(percentage_of_total)

return(df)
}

# Pull unique gated populations
unique.pops <- function(){
   df <- flowmalizr(path = path_to_data)
   unique <- df %>% dplyr::pull(name) %>% unique()

return(unique)
}

# Determine mean populations of experiment
view_pops <- function(){
   df <- flowmalizr(path = path_to_data)
   view <- df %>% dplyr::group_by(name) %>% dplyr::summarise_at(vars(percentage_of_total),funs(mean(.,na.rm=TRUE)))

return(view)
}

# Plot percentage of unique populations



