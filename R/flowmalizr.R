#' make_df
#'
#' @param file
#'
#' @return
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#                      work on exp groups vs control groups
flowmalizr <- function(path){
   xlsx_file <- readxl::read_excel(path)
   df <- tidyr::pivot_longer(xlsx_file, cols = -c(1:3))
   df <- df %>%
      dplyr::mutate(cells_from_total = total_cell_count_per_mL*value/live_cells) %>%
      dplyr::select(1, 2, 4, 6) %>%
      dplyr::mutate(percentage_of_total = cells_from_total/total_cell_count_per_mL*100)

return(df)
}

flowmalizr("example_data/example_data.xlsx")


#                             group vs cell pops
