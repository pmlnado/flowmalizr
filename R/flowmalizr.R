#' make_df
#'
#' @param file
#'
#' @return
#' @export
#'
#' @examples
#                      work on exp groups vs control groups
make_df <- function(path){
   xlsx_file <- readxl::read_excel(path)
   df <- tidyr::pivot_longer(xlsx_file, cols = -c(1:3))

return(df)
}

make_df("example_data/example_data.xlsx")



#' flowmalizr
#'
#' @param df
#'
#' @return
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
flowmalizr <- function(df){
   df <- df %>%
     dplyr::mutate(cells_from_total = total_cell_count_per_mL*value/live_cells) %>%
     select(1, 2, 4, 6) %>%
       mutate(percentage_of_total = cells_from_total/total_cell_count_per_mL*100) %>%
       arrange(percentage_of_total)

return(df)
}

flowmalizr(df)

#                             group vs cell pops
