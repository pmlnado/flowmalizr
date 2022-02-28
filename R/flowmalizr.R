#' make_df
#'
#' @param file
#'
#' @return
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' path_to_data <- system.file("extdata", "example_data.xlsx", package = "flowmalizr")
#' flowmalizr(path = path_to_data)
flowmalizr <- function(path){
   xlsx_file <- readxl::read_excel(path)
   df <- tidyr::pivot_longer(xlsx_file, cols = -c(1:3))
   df <- df %>%
      dplyr::mutate(cells_from_total = total_cell_count_per_mL*value/live_cells) %>%
      dplyr::select(1, 2, 4, 6) %>%
      dplyr::mutate(percentage_of_total = cells_from_total/total_cell_count_per_mL*100)

return(df)
}

