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

# First function to normalize cell data
path_to_data <- system.file("extdata", "example_data.xlsx", package = "flowmalizr")

flowmalizr <- function(path){
   xlsx_file <- readxl::read_excel(path)
   df <- tidyr::pivot_longer(xlsx_file, cols = -c(1:3))
   df <- df %>%
      dplyr::mutate(cells_from_total = total_cell_count_per_mL*value/live_cells) %>%
      dplyr::select(1, 2, 4, 6) %>%
      dplyr::mutate(percentage_of_total = cells_from_total/total_cell_count_per_mL*100) %>%
      dplyr::arrange(percentage_of_total)

   return(df)
df <- return(df)
}

flowmalizr(path = path_to_data)

# Pull unique gated populations
unique.pops <- function(df){
   unique <- df %>% dplyr::pull(name) %>% unique()

return(unique)
}

unique.pops(df)

# Plot percentage of unique populations
function(df){
pop <- ggplot2::ggplot(df) +
   ggplot2::geom_bar(ggplot2::aes(y = df$percentage_of_total, fill = name))

return(pop)
}


