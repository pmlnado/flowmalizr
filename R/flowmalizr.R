library(tidyverse)

#' flowmalizr
#'
#' @param file
#'
#' @return
#' @export
#'
#' @examples

df <- readxl::read_excel("example_data/example_data.xlsx")
df <- tidyr::pivot_longer(df, cols = -c(1:3))


flowmalizr <- function(df){
   df <- df %>%
     mutate(cells_from_total = total_cell_count_per_mL*value/live_cells) %>%
     select(1, 2, 4, 6) %>%
       mutate(percentage_of_total = cells_from_total/total_cell_count_per_mL*100) %>%
       arrange(percentage_of_total)

return(df)
}

flowmalizr(df)

