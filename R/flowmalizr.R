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
     mutate(cells = total_cell_count_per_mL*value/live_cells) %>%
     arrange(cells) %>%
     select(1, 4, 6)
return(df)
}

flowmalizr(df)

