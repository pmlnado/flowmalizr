
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
    library(tidyverse)
   df <- df %>%
     mutate(cells = (df[,"total_cell_count_per_mL"]*df[,"value"])/(df[,"live_cells"])) %>%
     arrange(cells) %>%
     select(1, 4, 6)

return(df)
}

flowmalizr(df)
