library(magrittr)
orbits <- scan("06_input.txt", what = character())
network <- stringr::str_split(orbits, "\\)") %>% 
  purrr::map_dfr(~ tibble(from = .x[2], to = .x[1]))

d <- igraph::graph_from_data_frame(network)

sum(igraph::distance_table(d)$res)
