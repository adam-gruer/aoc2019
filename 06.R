library(magrittr)
library(tibble)
library(igraph)

orbits_map <- scan("06_input.txt", what = character())

orbits_edges <- stringr::str_split(orbits_map, "\\)") %>% 
  purrr::map_dfr( ~ tibble( from = .x[2],  to = .x[1]))

orbits_graph <- igraph::graph_from_data_frame(orbits_edges)

part1 <- igraph::distance_table(orbits_graph)$res %>% 
      sum()
part1

part2 <- distances(orbits_graph, 
               v = V(orbits_graph)["YOU"] ,
               to = V(orbits_graph)["SAN"]) - 2
part2
