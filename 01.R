

library(purrr)
library(magrittr)
mass <-scan("01_input.txt")

fuel <- function(mass){
  # integer division
  mass %/% 3 - 2
}

part1 <- sum(fuel(mass))
part1

## part 2

fuel2 <- function(mass){
  # base case
  if (mass %/% 3 <= 2) return(0)
  
  # pretend you are done
  c(fuel(mass), fuel2(fuel(mass))) %>% sum()
}

 
part2 <- purrr::map_dbl(mass, fuel2) %>% sum()
part2
