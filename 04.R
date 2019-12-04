library(purrr)
min <- 359282
max <- 820401
ran <- min:max

#integer division then modulo division by 10 on result
# return a vector of digits
digits <- function(number, units){ number %/% units %% 10}


#if the digitas sorted are the same as original digits then ascending order
is_increasing <- function(number){
 # len <- as.character(number) %>% 
  #          nchar()
  dig <- digits(number, 10^c(5:0))
  identical(dig, sort(dig))
  
}

has_multiple <- function(number){
 # len <- as.character(number) %>% 
  #  nchar()
  dig <- digits(number, 10^c(5:0))
  duplicated(dig) %>% any()
  
}

has_adjacent <- function(number){
  dig <- digits(number, 10^c(5:0))
  (rle(dig)$lengths == 2) %>% any()
  
}
part1 <- ran[map_lgl(ran, ~ is_increasing(.x) && has_multiple(.x))]
length(part1)

part2 <- map_lgl(part1, has_adjacent)
sum(part2)

