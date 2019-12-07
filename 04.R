library(purrr)

# convert number to vector of digits
#integer division then modulo division by 10 on result
# return a vector of digits
digits <- function(number, units){ number %/% units %% 10}

has_adjacents <- function(x){
  # entire vector processed no adjacents found
  if(length(x) == 1) return(FALSE)
  # if first item is equal to second an adjacent has been found
  if(x[1] == x[2]) return(TRUE)
  # if no adjacent found process remainder of vector
  has_adjacents(x[-1])
}

is_increasing <- function(x){
  # end of vector reached, all values in ascending value
  if(length(x) == 1)return(TRUE)
  # if first value is greater than second value then not in ascending order
  if(x[1] > x[2]) return(FALSE)
  # if first value not greater then second value process rest of vector
  is_increasing(x[-1])
}

# perform run length encoding
# test if any of the runs have a length of 2
has_adjacent <- function(x){
  (rle(x)$lengths == 2) %>% any()
  
}

min <- 359282
max <- 820401

#convert to digits
dig <- map(min:max, digits, units = 10^c(5:0))

increasing <- dig[map_lgl(dig, is_increasing)]
valid_password <- increasing[map_lgl(increasing, has_adjacents)]
part1 <- length(valid_password)
part1


part2 <- map_lgl(valid_password, has_adjacent) %>% sum()
part2

