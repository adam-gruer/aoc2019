library(magrittr)
sig <- scan("08_input.txt", what = "character")
sig <- stringr::str_split(sig, "")
sig <- unlist(sig)
sig <- as.integer(sig)
x <- array(sig, dim = c(25,6, 100))
x <- aperm(x, c(2,1,3))

fewest_zeroes <- apply(x, 3, function(x){sum(x == 0)}) %>% 
  which.min()

part1 <- sum(x[,,fewest_zeroes]  == 1) * sum(x[,,fewest_zeroes]  == 2) 
part1

x[1,1,][x[1,1,] != 2][1]




      