asteroid_map <- readLines("10_input.txt")
asteroid_map <- stringr::str_split(asteroid_map,"")
asteroid_map <- unlist(asteroid_map)
asteroid_map <- matrix(asteroid_map, nrow = 25, ncol = 25, byrow = TRUE)
asteroid_map

which(asteroid_map == "#") -> ind


test = ".#..#
.....
#####
....#
...##"

test <- stringr::str_split(test, "\n")
test
test <- stringr::str_split(test[[1]], "")
test <- unlist(test)
test <- matrix(test, nrow = 5, ncol = 5, byrow = TRUE)
which(test == "#", arr.ind = TRUE) -> ind
df <- as.data.frame(ind)
function(x){
pt1 <- df[x,]
df_1 <- df[-x,]
df_1$row <- abs(df_1$row - pt1$row)
df_1$col <- abs(df_1$col - pt1$col)
any(df_1$row == 0)
gcds <- purrr::pmap_dbl(df_1, function(row,col){gcd2(row,col)})
sum(gcds == 1)
}
ind[-1,]  
gcd2 <- function(a, b) {
  if (b == 0) a else Recall(b, a %% b)
}

gcd2(50,2)

c(1,2) 
c(1,5)
c(3,)

Matrix::


