library(purrr)

angle_between <-  function(a, b){
 atan2(b$x-a$x, -(b$y-a$y)) %% (2* pi)
}
 
unique_angles <-  function(asteroid, asteroids){
map_dbl(setdiff(asteroids, list(asteroid)), angle_between, a = asteroid) %>%
    unique() %>% 
    length()  
}



#test <- c(".#..#",".....","#####","....#","...##")
asteroid_map <-  readLines("10_input.txt")
asteroid_map <- stringr::str_split(asteroid_map,"", simplify = TRUE)          
asteroid_map

asteroids <- which(asteroid_map == "#", arr.ind = TRUE)
colnames(asteroids) <- c("y","x")
asteroids <-  apply(asteroids, 1, as.list) 

 
part1 <- map_int(asteroids, unique_angles, asteroids = asteroids)%>%
  max()
part1

c(4,3) %*% c(3,5)
pracma::Norm(c(3,5))

1+1i
4 +3i - (3 + 5i)

pracma::angle(4+4i - (5+4i))



angle_between_cmplex <- function(a, b){
  Arg(a - b) 
}

n_unique_lines_of_sight <- function(a, b){
  angle_between_cmplex(a, b) %>% 
  unique() %>% 
  length()
}


asteroids_cmplx <- complex(real = asteroids[,"x"], imaginary = asteroids[,"y"])


imap_dbl(asteroids_cmplx, function(asteroid,i, asteroids){
    n_unique_lines_of_sight(asteroid, asteroids[-i]) 
  }, asteroids = asteroids_cmplx) %>%
  max()

station <- 
  imap_dbl(asteroids_cmplx, function(asteroid,i, asteroids){
    n_unique_lines_of_sight(asteroid, asteroids[-i]) 
  }, asteroids = asteroids_cmplx) %>%
  which.max()

diff_from_station <- asteroids_cmplx[-station] - asteroids_cmplx[station] 
 
angles <- Arg(diff_from_station)
distances <- sqrt(Re(diff_from_station)^2 + Im(diff_from_station)^2 )

asteroids_cmplx[-station]

asteroids_cmplx[-station][order(distances)][order(angles - Arg(0+1i))]

angles / Arg(0+1i)


test <- c(".#....#####...#..",
"##...##.#####..##",
"##...#...#.#####.",
"..#.....X...###..",
"..#.#.....#....##")

test <- stringr::str_split(test,"", simplify = TRUE)          
test

asteroids_t <- which(test == "#", arr.ind = TRUE)
colnames(asteroids_t) <- c("y","x")

station <- which(test == "X", arr.ind = TRUE)
colnames(station) <- c("y","x")
station

asteroids_t_c <- complex(real = asteroids_t[,"x"], imaginary = asteroids_t[,"y"])
station_c <-  complex(real = station[,"x"], imaginary = station[,"y"])
station_c


differ <- station_c - asteroids_t_c 
differ
asteroids.df <- data.frame(asteroid = asteroids_t_c, diff_from_station = differ, angle_r = Arg(differ)    , distance = Mod(differ))
asteroids.df$angle_d <- (asteroids.df$angle_r  * 180/pi)

asteroids.df$angle_d[asteroids.df$angle_d <= 0] <- abs(asteroids.df$angle_d[asteroids.df$angle_d <= 0]) + 90

asteroids.df %>% 
  mutate(new_angle = case_when(
    angle_d <= 0 ~ abs(angle_d) + 90,
    angle_d > 0 & angle_d <= 90 ~ 90 - angle_d,
    angle_d > 90 & angle_d <= 180 ~ angle_d + 180
  ))


asteroids.df %>%
  nest(angle = angle) %>% 
   arrange(distance, .by_group = TRUE) %>%  View()
library(tidyr)


range(asteroids.df$angle)


tapply(asteroids_t_c, list(Arg(differ) -Arg(0 + 1i)), FUN = list) -> same_angle
