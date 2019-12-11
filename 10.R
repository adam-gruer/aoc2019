
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


##alternate
angle_between_cmplex <- function(a, b){
  Arg(a - b) 
}

n_unique_lines_of_sight <- function(a, b){
  angle_between_cmplex(a, b) %>% 
  unique() %>% 
  length()
}


asteroids <- which(asteroid_map == "#", arr.ind = TRUE)
colnames(asteroids) <- c("y","x")

asteroids_cmplx <- complex(real = asteroids[,"x"], imaginary = asteroids[,"y"])


part1 <- imap_dbl(asteroids_cmplx, function(asteroid,i, asteroids){
    n_unique_lines_of_sight(asteroid, asteroids[-i]) 
  }, asteroids = asteroids_cmplx) %>%
  max()
part1

##Part 2
station <- 
  imap_dbl(asteroids_cmplx, function(asteroid,i, asteroids){
    n_unique_lines_of_sight(asteroid, asteroids[-i]) 
  }, asteroids = asteroids_cmplx) %>%
  which.max()

target_asteroids <- asteroids_cmplx[-station]
diff_from_station <- target_asteroids  - asteroids_cmplx[station]

asteroids.df <- data.frame(asteroid = target_asteroids,
                           diff_from_station = diff_from_station, 
                           angle_r = Arg(diff_from_station) ,
                           distance = Mod(diff_from_station))
asteroids.df$angle_d <- (asteroids.df$angle_r  * 180/pi)

ast_200 <- asteroids.df %>% 
  mutate(heading = dplyr::case_when(
    angle_r >= - (pi /2) & angle_r <= 0 ~  angle_d + 90,
    angle_r > 0 ~ abs(angle_r *180/pi) + 90,
    TRUE ~ 180 + (angle_r *180/pi) + 270
  )) %>% 
  arrange(heading, distance) %>% 
  group_by(heading) %>% 
  tidyr::nest() %>% 
  ungroup() %>% 
  slice(200) %>% 
  mutate(aster = map(data, function(df){
    df[1, ] %>% pull(asteroid)
  })) %>% 
  pull (aster) %>% 
  unlist()

x <- Re(ast_200) - 1
y <-  Im(ast_200) - 1
x * 100 + y