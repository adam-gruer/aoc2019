library(magrittr)
library(purrr)
library(stringr)

gravity <- function(pos_a, pos_b){
  if(pos_b > pos_a ) 1 else {
    if(pos_b < pos_a )  -1 else 0
  }
}

apply_gravity <- function(moons){
  if(length(moons) != 2  ) stop(("please supply a lsit of two moons"))
  moon_a <- moons[[1]]
  moon_b <- moons[[2]]
  
  velocity_changes_a <- map2_dbl(moon_a$position,
             moon_b$position,
             gravity)
  moon_a$velocity <-  moon_a$velocity + velocity_changes_a
  moon_b$velocity <-  moon_b$velocity - velocity_changes_a
  
  list(moon_a, moon_b)
}


apply_velocity <- function(moon){
  moon$position <-  moon$position + moon$velocity
  moon
}

kinetic_energy <- function(moon){
  abs(moon$velocity) %>% sum()
}

potential_energy <- function(moon){
  abs(moon$position) %>% sum()
}

total_energy <- function(moon){
  kinetic_energy(moon) * potential_energy(moon)
}

moons <- list(
  io = list(position = c(x = -1, y = 0, z = 2),
            velocity = c(x = 0, y = 0, z = 0 ) ), 
  europa = list(position = c(x = 2, y = -10, z = -7),
                velocity = c(x = 0, y = 0, z = 0 ) ),
  ganymede =list(position = c(x = 4, y = -8, z = 8),
                 velocity = c(x = 0, y = 0, z = 0 ) ),
  callisto = list(position = c(x = 3, y = 5, z = -1),
                  velocity = c(x = 0, y = 0, z = 0 ) )
)


moons <- readLines("12_input.txt") %>% 
  str_extract_all("-*\\d+") %>% 
  map(as.integer) %>% 
  map(function(scan){
    names(scan) <- c("x", "y","z")
    list(position = scan,
         velocity = c(x = 0, y = 0, z = 0 ))
    
  })

pairs <- lower.tri(matrix(nrow = length(moons), ncol = length(moons))) %>% 
  which(arr.ind = TRUE)

step <- function(){
  
  apply(pairs, MARGIN = 1, function(pair){
    moons[pair] <<-  apply_gravity(moons[pair])})
    
  moons <<- lapply(moons, apply_velocity)
}

replicate(1000, step())

part1 <- map_dbl(moons, total_energy) %>% sum()
part1

######## part 2

extract_dimension <- function(dimension, moons){
  original_pos <- map_dbl(moons,c("position", dimension) )
  original_vel <- map_dbl(moons,c("velocity", dimension) )
  matrix(c(original_pos, original_vel), nrow = 4)
}

gcd2 <- function(a, b) {
  if (b == 0) a else gcd2(b, a %% b)
}

#gcd <- function(...) Reduce(gcd2, c(...))

lcm2 <- function(a, b){ a * b /  gcd2 ( a , b )}
lcm <- function(...) Reduce(lcm2, c(...))



originals <- sapply(c("x","y","z"), 
       FUN = extract_dimension,
       moons = moons,
       USE.NAMES = TRUE,
       simplify = FALSE
       )

cycles <- c(x = 0, y = 0, z = 0)
step_cnt <- 0

while (!all(cycles != 0)){
  step_cnt <- step_cnt + 1
  dims <- names(cycles)[cycles == 0]
  step()
  current <- sapply(c("x","y","z"), 
                  FUN = extract_dimension,
                  moons = moons,
                  simplify = FALSE,
                  USE.NAMES = TRUE)
                  
 
  cycles[dims[which(mapply(identical,
                      originals[dims],
                      current[dims]),
               arr.ind = TRUE
               )]] <- step_cnt

}

part2 <- lcm(cycles)
part2
##TODO don't calculate steps for dimensions already done

