
source("intocde.R")
library(ggplot2)
library(dplyr)
library(gganimate)
library(vapoRwave)

get_screen <- function(game){
  matrix(game$output, ncol = 3, byrow = TRUE)
}

get_pos <- function(screen, object){
  object <- switch(object,
                   ball = 4,
                   paddle = 3)
  object <- screen[which(screen[, 3] == object), ]
  #complex(real = object[1] , imaginary =  object[2])
  object[1]
}

update_screen <- function(screen, updates){

  apply(updates, 1, function(update){
    screen[screen[, 1] == update[1] & screen[,2] == update[2], 3]<<- update[3]
  })
  screen
}

joystick_input <- function(screen){
  ball_pos <- get_pos(screen,"ball")
  paddle_pos <- get_pos(screen,"paddle")
  diff <- ball_pos -  paddle_pos
  if(diff != 0) diff / abs(diff) else diff
}

plot_screen <- function(screen){

  if(class(screen) == "numeric"){
    screen <- matrix(screen, ncol = 3, byrow = TRUE) } else 
      if (class(screen) != "matrix") stop("Screen must be a numeric vector or a matrix")
  
  screen <- as.data.frame(screen,
                          stringsAsFactors = FALSE) %>% 
    rename(x = V1, y = V2, value = V3) %>% 
    mutate(y = - y ,
           value = factor(value))
  
  score <- filter(screen, x == -1) %>% pull(value)
  screen <- filter(screen , x != -1)
  
  ggplot(screen)  +
    geom_tile(aes(x = x, y = y, fill = value), show.legend = FALSE) +
    geom_text(x = 20,
              y = - 1,
              colour = "#ECE976",
             size = 8,
             label = paste("Score: ", score)) +
    theme_void() +
    scale_fill_hyperBubble() +
    coord_equal()
}


animate_recording <- function(screen_recording){
  if (class(screen_recording) != "matrix") stop("Screen must be a numeric vector or a matrix")
  
  screen_recording <- as.data.frame(screen_recording,
                                    stringsAsFactors = FALSE) %>% 
    rename(x = V1, y = V2, value = V3, frame = V4) 
  
  score <- filter(screen_recording, x == -1) 
  
  screen_recording <- filter(screen_recording , x != -1) %>% 
    mutate(y = - y ,
           value = factor(value)) 
  
  
  ggplot(screen_recording)  +
    geom_tile(aes(x = x, y = y, fill = value), show.legend = FALSE) +
    geom_text(data = score,
              x = 20,
              y = - 1,
              colour = "#ECE976",
              size = 12,
              aes(label = paste("Score: ", value))) +
    theme_void() +
    scale_fill_hyperBubble() + 
    coord_equal() +
    transition_time(frame)
}

#####input
input <- scan("13_input.txt", sep = ",")


#####part1
game <- tr_intcode(input)
screen <- matrix(game, ncol = 3, byrow = TRUE)
part1 <- (screen[,3] == 2 )%>% sum()
part1
plot_screen(screen)



#####part 2

# free play mode
free_play <- c(2, input[-1])

#start game

game <- tr_intcode(free_play)
screen <- get_screen(game)
frame <- 1
screen_recording <- matrix(nrow = 5339 * 961, ncol = 4)
screen_recording[seq(from = frame * 961 - 960, to = frame * 961 ), ] <-  cbind(screen, frame)

#gameloop
while(is.list(game)){
frame <- frame + 1
game <- tr_intcode(game$program,
                   input = joystick_input(screen),
                   instruction_pointer = game$instruction_pointer,
                   relative_base = game$relative_base)

if(is.list(game)) {
screen <- update_screen(screen, get_screen(game))
} else  { 
  screen <- update_screen(screen, matrix(game, ncol = 3, byrow = TRUE))
}

screen_recording[seq(from = frame * 961 - 960, to = frame * 961 ), ] <-  cbind(screen, frame)

}

part2 <- screen[screen[, 1] == -1, 3]
part2

plot_screen(screen)

anim <- animate_recording(screen_recording[screen_recording[,4] %in% 250:350,])

animate(anim, fps = 10, nframes = 100)
anim_save("breakout.gif")
