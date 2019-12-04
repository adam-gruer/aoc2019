library(stringr)


#parse into direction and distance using regex
parse_instruction <- function(instruction){
  list(direction = str_extract(instruction, "[UDLR]"),
       distance = str_extract(instruction, "\\d+") %>% as.integer()
  )
}

wire_paths <- scan("03_input.txt", what =character()) %>% 
  lapply( strsplit, split = ",") %>% 
  lapply(unlist) %>% 
  lapply(function(instructions) {parse_instruction(instructions)})

move <- function(start, direction, distance){
        switch(direction,
               U = list(x = start$x, y = start$y + distance),
               D = list(x = start$x, y = start$y - distance),
               R = list(x = start$x + distance, y = start$y),
               L = list(x = start$x - distance, y = start$y))
  
}


point <- list(x = 0, y = 0)
 
U7

point2 <-  list(x = point$x, y = point$y + 7)
move(point, U7)

 

