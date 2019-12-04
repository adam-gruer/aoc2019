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
         U = list())
  
}


 

