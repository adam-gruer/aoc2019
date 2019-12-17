library(magrittr)
source("intocde.R")

code2char <- function(code) rawToChar(as.raw(code))

ascii_program <- scan("17_input.txt", sep = ",")
camera_image <- tr_intcode(ascii_program)
camera_image <- code2char(camera_image) %>% 
  strsplit("\n+") %>% 
  unlist(use.names = FALSE) %>% 
  sapply(FUN = function(line) {strsplit(line, "") %>%
  unlist(use.names = FALSE)})
dimnames(camera_image) <- NULL


scaffolds <- which(camera_image == "#" & 
                     !row(camera_image) %in% c(1, nrow(camera_image)) & 
                     !col(camera_image) %in% c(1, ncol(camera_image)),
                   arr.ind = TRUE)

is_intersection <- function(coord){
  (camera_image[matrix(coord + 
                        c(1,0, -1 , 0, 0,-1, 0,1),
                      byrow = TRUE, 
                      ncol = 2)] == "#") %>% all()
  
}

intersections <- scaffolds[ apply(scaffolds, 1, is_intersection), ] - 1
part1 <- sum(intersections[,1] * intersections[,2])
part1




