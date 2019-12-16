library(purrr)

input <- c("80871224585914546619083218645595")
input <- scan("16_input.txt", character())
input <- strsplit(input,"")[[1]]
input <- as.integer(input)
length_input <- length(input)
original_base_length <- 4

base <- c(0, 1, 0, -1)

pattern_matrix <- sapply(seq(length_input), function(i){
  
  base <- rep(base, each = i)
  if ( i <= length_input / original_base_length){
    c(base[-1], rep_len(base, length_input - length(base)  + 1)) } else
      base[2:(length_input + 1)]
  
}, simplify = TRUE) %>% 
  t()

part1 <- Reduce( function(input, phases){
  sapply(seq(length_input),function(i, input){
  input[i] * pattern_matrix[,i ]
}, input = input) %>%
  rowSums() %>% 
  abs() %% 10
},  seq(100), init = input)
part1 <- part1[1:8] %>% paste0(collapse = "")
part1

input <- rep(input, times = 10000)
length_input <- length(input)
pattern_matrix <- matrix(nrow = length_input, ncol = length_input)
pattern_matrix <- sapply(seq(length_input), function(i){
  
  base <- rep(base, each = i)
  if ( i <= length_input / original_base_length){
    c(base[-1], rep_len(base, length_input - length(base)  + 1)) } else
      base[2:(length_input + 1)]
  
}, simplify = TRUE) %>% 
  t()

part2 <- Reduce( function(input, phases){
  sapply(seq(length_input),function(i, input){
    input[i] * pattern_matrix[,i ]
  }, input = input) %>%
    rowSums() %>% 
    abs() %% 10
},  seq(100), init = input)
