library(purrr)

input <- c("12345678")
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


###### part2
###### Some help from online, each digit in the second half of any output
#### is the cumulative sum (from right to left) of the input digits then modulo
## 10. The offset is well over half the input so I can filter out all those
### then reverse the vector and for 100 phases calculate the cumulative sums
## mod 10.  Then reverse the final vector and read off the first 8 digits.

input <- scan("16_input.txt", character())

input <- strsplit(input,"")[[1]]
input <- as.integer(input)
input <- rep(input, times = 10000)

offset <- (input[1:7] * 10^(6:0) )%>% sum()

input <- input[(offset + 1):length(input)]

rev_input <- rev(input)

part2 <- Reduce(function(input, phase){
  cumsum(input) %% 10},
  seq(100),
  init = rev_input) %>% 
  rev()

part2 <- paste0(part2[1:8],collapse = "")
part2
