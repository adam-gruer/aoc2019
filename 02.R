
library(purrr)
program <- scan("02_input.txt", sep = ",")

operation <- function(program, line) {
  op <- line[1]
  input1_idx <- line[2] + 1
  input2_idx <- line[3] + 1
  output_idx <- line[4] + 1

  input1_val <- program[input1_idx]
  input2_val <- program[input2_idx]

  program[output_idx] <- switch(op,
    input1_val + input2_val,
    input1_val * input2_val
  )

  program
}

run_program <- function(program, noun, verb) {
  program[2] <- noun
  program[3] <- verb

  start_idx <- 1

  while (program[start_idx] != 99) {
    program <- operation(program, program[start_idx:(start_idx + 3)])
    start_idx <- start_idx + 4
  }

  program[1]
}

part1 <- run_program(program, noun = 12, verb = 2)
part1

combinations <- cross(list(0:99, 0:99))
runs <- map_dbl(combinations, ~ run_program(program, noun = .x[[1]], verb = .x[[2]]))

part2 <- combinations[which(runs == 19690720)] %>% unlist()
part2 <- 100 * part2[1] + part2[2]
part2
