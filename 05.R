
library(purrr)
program <- scan("05_input.txt", sep = ",")

get_param_modes <- function(instr){instr %/% 10^c(2:4) %% 10}
get_opcode <- function(instr) {instr %% 10^2}
get_n_params <- function(op){ switch(op, 3, 3, 1, 1, 2, 2, 3, 3) }
get_next_intruction_pointer <- function(instruction_pointer, op){
  instruction_pointer + get_n_params(op) + 1
}

add <- function(program, params, param_modes ) {
  
  a <- if(param_modes[1] == 0){
    program[params[1] + 1]
  } else {
    params[1]
  }
  
  b <- if(param_modes[2] == 0){
    program[params[2] + 1]
  } else {
    params[2]
  }
  program[params[3] + 1] <- a + b
  program
}

multiply <- function(program, params, param_modes ) {
  
  a <- if(param_modes[1] == 0){
    program[params[1] + 1]
  } else {
    params[1]
  }
  
  b <- if(param_modes[2] == 0){
    program[params[2] + 1]
  } else {
    params[2]
  }
  program[params[3] + 1] <- a * b
  program
}

less_than <- function(program, params, param_modes ) {
  
  a <- if(param_modes[1] == 0){
    program[params[1] + 1]
  } else {
    params[1]
  }
  
  b <- if(param_modes[2] == 0){
    program[params[2] + 1]
  } else {
    params[2]
  }
  program[params[3] + 1] <- if(a < b) 1 else 0
  program
}

equals <- function(program, params, param_modes ) {
  
  a <- if(param_modes[1] == 0){
    program[params[1] + 1]
  } else {
    params[1]
  }
  
  b <- if(param_modes[2] == 0){
    program[params[2] + 1]
  } else {
    params[2]
  }
  program[params[3] + 1] <- if(a == b) 1 else 0
  program
}

get_input <- function(program, params, input ) {
  
  program[params + 1] <- input
  program
  
}

set_output <- function(program, params, param_modes, output ) {
  output <- c(output,
              if ( param_modes[1] == 0 ){
                program[params + 1]}
              else {
                params
              })
  output
}

jump_if_true <- function(program, params, param_modes, instruction_pointer,
                         opcode){
  a <- if(param_modes[1] == 0){
    program[params[1] + 1]
  } else {
    params[1]
  }
  
  b <- if(param_modes[2] == 0){
    program[params[2] + 1]
  } else {
    params[2]
  }
  
  if (a != 0) {b + 1} else {
    get_next_intruction_pointer(instruction_pointer, opcode)}
  
}

jump_if_false <- function(program, params, param_modes, instruction_pointer,
                         opcode){
  a <- if(param_modes[1] == 0){
    program[params[1] + 1]
  } else {
    params[1]
  }
  
  b <- if(param_modes[2] == 0){
    program[params[2] + 1]
  } else {
    params[2]
  }
  
  if (a == 0) {b + 1} else {
    get_next_intruction_pointer(instruction_pointer, opcode)}
  
}


intcode <- function(program,
                    input = NULL,
                    output = NULL,
                    instruction_pointer = 1){

  
  instruction <- program[instruction_pointer]
  #end of program
  if (instruction == 99) return(output)
  
  opcode <- get_opcode(instruction)
  param_modes <- get_param_modes(instruction)
  params <- program[instruction_pointer + 1:get_n_params(opcode)]
  
  program <- if(opcode == 1){
       add(program, params, param_modes)
  } else if(opcode == 2) {
      multiply(program, params, param_modes)
  } else if(opcode == 3) {
      get_input(program, params , input )
  } else if(opcode == 7) {
    less_than(program, params , param_modes )
  } else if(opcode == 8) {
    equals(program, params , param_modes )
  } else program
  
  output <-  if(opcode == 4) {
    set_output(program, params, param_modes, output)
  } else output
  
  instruction_pointer <-  if(opcode == 5){
    jump_if_true(program, params, param_modes,instruction_pointer, opcode)
  } else  if(opcode == 6){
    jump_if_false(program, params, param_modes,instruction_pointer, opcode)
  } else {
    get_next_intruction_pointer(instruction_pointer, opcode)
   }
  
  intcode(program, input, output, instruction_pointer)
  
  
}


intcode(program, 5)






