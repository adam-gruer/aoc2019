library(purrr)
options(digits = 22)

program <- scan("09_input.txt", sep = ",")

get_param_modes <- function(instr){instr %/% 10^c(2:4) %% 10}

get_opcode <- function(instr) { instr %% 10^2}

get_n_params <- function(op){ switch(op, 3, 3, 1, 1, 2, 2, 3, 3, 1) }

get_next_intruction_pointer <- function(instruction_pointer, op){
  instruction_pointer + get_n_params(op) + 1
}

get_param <- function(program, param_mode, param, relative_base = 0) {
  
  if(param_mode == 0){
    param <- program[param + 1]
    ifelse(is.na(param), 0, param)
  } else if (param_mode == 2){
    param <- program[param + relative_base + 1]
    ifelse(is.na(param), 0, param)
  }else {
    param
  }
}

add <- function(program, params, param_modes ) {
  
  a <-  get_param(program, param_modes[1], params[1])
  b <- get_param(program, param_modes[2], params[2])
  
  program[params[3] + 1] <- a + b
  program
}



multiply <- function(program, params, param_modes ) {
  
  a <-  get_param(program, param_modes[1], params[1])
  b <- get_param(program, param_modes[2], params[2])
  
  program[params[3] + 1] <- a * b
  program
}



less_than <- function(program, params, param_modes ) {
  
  a <-  get_param(program, param_modes[1], params[1])
  b <- get_param(program, param_modes[2], params[2])
  
  program[params[3] + 1] <- if(a < b) 1 else 0
  program
}

equals <- function(program, params, param_modes ) {
  
  a <-  get_param(program, param_modes[1], params[1])
  b <- get_param(program, param_modes[2], params[2])
  
  program[params[3] + 1] <- if(a == b) 1 else 0
  program
}

get_input <- function(program, params, input, param_modes, relative_base ) {
  
  intput_ptr <- if(param_modes[1] == 0){
    params + 1} else if(param_modes[1] == 2){
      params + relative_base + 1
    }
  program[intput_ptr] <- input[1]
  
  program
  
}

set_output <- function(program, param, param_mode, output, relative_base ) {
  output <- c(output,
              if(param_mode[1] == 0){
                param <- program[param[1] + 1]
                ifelse(is.na(param), 0, param)
              } else if (param_mode[1] == 2){
                param <- program[param[1] + relative_base + 1]
                ifelse(is.na(param), 0, param)
              }else {
                param
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

adjust_relative_base <- function(relative_base, param, param_mode, program){
  param <- if(param_mode == 0){
    program[param + 1]
  } else if (param_mode == 2){
    program[param + relative_base + 1]
  } else param
  relative_base + param
} 

intcode <- function(program,
                    input = NULL,
                    output = NULL,
                    instruction_pointer = 1,
                    relative_base = 0){
  
  
  instruction <- program[instruction_pointer]
  #end of program
  if (instruction == 99) return(output)
  
  opcode <- get_opcode(instruction)
  
  if(opcode == 3 & is.null(input)) {
    return(list(program = program, instruction_pointer = instruction_pointer, output = output))}
  
  
  param_modes <- get_param_modes(instruction)
  params <- program[instruction_pointer + 1:get_n_params(opcode)]
  
  program <- if(opcode == 1){
 
    add(program, params, param_modes)
  } else if(opcode == 2) {
  
    multiply(program, params, param_modes)
  } else if(opcode == 3) {
    get_input(program, params , input, param_modes, relative_base)
    
  } else if(opcode == 7) {
    less_than(program, params , param_modes )
  } else if(opcode == 8) {
    
    equals(program, params , param_modes )
  } else program
  
  output <-  if(opcode == 4) {
 
    set_output(program, params, param_modes, output, relative_base)
  } else output
  
  instruction_pointer <-  if(opcode == 5){
    jump_if_true(program, params, param_modes,instruction_pointer, opcode)
  } else  if(opcode == 6){
    jump_if_false(program, params, param_modes,instruction_pointer, opcode)
  } else {
    get_next_intruction_pointer(instruction_pointer, opcode)
  }
  
  relative_base <- if(opcode == 9){
 
    adjust_relative_base(relative_base, params, param_modes, program)
  } else relative_base
  
  #if( opcode == 3 & !is.null(input)){
   # input <-  NULL}
  
  intcode(program, input, output, instruction_pointer, relative_base )
  
  
 }

intcode(c(109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99 ))
intcode(program, input = 1)
program
intcode(c(109,1,203,11,209,8,204,1,99,10,0,42,0),10)
