library(purrr)
options(digits = 22)

get_param_modes <- function(instr){instr %/% 10^c(2:4) %% 10}

get_opcode <- function(instr) { instr %% 10^2}

get_n_params <- function(op){ switch(op, 3, 3, 1, 1, 2, 2, 3, 3, 1) }

get_next_intruction_pointer <- function(instruction_pointer, op){
  instruction_pointer + get_n_params(op) + 1
}

write_param <- function(param, param_mode, relative_base){
  if(param_mode == 0){
    param + 1
  } else if (param_mode == 2){
    param + 1 + relative_base
  }
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

add <- function(program, params, param_modes, relative_base ) {
  
  a <- get_param(program, param_modes[1], params[1], relative_base)
  b <- get_param(program, param_modes[2], params[2], relative_base)
  
  program[write_param(params[3], param_modes[3], relative_base)] <- a + b
  program
}



multiply <- function(program, params, param_modes, relative_base ) {
  
  a <- get_param(program, param_modes[1], params[1], relative_base)
  b <- get_param(program, param_modes[2], params[2], relative_base)
  
  program[write_param(params[3], param_modes[3], relative_base)] <- a * b
  program
}



less_than <- function(program, params, param_modes, relative_base ) {
  
  a <-  get_param(program, param_modes[1], params[1], relative_base )
  b <- get_param(program, param_modes[2], params[2], relative_base)
  
  program[write_param(params[3], param_modes[3], relative_base)] <- if(a < b) 1 else 0
  program
}

equals <- function(program, params, param_modes, relative_base ) {
  
  a <-  get_param(program, param_modes[1], params[1], relative_base)
  b <- get_param(program, param_modes[2], params[2], relative_base)
  
  program[write_param(params[3], param_modes[3], relative_base)] <- if(a == b) 1 else 0
  program
}

get_input <- function(program, params, input, param_modes, relative_base ) {
  
  
  program[write_param(params[1], param_modes[1], relative_base)] <- input[1]
  
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
                         opcode, relative_base){
  
  a <- get_param(program, param_modes[1], params[1], relative_base)
  
  b <- get_param(program, param_modes[2], params[2], relative_base)
  
  if (a != 0) { b + 1 } else {
    get_next_intruction_pointer(instruction_pointer, opcode)}
  
}

jump_if_false <- function(program, params, param_modes, instruction_pointer,
                          opcode, relative_base){
  a <- get_param(program, param_modes[1], params[1], relative_base)
  
  b <- get_param(program, param_modes[2], params[2], relative_base)
  
  if (a == 0) { b + 1 } else {
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
  
  if(opcode == 3 & length(input) == 0) {
    return(list(program = program,
                instruction_pointer = instruction_pointer, 
                output = output,
                relative_base = relative_base))}
  
  
  param_modes <- get_param_modes(instruction)
  params <- program[instruction_pointer + 1:get_n_params(opcode)]
  
  program <- if(opcode == 1){
    
    add(program, params, param_modes, relative_base)
  } else if(opcode == 2) {
    
    multiply(program, params, param_modes, relative_base)
  } else if(opcode == 3) {
    i <- input[1]
    input <- input[-1]
    get_input(program, params , i, param_modes, relative_base)
    
  } else if(opcode == 7) {
    less_than(program, params , param_modes , relative_base)
  } else if(opcode == 8) {
    
    equals(program, params , param_modes, relative_base )
  } else program
  
  output <-  if(opcode == 4) {
    
    set_output(program, params, param_modes, output, relative_base)
  } else output
  
  instruction_pointer <-  if(opcode == 5){
    jump_if_true(program, params, param_modes,instruction_pointer, opcode,
                 relative_base)
  } else  if(opcode == 6){
    jump_if_false(program, params, param_modes,instruction_pointer, opcode,
                  relative_base)
  } else {
    get_next_intruction_pointer(instruction_pointer, opcode)
  }
  
  relative_base <- if(opcode == 9){
    
    adjust_relative_base(relative_base, params, param_modes, program)
  } else relative_base
  
  # if( opcode == 3 & !is.null(input)){
  #  input <-  NULL}
  
  intcode(program, input, output, instruction_pointer, relative_base )
  
  
}

tr_intcode <- tailr::loop_transform(intcode)