
library(purrr)

program <- scan("07_input.txt", sep = ",")

get_param_modes <- function(instr){instr %/% 10^c(2:4) %% 10}

get_opcode <- function(instr) { instr %% 10^2}

get_n_params <- function(op){ switch(op, 3, 3, 1, 1, 2, 2, 3, 3) }

get_next_intruction_pointer <- function(instruction_pointer, op){
  instruction_pointer + get_n_params(op) + 1
}

get_param <- function(program, param_mode, param) {
 if(param_mode == 0){
   program[param + 1]
 } else {
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

get_input <- function(program, params, input ) {
  
  program[params + 1] <- input[1]

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
  
  if(opcode == 3 & is.null(input)) {
  return(list(program = program, instruction_pointer = instruction_pointer, output = output))}
  
  
  param_modes <- get_param_modes(instruction)
  params <- program[instruction_pointer + 1:get_n_params(opcode)]
  
  program <- if(opcode == 1){
       add(program, params, param_modes)
  } else if(opcode == 2) {
      multiply(program, params, param_modes)
  } else if(opcode == 3) {
        get_input(program, params , input)
      
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
  
 if( opcode == 3 & !is.null(input)){
  input <-  NULL}

  intcode(program, input, output, instruction_pointer )
 
  
}




amp_controller <- function(input, phase){
  intcode(program, c(phase, input))
}

run_sequence <- function(sequence){
  reduce(sequence, amp_controller, .init = 0 )
}

phases <- gtools::permutations(5, 5, 0:4)

part1 <- apply(phases, 1, run_sequence) %>% 
  max()

part1



test_program <- c(3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,
             -5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,
             53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10)

#initialize amp
initialise_amp <- function(program, phase){
  intcode(program, phase)
}
 

test_phase <- function(phase, program){

amps <- lapply(phase, initialise_amp, program = program)



amps[[1]] <- intcode(amps[[1]]$program, input = 0, instruction_pointer = amps[[1]]$instruction_pointer)
 

repeat({
amps[[2]] <- intcode(amps[[2]]$program, input = ifelse(is.list(amps[[1]]),amps[[1]]$output,amps[[1]]), instruction_pointer = amps[[2]]$instruction_pointer)
 

amps[[3]] <- intcode(amps[[3]]$program, input =  ifelse(is.list(amps[[2]]),amps[[2]]$output,amps[[2]]), instruction_pointer = amps[[3]]$instruction_pointer)
 

amps[[4]]<- intcode(amps[[4]]$program, input =  ifelse(is.list(amps[[3]]),amps[[3]]$output,amps[[3]]), instruction_pointer = amps[[4]]$instruction_pointer)
 


amps[[5]]<- intcode(amps[[5]]$program, input =   ifelse(is.list(amps[[4]]),amps[[4]]$output,amps[[4]]), instruction_pointer = amps[[5]]$instruction_pointer)
 
if(!is.list(amps[[5]])) break()

amps[[1]] <- intcode(amps[[1]]$program,  ifelse(is.list(amps[[5]]),amps[[5]]$output,amps[[5]]), instruction_pointer = amps[[1]]$instruction_pointer)
 
}
)

amps[[5]]
}

test_phase(c(9,7,8,5,6), program)


phases <- gtools::permutations(5, 5, 5:9)

part2 <- apply(phases, 1, test_phase, program = program) %>% 
  max()

part2
 








