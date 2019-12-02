day2 <- function(path) {
  microcode <- function(opcode, x, y) {
    switch(opcode,
           "1" = x + y,
           "2" = x * y,
           "99" = NULL)
  }

  initial_tape <- as.numeric(strsplit(readLines(path), ",")[[1]])

  tape <- initial_tape

  # restore 1202 error state
  tape[2] <- 12
  tape[3] <- 2

  pos <- 1
  # This is going to be arduous...
  while(!is.null(result <- microcode(tape[pos], tape[tape[pos + 1] + 1], tape[tape[pos + 2] + 1]))) {
    tape[tape[pos + 3] + 1] <- result
    pos <- pos + 4
  }

  message(paste(tape, collapse = ", "))
  message(tape[1])

  # The Eagle has landed =)
  expected_output <- 19690720
  done <- FALSE

  for(noun in 0:99) {
    for(verb in 0:99) {
      tape <- initial_tape
      tape[2] <- noun
      tape[3] <- verb

      pos <- 1
      while(!is.null(result <- microcode(tape[pos], tape[tape[pos + 1] + 1], tape[tape[pos + 2] + 1]))) {
        tape[tape[pos + 3] + 1] <- result
        pos <- pos + 4
      }

      if(tape[1] == expected_output) {
        message(sprintf("noun: %d, verb: %d\n100*noun + verb: %d", noun, verb, 100*noun + verb))
        done <- TRUE;
        break;
      }
    }

    if(done) {
      break;
    }
  }
}
