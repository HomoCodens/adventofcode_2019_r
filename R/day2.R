day2 <- function(path) {
  initial_tape <- as.numeric(strsplit(readLines(path), ",")[[1]])

  tape <- initial_tape

  # restore 1202 error state
  tape[2] <- 12
  tape[3] <- 2

  runIntCodeComputer(list(
    list(
      tape = tape,
      done = function(tape) {
        message(tape[1])
  })))

  # The Eagle has landed =)
  expected_output <- 19690720
  done <- FALSE

  for(noun in 0:99) {
    for(verb in 0:99) {
      tape <- initial_tape

      tape[2] <- noun
      tape[3] <- verb

      # Just run them all. even worse brute force
      runIntCodeComputer(list(
        list(
          tape = tape, done = function(tape) {
            if(tape[1] == expected_output) {
              message(sprintf("noun: %d, verb: %d\n100*noun + verb: %d", noun, verb, 100*noun + verb))
              done <<- TRUE
            }
          })))

      if(done) {
        break;
      }
    }

    if(done) {
      break;
    }
  }
}
