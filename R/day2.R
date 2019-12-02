day2 <- function(path) {
  microcode <- function(opcode, x, y) {
    switch(opcode,
           "1" = x + y,
           "2" = x * y,
           "99" = NULL)
  }

  tape <- as.numeric(strsplit(readLines(path), ",")[[1]])

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
}
