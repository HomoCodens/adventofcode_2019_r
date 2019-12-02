runIntCodeComputer <- function(tape,
                               noun = NULL,
                               verb = NULL,
                               pos = 0,
                               debug = 0) {
  if(!is.null(noun)) {
    tape <- tapeSet(tape, 0, noun)
  }

  if(!is.null(verb)) {
    tape <- tapeSet(tape, 1, verb)
  }

  state <- list(
    tape = tape,
    pos = pos,
    debug = debug,
    halt = FALSE
  )

  # Because fun ;P
  while({
    state <- advanceState(state)

    if(debug > 1) {
      print(state)
    }

    !state$halt
  }){
  }

  state$tape
}

advanceState <- function(state) {
  tape <- state$tape
  pos <- state$pos
  debug <- state$debug

  if(debug > 0) {
    message(sprintf("Executing opCode %d", tapeGet(tape, pos)))
  }

  out <- switch(
    # Oh R, you silly stupid language, you...
    # Cast the opcode as character because otherwise multi-digit codes won't work
    as.character(tapeGet(tape, pos)),
    "1" = {
      list(
        tape = tapeSet(
          tape,
          tapeGet(tape, pos + 3),
          tapeGet(tape, tapeGet(tape, pos + 1)) + tapeGet(tape, tapeGet(tape, pos + 2))),
        pos = pos + 4,
        halt = FALSE
      )
    },
    "2" = {
      list(
        tape = tapeSet(
          tape,
          tapeGet(tape, pos + 3),
          tapeGet(tape, tapeGet(tape, pos + 1)) * tapeGet(tape, tapeGet(tape, pos + 2))),
        pos = pos + 4,
        halt = FALSE
      )
    },
    "99" = {
      list(
        tape = tape,
        pos = pos + 1,
        halt = TRUE
      )
    }
  )

  out$debug <- debug

  out
}

# Tape wrappers -----------------------------------------------------------

# Simple getter/setter to make working with 1-indexed Rrays easier

#' Get value from an IntCode tape
#'
#' Get the value stored on tape at the given address
#'
#' @param tape
#' @param address
#'
#' @return The value at address on tape
tapeGet <- function(tape, address) {
  tape[address + 1]
}

#' Store value on a tape
#'
#' Sets the value at address on the tape to value
#'
#' @param tape
#' @param address
#' @param value
#'
#' @return A copy of the tape after setting the value
tapeSet <-  function(tape, address, value) {
  tape[address + 1] <- value
  tape
}
