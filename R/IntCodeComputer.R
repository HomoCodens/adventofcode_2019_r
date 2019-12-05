# TODO:
# * document these
# * improve debugging (i.e. print current instruction etc.)

runIntCodeComputer <- function(tape,
                               noun = NULL,
                               verb = NULL,
                               iccin = function(){0},
                               iccout = function(x){},
                               pos = 0,
                               debug = 0) {

  debugFcn <- function(x, level) {
    if(level <= debug) {
      if(is.character(x)) {
        message(x)
      } else {
        print(x)
      }
    }
  }

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
    state <- advanceState(state, iccin, iccout, debugFcn)

    debugFcn(state, 10)

    !state$halt
  }){
  }

  state$tape
}

advanceState <- function(state,
                         iccin,
                         iccout,
                         debug) {
  tape <- state$tape
  pos <- state$pos

  # Let's assume there are no more than 3 parameters
  # Easy to extend anyway
  nParams <- 3
  instruction <- tapeGet(tape, pos)
  opcode <- instruction %% 100
  paramModes <- rev(((instruction %/% 100) %% 10^(nParams:1)) %/% 10^((nParams-1):0))

  debug(sprintf("Head at %d", pos), 3)
  debug(sprintf("Got instruction %d", instruction), 4)
  debug(sprintf("Param modes: %s", paste(paramModes, collapse = ",")), 4)
  debug(sprintf("Executing opCode %d", opcode), 1)

  out <- switch(
    # Oh R, you silly stupid language, you...
    # Cast the opcode as character because otherwise multi-digit codes won't work
    as.character(opcode),
    "1" = {
      debug(paste(tapeGet(tape, pos:(pos + 3)), collapse = ","), 2)

      list(
        tape = tapeSet(
          tape,
          tapeGet(tape, pos + 3),
          tapeGet(tape,
                  tapeGet(tape, pos + 1, paramModes[1])) +
            tapeGet(tape,
                    tapeGet(tape, pos + 2, paramModes[2]))),
        pos = pos + 4,
        halt = FALSE
      )
    },
    "2" = {
      debug(paste(tapeGet(tape, pos:(pos + 3)), collapse = ","), 2)

      list(
        tape = tapeSet(
          tape,
          tapeGet(tape, pos + 3),
          tapeGet(tape,
                  tapeGet(tape, pos + 1, paramModes[1])) *
            tapeGet(tape,
                    tapeGet(tape, pos + 2, paramModes[2]))),
        pos = pos + 4,
        halt = FALSE
      )
    },
    "3" = {
      debug(paste(tapeGet(tape, pos:(pos + 1)), collapse = ","), 2)

      list(
        tape = tapeSet(
          tape,
          tapeGet(tape, pos + 1, paramModes[1]),
          iccin()), # I'm giggling like a stupid person here ^^
        pos = pos + 2,
        halt = FALSE
      )
    },
    "4" = {
      debug(paste(tapeGet(tape, pos:(pos + 1)), collapse = ","), 2)

      iccout(tapeGet(tape, tapeGet(tape, pos + 1, paramModes[1])))
      state$pos <- pos + 2
      state
    },
    "99" = {
      list(
        tape = tape,
        pos = pos + 1,
        halt = TRUE
      )
    }
  )

  out
}

# Tape wrappers -----------------------------------------------------------

# Simple getter/setter to make working with 1-indexed Rrays easier

#' Get value from an IntCode tape
#'
#' Get the value stored on tape at the given address
#'
#' @param tape
#' @param parameter
#' @param mode
#'
#' @return The value at address on tape
tapeGet <- function(tape, parameter, mode = 0) {
  if(mode == 0) {
    tape[parameter + 1]
  } else {
    parameter
  }
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
