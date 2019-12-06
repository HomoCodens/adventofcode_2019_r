# TODO:
# * document these

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

  invisible(state$tape)
}

advanceState <- function(state,
                         iccin,
                         iccout,
                         debug) {
  tape <- state$tape
  pos <- state$pos

  debug(sprintf("Head at %d", pos), 3)
  instruction <- tapeGet(tape, pos)
  debug(sprintf("Got instruction %d", instruction), 4)
  opcode <- instruction %% 100
  debug(sprintf("Executing opCode %d", opcode), 1)

  if(opcode == 99) {
    debug("HALT!!!", 1)
    state$halt <- TRUE
    state
  } else {
    opcodeNParams <- c(
      3,
      3,
      1,
      1,
      2,
      2,
      3,
      3
    )

    nParams <- opcodeNParams[opcode]
    paramModes <- rev(((instruction %/% 100) %% 10^(nParams:1)) %/% 10^((nParams-1):0))
    debug(sprintf("Param modes: %s", paste(paramModes, collapse = ",")), 4)


    paramIdx <- ((pos + 1):(pos + nParams))
    debug(paste0("params at: ", paste(paramIdx, collapse = ",")), 4)
    params <- tapeGet(tape, paramIdx)
    debug(paste0("params: ", paste(params, collapse = ",")), 2)
    args <- tapeGet(tape, params, paramModes)
    debug(paste0("args: ", paste(args, collapse = ",")), 2)

    switch(
      # Oh R, you silly stupid language, you...
      # Cast the opcode as character because otherwise multi-digit codes won't work
      as.character(opcode),
      "1" = {
        debug(sprintf("%d + %d -> %d", args[1], args[2], params[3]), 2)

        list(
          tape = tapeSet(
            tape,
            params[3],
            args[1] + args[2]),
          pos = pos + 4,
          halt = FALSE
        )
      },
      "2" = {
        debug(sprintf("%d * %d -> %d", args[1], args[2], params[3]), 2)

        list(
          tape = tapeSet(
            tape,
            params[3],
            args[1] * args[2]),
          pos = pos + 4,
          halt = FALSE
        )
      },
      "3" = {
        value <- iccin()  # I'm giggling like a stupid person here ^^
        debug(sprintf("Read value %d -> %d", value, params[1]), 2)
        list(
          tape = tapeSet(
            tape,
            params[1],
            value),
          pos = pos + 2,
          halt = FALSE
        )
      },
      "4" = {
        debug(sprintf("Writing %d to iccout", args[1]), 2)
        iccout(args[1])
        state$pos <- pos + 2
        state
      },
      "5" = {
        debug(sprintf("Checking whether %d != 0", args[1]), 2)
        state$pos <- ifelse(args[1] != 0, args[2], pos + 3)
        state
      },
      "6" = {
        debug(sprintf("Checking whether %d == 0", args[1]), 2)
        state$pos <- ifelse(args[1] == 0, args[2], pos + 3)
        state
      },
      "7" = {
        debug(sprintf("%d < %d -> %d", args[1], args[2], params[3]), 2)
        list(
          tape = tapeSet(tape,
                         params[3],
                         ifelse(args[1] < args[2], 1, 0)),
          pos = pos + 4,
          halt = FALSE
        )
      },
      "8" = {
        debug(sprintf("%d == %d -> %d", args[1], args[2], params[3]), 2)
        list(
          tape = tapeSet(tape,
                         params[3],
                         ifelse(args[1] == args[2], 1, 0)),
          pos = pos + 4,
          halt = FALSE
        )
      }
    )
  }
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
tapeGet <- function(tape, parameters, modes = numeric(length(parameters))) {
  # That's cheating a little but there should not be any negative params with mode == 0
  ifelse(modes == 0, tape[abs(parameters) + 1], parameters)
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


#' Create an ICC input "tape"
#'
#' Create a function that can be used as an iccin.
#' If loop is true the values will repeated, if false
#' the function will throw an error if it is called more
#' often than there are values to supply
#'
#' @param values Values to feed to input
#' @param loop Should the values repeat?
iccInput <- function(values, loop = FALSE) {
  i <- 1
  nValues <- length(values)

  function() {
    if(i > nValues) {
      if(loop) {
        i <- 1
      } else {
        stop(sprintf("Out of values to input!"))
      }
    }

    out <- values[i]
    i <<- i + 1

    out
  }
}

#' Collects output values of an ICC
#'
#' Because it might come in handy at some point?
#' The actual output function to be passed to the computer
#' is in the acc property.
#'
#' @param print Should output values be printed as they are received?
#'
#' @return An iccOutputAccumulator object
#' @export
#'
#' @examples
#' o <- iccOutputAccumulator()
#' o$acc(1)
#' o$acc(101)
#' getOutput(o)
iccOutputAccumulator <- function(print = FALSE) {
  values <- c()

  out <- list(acc = function(x) {
    if(print) {
      message(x)
    }

    values <<- c(values, x)
  })

  class(out) <- "iccOutputAccumulator"
  out
}

#' Get values from an iccOutputAccumulator
#'
#' @param accumulator
#'
#' @return A vector of values that were passed to the output function
#' @export
#'
#' @examples
getOutput <- function(accumulator) {
  as.list(environment(accumulator$acc))$values
}
