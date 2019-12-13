#' Run a program on an ICC
#'
#' @param computers list Pronounced COMputers. List of list(tape, input, output, done)
#' @param debug integer Debug level (default 0)
#'
#' @export
runIntCodeComputer <- function(computers,
                               debug = 0) {

  # General purpoise debug function so the code does not get
  # cluttered with if(debug >= xx)
  # Handles strings and other objects
  debugFcn <- function(x, level) {
    if(level <= debug) {
      if(is.character(x)) {
        message(x)
      } else {
        print(x)
      }
    }
  }

  # Initialize states
  for(i in seq_along(computers)) {
    computers[[i]]$state <- list(
      tape = computers[[i]]$tape,
      pos = 0,
      relBase = 0,
      halt = FALSE,
      needValue = FALSE,
      valueOut = NULL
    )
  }

  while(!all(sapply(computers, function(x) { x$state$halt }))) {
    for(i in seq_along(computers)) {
      if(computers[[i]]$state$halt) {
        next;
      }

      vout <- computers[[i]]$state$valueOut
      if(!is.null(vout)) {
        computers[[i]]$iccout(vout)
        computers[[i]]$state$valueOut <- NULL
      }

      if(computers[[i]]$state$needValue) {
        vin <- computers[[i]]$iccin()
        if(!is.null(vin)) {
          computers[[i]]$state$valueIn <- vin
        }
      }

      computers[[i]]$state <- advanceState(computers[[i]]$state, debugFcn)

      if(computers[[i]]$state$halt) {
        if(!is.null(computers[[i]]$done)) {
          computers[[i]]$done(computers[[i]]$state$tape)
        }
      }
    }
  }

  computers
}

# Main driver
advanceState <- function(state, debug) {
  tape <- state$tape
  pos <- state$pos
  relBase <- state$relBase

  debug(tape, 10)

  debug(sprintf("Head at %.0f", pos), 3)
  debug(sprintf("Relative base at %.0f", relBase), 3)

  instruction <- tapeGet(tape, pos)
  debug(sprintf("Got instruction %.0f", instruction), 4)

  opcode <- instruction %% 100
  debug(sprintf("Executing opCode %.0f", opcode), 1)

  if(opcode == 99) {
    debug("HALT!!!", 1)
    state$halt <- TRUE
  } else {

    opcodeNParams <- c(
      3,
      3,
      1,
      1,
      2,
      2,
      3,
      3,
      1
    )

    nParams <- opcodeNParams[opcode]
    paramModes <- rev(((instruction %/% 100) %% 10^(nParams:1)) %/% 10^((nParams-1):0))
    debug(sprintf("Param modes: %s", paste(paramModes, collapse = ",")), 4)


    paramIdx <- ((pos + 1):(pos + nParams))
    debug(paste0("params at: ", paste(paramIdx, collapse = ",")), 4)

    params <- tapeGet(tape, paramIdx)
    debug(paste0("params: ", paste(params, collapse = ",")), 2)

    args <- tapeGet(tape, params, paramModes, relBase)
    debug(paste0("args: ", paste(args, collapse = ",")), 2)

    switch(
      # Oh R, you silly stupid language, you...
      # Cast the opcode as character because otherwise multi-digit codes won't work
      as.character(opcode),

      # Addition
      "1" = {
        to <- params[3] + ifelse(paramModes[3] == 0, 0, relBase)

        debug(sprintf("%.0f + %.0f -> %.0f", args[1], args[2], to), 2)

        state$tape <- tapeSet(
          tape,
          to,
          args[1] + args[2])

        state$pos <- pos + 4
      },

      # Multiplication
      "2" = {
        to <- ifelse(paramModes[3] == 0, params[3], params[3] + relBase)

        debug(sprintf("%.0f * %.0f -> %.0f", args[1], args[2], to), 2)

        state$tape <- tapeSet(
          tape,
          to,
          args[1] * args[2])

        state$pos <- pos + 4
      },

      # Input

      "3" = {
        if(is.null(state$valueIn)) {
          state$needValue <- TRUE
        } else {
          to <- ifelse(paramModes[1] == 0, params[1], params[1] + relBase)

          state$tape <- tapeSet(
            tape,
            to,
            state$valueIn)

          state$valueIn <- NULL
          state$needValue <- FALSE

          state$pos <- pos + 2
        }
      },

      # Output
      "4" = {
        state$valueOut <- args[1]
        state$pos <- pos + 2
      },

      # Jump if true
      "5" = {
        debug(sprintf("Checking whether %.0f != 0", args[1]), 2)
        state$pos <- ifelse(args[1] != 0, args[2], pos + 3)
      },

      # Jump if false
      "6" = {
        debug(sprintf("Checking whether %.0f == 0", args[1]), 2)
        state$pos <- ifelse(args[1] == 0, args[2], pos + 3)
      },

      # Compare less
      "7" = {
        to <- ifelse(paramModes[3] == 0, params[3], params[3] + relBase)

        debug(sprintf("%.0f < %.0f -> %.0f", args[1], args[2], to), 2)

        state$tape <- tapeSet(tape,
                              to,
                              ifelse(args[1] < args[2], 1, 0))

        state$pos <- pos + 4
      },

      # Compare equal
      "8" = {
        to <- ifelse(paramModes[3] == 0, params[3], params[3] + relBase)

        debug(sprintf("%.0f == %.0f -> %.0f", args[1], args[2], to), 2)

        state$tape <- tapeSet(tape,
                              to,
                              ifelse(args[1] == args[2], 1, 0))

        state$pos <- pos + 4
      },

      # Adjust relative base
      "9" = {
        debug(sprintf("relBase %.0f + %.0f -> %.0f", relBase, args[1], relBase + args[1]), 2)

        state$relBase <- relBase + args[1]

        state$pos <- state$pos + 2
      }
    )
  }

  state
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
tapeGet <- function(tape,
                    parameters,
                    modes = numeric(length(parameters)),
                    base = 0) {
  out <- c()
  for(i in seq_along(parameters)) {
    if(modes[i] == 0) {
      out <- c(out, tape[parameters[i] + 1])
    } else if(modes[i] == 1) {
      out <- c(out, parameters[i])
    } else if(modes[i] == 2) {
      out <- c(out, tape[base + parameters[i] + 1])
    } else {
      stop("Unsupported parameter mode!")
    }
  }

  # In case out of range values were read -> return 0, not NA
  out[is.na(out)] <- 0

  out
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

  # If tape has grown -> set intermediate values to 0 instead of NA
  tape[is.na(tape)] <- 0

  tape
}

readICCTape <- function(f) {
  as.numeric(strsplit(readLines(f), ",")[[1]])
}

# input methods -----------------------------------------------------------

iccFileInput <- function(con) {
  if(!inherits(con, "connection")) {
    stop("Please provide a file connection")
  }

  if(!isOpen(con)) {
    open(con)
  }

  function() {
    readLines(con, 1)
  }
}

iccManualInput <- function() {
  function() {
    done <- FALSE

    while(!done) {
      x <- suppressWarnings(as.numeric(readline("icc> ")))
      done <- !is.na(x) && as.integer(x) == x

      if(!done) {
        message("icc> Nope, please enter an integer...")
      }
    }

    x
  }
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

iccPipe <- function(initialBuffer = NULL, output = NULL) {
  start <- 1
  end <- length(initialBuffer)
  buffer <- initialBuffer

  function(x = NULL) {
    if(!is.null(x)) {
      # Being used as output (data in)

      if(!is.null(output)) {
        output(x)
      }

      buffer <<- c(buffer, x)
      end <<- end + 1
    } else {
      # Being used as input (data out)
      if(end < start) {
        # We have no data ready, register the callback for later use
        NULL
      } else {
        # There is data available, call callback immediately
        out <- buffer[start]
        start <<- start + 1
        out
      }
    }
  }
}


# output methods ----------------------------------------------------------



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
#' o$values()
iccOutputAccumulator <- function(print = FALSE) {
  values <- c()

  out <- list(acc = function(x) {
    if(print) {
      message(x)
    }

    values <<- c(values, x)
  },
  values = function() {
    values
  })

  class(out) <- "iccOutputAccumulator"
  out
}
