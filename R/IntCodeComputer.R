#' Run a program on an ICC
#'
#' @param tape integer IntCode program to run
#' @param noun integer Noun for the program (default none)
#' @param verb integer Verb for the program (default none)
#' @param iccin function Input function (See IccInput)
#' @param iccout function Output function (See IccOutput)
#' @param done function Gets called with the final tape state after the computer halts
#' @param pos integer Starting position of the tape head (default 0)
#' @param debug integer Debug level (default 0)
#'
#' @export
runIntCodeComputer <- function(tape,
                               noun = NULL,
                               verb = NULL,
                               iccin = function(){0},
                               iccout = function(x){},
                               done = function(x){},
                               pos = 0,
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

  # If noun and/or verb are provided, set them on the tape
  if(!is.null(noun)) {
    tape <- tapeSet(tape, 1, noun)
  }
  if(!is.null(verb)) {
    tape <- tapeSet(tape, 2, verb)
  }

  # Main driver
  advanceState <- function(state) {
    repeat {
      tape <- state$tape
      pos <- state$pos
      relBase <- state$relBase

      debugFcn(tape, 10)

      debugFcn(sprintf("Head at %.0f", pos), 3)
      debugFcn(sprintf("Relative base at %.0f", relBase), 3)

      instruction <- tapeGet(tape, pos)
      debugFcn(sprintf("Got instruction %.0f", instruction), 4)

      opcode <- instruction %% 100
      debugFcn(sprintf("Executing opCode %.0f", opcode), 1)

      if(opcode == 99) {
        debugFcn("HALT!!!", 1)
        done(state$tape)
        break;
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
        debugFcn(sprintf("Param modes: %s", paste(paramModes, collapse = ",")), 4)


        paramIdx <- ((pos + 1):(pos + nParams))
        debugFcn(paste0("params at: ", paste(paramIdx, collapse = ",")), 4)

        params <- tapeGet(tape, paramIdx)
        debugFcn(paste0("params: ", paste(params, collapse = ",")), 2)

        args <- tapeGet(tape, params, paramModes, relBase)
        debugFcn(paste0("args: ", paste(args, collapse = ",")), 2)

        switch(
          # Oh R, you silly stupid language, you...
          # Cast the opcode as character because otherwise multi-digit codes won't work
          as.character(opcode),

          # Addition
          "1" = {
            to <- params[3] + ifelse(paramModes[3] == 0, 0, relBase)

            debugFcn(sprintf("%.0f + %.0f -> %.0f", args[1], args[2], to), 2)

            state$tape <- tapeSet(
              tape,
              to,
              args[1] + args[2])

            state$pos <- pos + 4
          },

          # Multiplication
          "2" = {
            to <- ifelse(paramModes[3] == 0, params[3], params[3] + relBase)

            debugFcn(sprintf("%.0f * %.0f -> %.0f", args[1], args[2], to), 2)

            state$tape <- tapeSet(
              tape,
              to,
              args[1] * args[2])

            state$pos <- pos + 4
          },

          # Input
          # This is the tricky bit:
          # Since R runs on a single thread, this needs to be done asynchronously
          # Elswise a computer waiting for input would block all others running at the
          # same time (and any code that might provide it with input)
          # Therefore wait for input to be ready by registering the next advanceState
          # as a callback for iccin (see iccInput methods for more info)
          "3" = {
            iccin(function(value) {
              to <- ifelse(paramModes[1] == 0, params[1], params[1] + relBase)

              debugFcn(sprintf("Read value %.0f -> %.0f", value, to), 2)

              state$tape <- tapeSet(
                tape,
                to,
                value)

              state$pos <- pos + 2

              advanceState(state)
            }) # I'm giggling like a stupid person here ^^
            break;
          },

          # Output
          "4" = {
            debugFcn(sprintf("Writing %.0f to iccout", args[1]), 2)
            iccout(args[1])
            state$pos <- pos + 2
          },

          # Jump if true
          "5" = {
            debugFcn(sprintf("Checking whether %.0f != 0", args[1]), 2)
            state$pos <- ifelse(args[1] != 0, args[2], pos + 3)
          },

          # Jump if false
          "6" = {
            debugFcn(sprintf("Checking whether %.0f == 0", args[1]), 2)
            state$pos <- ifelse(args[1] == 0, args[2], pos + 3)
          },

          # Compare less
          "7" = {
            to <- ifelse(paramModes[3] == 0, params[3], params[3] + relBase)

            debugFcn(sprintf("%.0f < %.0f -> %.0f", args[1], args[2], to), 2)

            state$tape <- tapeSet(tape,
                                  to,
                                  ifelse(args[1] < args[2], 1, 0))

            state$pos <- pos + 4
          },

          # Compare equal
          "8" = {
            to <- ifelse(paramModes[3] == 0, params[3], params[3] + relBase)

            debugFcn(sprintf("%.0f == %.0f -> %.0f", args[1], args[2], to), 2)

            state$tape <- tapeSet(tape,
                                  to,
                                  ifelse(args[1] == args[2], 1, 0))

            state$pos <- pos + 4
          },

          # Adjust relative base
          "9" = {
            debugFcn(sprintf("relBase %.0f + %.0f -> %.0f", relBase, args[1], relBase + args[1]), 2)

            state$relBase <- relBase + args[1]

            state$pos <- state$pos + 2
          }
        )
      }
    }
  }

  # Kick things off
  state <- list(
    tape = tape,
    pos = pos,
    relBase = 0
  )

  advanceState(state)

  TRUE
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
  # That's cheating a little but there should not be any negative params with mode == 0
  # Confirmed on day 9
  out <- ifelse(modes == 0,
         tape[abs(parameters) + 1],
         ifelse(modes == 1,
                parameters,
                tape[base + parameters + 1]))

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


# input methods -----------------------------------------------------------

iccFileInput <- function(con) {
  if(!inherits(con, "connection")) {
    stop("Please provide a file connection")
  }

  if(!isOpen(con)) {
    open(con)
  }

  function(cb) {
    cb(readLines(con, 1))
  }
}

iccManualInput <- function() {
  function(cb) {
    done <- FALSE

    while(!done) {
      x <- suppressWarnings(as.numeric(readline("icc> ")))
      done <- !is.na(x) && as.integer(x) == x

      if(!done) {
        message("icc> Nope, please enter an integer...")
      }
    }

    cb(x)
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

  function(cb) {
    if(i > nValues) {
      if(loop) {
        i <- 1
      } else {
        stop(sprintf("Out of values to input!"))
      }
    }

    out <- values[i]
    i <<- i + 1

    cb(out)
  }
}

iccPipe <- function(initialBuffer = NULL, output = NULL) {
  start <- 1
  end <- length(initialBuffer)
  buffer <- initialBuffer

  cbStart <- 1
  cbEnd <- 0
  cbQueue <- list()

  function(x = NULL) {
    if(!is.function(x)) {
      # Being used as output (data in)

      if(!is.null(output)) {
        output(x)
      }

      if(cbStart > cbEnd) {
        # No queue, buffer value for later
        buffer <<- c(buffer, x)
        end <<- end + 1
      } else {
        # Somebody is already waiting
        cbStart <<- cbStart + 1
        cbQueue[[cbStart - 1]](x)
      }
    } else {
      # Being used as input (data out)
      if(end < start) {
        # We have no data ready, register the callback for later use
        cbQueue <<- c(cbQueue, x)
        cbEnd <<- cbEnd + 1
      } else {
        # There is data available, call callback immediately
        out <- buffer[start]
        start <<- start + 1
        x(out)
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


# no callback country -----------------------------------------------------

# I'll be back!
returnyAdvanceState <- function(state) {
  tape <- state$tape
  pos <- state$pos
  relBase <- state$relBase


  instruction <- tapeGet(tape, pos)

  opcode <- instruction %% 100

  if(opcode == 99) {
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

    paramIdx <- ((pos + 1):(pos + nParams))

    params <- tapeGet(tape, paramIdx)

    args <- tapeGet(tape, params, paramModes, relBase)

    switch(
      # Oh R, you silly stupid language, you...
      # Cast the opcode as character because otherwise multi-digit codes won't work
      as.character(opcode),

      # Addition
      "1" = {
        to <- params[3] + ifelse(paramModes[3] == 0, 0, relBase)

        state$tape <- tapeSet(
          tape,
          to,
          args[1] + args[2])

        state$pos <- pos + 4
      },

      # Multiplication
      "2" = {
        to <- ifelse(paramModes[3] == 0, params[3], params[3] + relBase)

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
        state$pos <- ifelse(args[1] != 0, args[2], pos + 3)
      },

      # Jump if false
      "6" = {
        state$pos <- ifelse(args[1] == 0, args[2], pos + 3)
      },

      # Compare less
      "7" = {
        to <- ifelse(paramModes[3] == 0, params[3], params[3] + relBase)

        state$tape <- tapeSet(tape,
                              to,
                              ifelse(args[1] < args[2], 1, 0))

        state$pos <- pos + 4
      },

      # Compare equal
      "8" = {
        to <- ifelse(paramModes[3] == 0, params[3], params[3] + relBase)

        state$tape <- tapeSet(tape,
                              to,
                              ifelse(args[1] == args[2], 1, 0))

        state$pos <- pos + 4
      },

      # Adjust relative base
      "9" = {
        state$relBase <- relBase + args[1]

        state$pos <- state$pos + 2
      }
    )
  }

  state
}
