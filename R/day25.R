day25 <- function(path = "inst/input/day25/input.txt") {
  tape <- readICCTape(path)

  input <- iccManualInput(ASCII = TRUE)
  output <-iccConsoleOutput()

  textAdventureBruteForcer <- function() {
    wayToCP <- c(
      "east",
      "take antenna",
      "west",
      "north",
      "take weather machine",
      "north",
      "take klein bottle",
      "east",
      "take spool of cat6",
      "east",
      "north",
      "west",
      "north",
      "take cake",
      "south",
      "east",
      "east",
      "south",
      "take shell",
      "north",
      "west",
      "south",
      "south",
      "take mug",
      "north",
      "west",
      "south",
      "south",
      "drop antenna",
      "drop weather machine",
      "drop klein bottle",
      "drop spool of cat6",
      "drop cake",
      "drop shell",
      "drop mug"
      # We are now at the checkpoint, carrying all items carriable
    )

    commands2buffer <- function(x) {
      as.numeric(
        sapply(strsplit(paste(c(x, ""), collapse = "\n"), "")[[1]],
               charToRaw))
    }

    bufferIn <- commands2buffer(wayToCP)

    bufferOut <- numeric(0)

    items <- rep(FALSE, 7)
    names(items) <- c(
      "antenna",
      "weather machine",
      "klein bottle",
      "spool of cat6",
      "cake",
      "shell",
      "mug"
    )

    atCP <- FALSE
    combination <- 0
    done <- FALSE
    fiddlingInventory <- FALSE

    function(vin = NULL) {
      if(is.null(vin)) {
        # Being used as input function
        out <- bufferIn[1]
        bufferIn <<- bufferIn[-1]
        if(length(bufferIn) == 0) {
          if(!atCP) {
            # We are now at the checkpoint with all items on the floor
            atCP <<- TRUE
            message("We haz arrifed")
          }

          if(fiddlingInventory) {
            fiddlingInventory <<- FALSE
          }
        }
        out
      } else {
        bufferOut <<- c(bufferOut, vin)
        if(vin == 10) {
          # Newline, do something with the line received
          line <- rawToChar(as.raw(bufferOut))
          cat(line) # check for contents of line
          bufferOut <<- c()

          if(atCP) {
            if(grepl("^Command", line) && !fiddlingInventory) {

              cmd <- c()

              newItems <- as.numeric(rawToBits(as.raw(combination)))[-8] > 0
              names(newItems) <- names(items)

              itemsToDrop <- items & !newItems
              itemsToPickUp <- newItems & !items

              # Pick up items
              cmd <- c(cmd, sprintf("take %s", names(items)[itemsToPickUp]))

              # Drop items
              cmd <- c(cmd, sprintf("drop %s", names(items)[itemsToDrop]))

              # Try going to the checkpoint
              cmd <- c(cmd, "east")

              bufferIn <<- commands2buffer(cmd)
              items <<- newItems

              fiddlingInventory <<- TRUE

              message(sprintf("Trying combo %d: %s...", combination, paste(names(items)[items], collapse = "-")))
              combination <<- combination + 1

            } else if(grepl("main airlock", line)) {
              stop("My work here is done.")
            }
          }
        }
      }
    }
  }

  hax <- textAdventureBruteForcer()

  runIntCodeComputer(list(list(
    tape = tape,
    iccin = hax,
    iccout = hax
  )))
}
