day17 <- function(path = "inst/input/day17/input.txt") {

  tape <- readICCTape(path)

  cc <- textConnection("map", open = "w", local = TRUE)

  out <- function(x) {
    cat(rawToChar(as.raw(x)), file = cc)
  }

  runIntCodeComputer(list(
    list(
      tape = tape,
      iccout = out
    )))

  l <- textConnectionValue(cc)
  l <- l[-length(l)]
  close(cc)

  scaffold <- unname(t(sapply(l, function(x) {
    strsplit(x, "")[[1]]
  })))

  nRow <- nrow(scaffold)
  nCol <- ncol(scaffold)
  isScaffold <- scaffold == "#"

  intersections <- (isScaffold[1:(nRow - 2), ] & isScaffold[2:(nRow - 1), ] & isScaffold[3:nRow, ])[, 2:(nCol-1)] &
    (isScaffold[, 1:(nCol - 2)] & isScaffold[, 2:(nCol - 1)] & isScaffold[, 3:nCol])[2:(nRow-1), ]

  intLoc <- which(intersections)
  y <- intLoc %% nrow(intersections)
  x <- intLoc %/% nrow(intersections) + 1

  sum(x*y)

  # Yes, I did it by hand. Marry-Sue me
  # Idea to do it generally:
  #   1) Walk the plank, writing out full program
  #   2) Identify segments meeting criteria
  mainRoutine <- c("A,A,B,B,C,B,C,B,C,A")
  subA <- c("L,10,L,10,R,6")
  subB <- c("R,12,L,12,L,12")
  subC <- c("L,6,L,10,R,12,R,12")

  prog2ASCII <- function(x) {
    as.numeric(sapply(strsplit(paste0(x, "\n"), "")[[1]], charToRaw))
  }

  input <- iccInput(c(
    prog2ASCII(mainRoutine),
    prog2ASCII(subA),
    prog2ASCII(subB),
    prog2ASCII(subC),
    prog2ASCII("n")
  ))

  output <- function(x) {
    if(x < 1000) {
      cat(rawToChar(as.raw(x)))
    } else {
      message(sprintf("I think the answer is %.0f...", x))
    }
  }

  tape2 <- tapeSet(tape, 0, 2)
  runIntCodeComputer(list(
    list(
      tape = tape2,
      iccin = input,
      iccout = output
    )
  ))
}

