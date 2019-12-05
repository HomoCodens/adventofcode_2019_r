day5 <- function(path) {
  tape <- as.numeric(strsplit(readLines(path), ",")[[1]])

  iccin <- function() {
    1
  }

  iccout <- function(x) {
    message(x)
  }

  runIntCodeComputer(tape, iccin = iccin, iccout = iccout)
}
