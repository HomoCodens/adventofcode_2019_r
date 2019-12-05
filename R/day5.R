day5 <- function(path, debug = 0) {
  tape <- as.numeric(strsplit(readLines(path), ",")[[1]])

  iccin <- function() {
    1
  }

  iccout <- function(x) {
    message(x)
  }

  message("part 1")
  runIntCodeComputer(tape, iccin = iccin, iccout = iccout, debug = debug)

  message("\npart 2")
  runIntCodeComputer(tape, iccin = function(){5}, iccout = iccout, debug  = debug)
}
