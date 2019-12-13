day5 <- function(path, debug = 0) {
  tape <- as.numeric(strsplit(readLines(path), ",")[[1]])

  message("part 1")
  runIntCodeComputer(list(
    list(
      tape = tape,
      iccin = iccInput(1),
      iccout = message)),
      debug = debug)

  message("\npart 2")
  runIntCodeComputer(list(
    list(
      tape = tape,
      iccin = iccInput(5),
      iccout = message)),
    debug  = debug)
}
