day9 <- function(path = "inst/input/day9/input.txt") {
  tape <- readICCTape(path)

  in1 <- iccInput(1)
  out1 <- message

  invisible(runIntCodeComputer(list(
    list(
      tape = tape,
      iccin = in1,
      iccout = out1))))

  in2 <- iccInput(2)
  invisible(runIntCodeComputer(list(
    list(
      tape = tape,
      iccin = in2,
      iccout = out1))))
}
