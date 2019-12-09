day9 <- function(path) {
  tape <- as.numeric(strsplit(readLines(path), ",")[[1]])

  in1 <- iccInput(1, TRUE)
  out1 <- message

  runIntCodeComputer(tape, iccin = in1, iccout = out1)

  in2 <- iccInput(2)
  runIntCodeComputer(tape, iccin = in2, iccout = out1)
}
