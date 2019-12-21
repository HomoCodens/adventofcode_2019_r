day21 <- function(path = "inst/input/day21/input.txt") {
  tape <- readICCTape(path)

  springProg <- c(
    "NOT A J",
    "WALK"
  )

  springProg <- c(
    "NOT A J",
    "NOT B T",
    "AND T J",
    "NOT C T",
    "AND T J",
    "AND D J",
    "WALK"
  )

  springProg <- c(
    "NOT A J",
    "NOT B T",
    "OR T J",
    "NOT C T",
    "OR T J",
    "AND D J",
    "WALK"
  )

  iccin <- iccInput(paste0(paste(springProg, collapse = "\n"), "\n"), ASCII = TRUE)

  dmg <- 0
  iccout <- function(x) {
    if(x > 256) {
      dmg <<- dmg + x
    } else {
      cat(rawToChar(as.raw(x)))
    }
  }

  runIntCodeComputer(list(list(
    tape = tape,
    iccin = iccin,
    iccout = iccout
  )))

  runProg <- c(
    "OR E T",
    "OR H T",
    "OR A J",
    "AND B J",
    "AND C J",
    "NOT J J",
    "AND T J",
    "AND D J",
    "RUN"
  )

  iccin <- iccInput(paste0(paste(runProg, collapse = "\n"), "\n"), ASCII = TRUE)

  dmg <- 0

  runIntCodeComputer(list(list(
    tape = tape,
    iccin = iccin,
    iccout = iccout
  )))
}
