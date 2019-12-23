day23 <- function(path = "inst/input/day23/input.txt") {
  tape <- readICCTape(path)

  net <- intErnet(log = TRUE)

  computers <- lapply(0:49, function(ip) {
    connection <- net()
    list(
      tape = tape,
      iccin = connection,
      iccout = connection
    )
  })

  runIntCodeComputer(computers)
}
