day16 <- function(path = "inst/input/day16/input.txt") {
  input <- as.numeric(strsplit(readLines(path), "")[[1]])

  buildFilters <- function(n, filter = c(0, 1, 0, -1)) {
    t(sapply(seq(n), function(i) {
      rep_len(rep(filter, each = i), n + 1)[-1]
    }))
  }

  fold <- function(input, filters = buildFilters(length(input)), print = FALSE) {
    n <- length(input)
    output <- numeric(n)

    for(i in seq(n)) {
      f <- filters[i, ]
      output[i] <- abs(sum(input * f)) %% 10
      if(print) {
        message(paste(sprintf("%d*%d", f, input), collapse = " + "), " => ", output[i])
      }
    }

    output
  }

  state <- input
  fs <- buildFilters(length(state))
  for(i in 1:100) {
    state <- fold(state, fs)
  }

  message(state[1:8])

  offset <- as.numeric(paste(input[1:7], collapse = ""))

  state <- rep(input, 10000)
  state <- state[(offset+1):length(state)] # Well this was stupid...
  n <- length(state)
  for(i in 1:100) {
    state <- rev(cumsum(rev(state))) %% 10
  }

  message(state[1:8])
}
