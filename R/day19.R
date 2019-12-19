day19 <- function(path = "inst/input/day19/input.txt") {
  tape <- readICCTape(path)

  map <- matrix(" ", 50, 50)
  state <- "x"
  x <- rep(0:49, each = 50)
  y <- rep(0:49, 50)

  for(i in seq(50*50)) {
    input <- iccInput(c(x[i], y[i]))
    output <- function(x) {
      map[i] <<- ifelse(x == 1, "#", ".")
    }

    runIntCodeComputer(list(list(
      tape = tape,
      iccin = input,
      iccout = output
    )))

    cat("\014")
    cat(paste(apply(map, 1, paste, collapse = ""), collapse = "\n"))
  }

  message(sum(sum(map == "#")))

  # Reuse the initial scan
  map <- mm
  edge <- data.table()

  if(any(map[nrow(map), ] == "#")) {
    edge <- rbind(edge, data.table(
      x = which(map[nrow(map), ] == "#"),
      y = nrow(map)
    ))
  }

  if(any(map[, ncol(map)] == "#")) {
    edge <- rbind(edge, data.table(
      x = ncol(map),
      y = which(map[, ncol(map)] == "#")
    ))
  }
  edge <- unique(edge)
  setorder(edge, y, x)
  # Technically edge should be 1 step further ahead but doesn't hurt doing them again :man-shrugging:

  growMap <- function(map, dir) {
    message("Growing map ", ifelse(dir == 1, "down", "sideways"))
    if(dir == 1) {
      rbind(map, matrix(".", nrow(map), ncol(map)))
    } else {
      cbind(map, matrix(".", nrow(map), ncol(map)))
    }
  }

  sizeNeedsFitting <- 100
  done <- FALSE
  while(!done) {
  #for(i in seq(1000)) {
    node <- edge[1]

    message("Checking ", node$x, "-", node$y)

    edge <- edge[-1]
    if(node$x == ncol(map)) {
      map <- growMap(map, 0)
    }

    if(node$y == nrow(map)) {
      map <- growMap(map, 1)
    }

    # Anything diagonally below a cell that is not on the right edge MUST be affeteded
    if(map[node$y, node$x + 1] != ".") {
      map[node$y + 1, node$x + 1] <- "#"
    }

    input <- iccInput(c(node$x - 1, node$y - 1))
    isAffected <- FALSE
    output <- function(x) {
      isAffected <<- x == 1
    }

    runIntCodeComputer(list(list(
      tape = tape,
      iccin = input,
      iccout = output
    )))


    message("Is ", ifelse(isAffected, "", "not "), "affected")

    if(isAffected) {
      map[node$y, node$x] <- "#"
      newCandidates <- data.table(
        x = c(node$x, node$x + 1),
        y = c(node$y + 1, node$y)
      )

      # Baaaaad! Slooooow!!! Lazy...
      edge <- unique(rbind(edge, newCandidates))
      setorder(edge, y, x)
    }

    # Check for whether condition met
    if(all(nrow(map) >= sizeNeedsFitting + node$y, ncol(map) >= sizeNeedsFitting + node$x)) {
      if(all(map[(node$y - sizeNeedsFitting):node$y, (node$x - sizeNeedsFitting):node$x] == "#")) {
        done <- TRUE
        map[(node$y - sizeNeedsFitting):node$y, (node$x - sizeNeedsFitting):node$x] <- "O"
      }
    }
  }
  cat("\014")
  cat(paste(apply(map, 1, paste, collapse = ""), collapse = "\n"))
}
