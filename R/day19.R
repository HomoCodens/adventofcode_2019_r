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

  # For different inputs this would have to be done different (l2r instead of t2b,
  # maybe even filling if initial scan hits corner)
  bottom <- nrow(map)
  bottomIsAffected <- which(map[bottom, ] == "#")
  leftEdge <- min(bottomIsAffected)
  rightEdge <- max(bottomIsAffected)

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
  #for(i in seq(10)) {
    if(rightEdge == ncol(map)) {
      map <- growMap(map, 0)
    }

    if(bottom == nrow(map)) {
      map <- growMap(map, 1)
    }

    # Step 1 further
    bottom <- bottom + 1
    message("Now at ", bottom)

    # Scan for beginning of new left edge
    repeat {
      isAffected <- FALSE
      input <- iccInput(c(leftEdge - 1, bottom - 1))
      output <- function(x) {
        isAffected <<- x == 1
      }

      runIntCodeComputer(list(list(
        tape = tape,
        iccin = input,
        iccout = output
      )))

      if(isAffected) {
        break;
      } else {
        leftEdge <- leftEdge + 1
      }
    }

    # Scan for new right edge
    repeat {
      isAffected <- FALSE
      input <- iccInput(c(rightEdge, bottom - 1))
      output <- function(x) {
        isAffected <<- x == 1
      }

      runIntCodeComputer(list(list(
        tape = tape,
        iccin = input,
        iccout = output
      )))

      if(isAffected) {
        rightEdge <- rightEdge + 1
      } else {
        break;
      }
    }

    map[bottom, leftEdge:rightEdge] <- "#"

    # Check for whether condition met
    if(nrow(map) >= sizeNeedsFitting + bottom && ncol(map) >= sizeNeedsFitting + leftEdge) {
      if(all(map[(bottom-sizeNeedsFitting + 1):bottom, leftEdge:(leftEdge + sizeNeedsFitting - 1)] == "#")) {
        done <- TRUE
        # map[(bottom-sizeNeedsFitting + 1):bottom, leftEdge:(leftEdge + sizeNeedsFitting - 1)] <- "O"
        map[(bottom-sizeNeedsFitting + 1), leftEdge] <- "O"
      }
    }
  }
  # cat("\014")
  # cat(paste(apply(map, 1, paste, collapse = ""), collapse = "\n"))

  trueX <- leftEdge - 1
  trueY <- bottom - sizeNeedsFitting
  message(trueX*10000 + trueY)
}
