day24 <- function(path = "inst/input/day24/input.txt") {
  # I feel this grid is going to be expanded for part 2 making the naive approach impossible
  # Then again: Premature optimization
  bugs <- do.call(rbind, lapply(readLines(path), function(x){strsplit(x, "")[[1]]}))

  gridDim <- 5

  countNeighbours <- function(bugs, x, y) {
    n <- 0
    dx <- c(-1, 1, 0, 0)
    dy <- c(0, 0, -1, 1)

    for(i in 1:4) {
      xx <- x + dx[i]
      yy <- y + dy[i]
      if(xx > 0 && xx <= gridDim) {
        if(yy > 0 && yy <= gridDim) {
          if(bugs[yy, xx] == "#") {
            n <- n + 1
            # What am I, a self taught high school kid?
          }
        }
      }
    }
    n
  }

  evolve <- function(bugs) {
    newBugs <- matrix(".", nrow(bugs), ncol(bugs))
    for(x in 1:gridDim) {
      for(y in 1:gridDim) {
        nNeigh <- countNeighbours(bugs, x, y)
        if(bugs[y, x] == "#") {
          if(nNeigh == 1) {
            newBugs[y, x] <- "#"
          }
        } else {
          if(nNeigh > 0 && nNeigh < 3) {
            newBugs[y, x] <- "#"
          }
        }
      }
    }
    newBugs
  }

  prnt <- function(bugs) {
    cat(paste(apply(bugs, 1, paste, collapse = ""), collapse = "\n"))
  }

  getBiodiversity <- function(bugs) {
    sum(2^(which(c(t(bugs)) == "#")-1))
  }

  seen <- c()
  p1Bugs <- bugs

  while(TRUE) {
    p1Bugs <- evolve(p1Bugs)
    bd <- getBiodiversity(p1Bugs)
    cat("\014")
    prnt(p1Bugs)
    Sys.sleep(0.1)
    if(bd %in% seen) {
      message("State repeated, biodiversity: ", bd)
      break;
    }
    seen <- c(seen, bd)
  }

  getNeighbourRec <- function(bugs, x, y, l, dx, dy) {
    above <- as.character(l - 1)
    here <- as.character(l)
    below <- as.character(l + 1)

    if(dx == -1) {
      if(x - 1 == 0) {
        return(bugs[[above]][2, 3])
      } else if(x - 1 == 3 && y == 3) {
        return(bugs[[below]][, gridDim])
      } else {
        return(bugs[[here]][y, x - 1])
      }
    } else if(dx == 1) {
      if(x + 1 == gridDim + 1) {
        return(bugs[[above]][3, 1])
      } else if(x + 1 == 3 && y == 3) {
        return(bugs[[below]][, 1])
      } else {
        return(bugs[[here]][y, x + 1])
      }
    } else if(dy == -1) {
      if(y - 1 == 0) {
        return(bugs[[above]][2, 3])
      } else if(y - 1 == 3 && x == 3) {
        return(bugs[[below]][gridDim, ])
      } else {
        return(bugs[[here]][y - 1, x])
      }
    } else {
      if(y + 1 == gridDim + +1) {
        return(bugs[[above]][4, 3])
      } else if(y + 1 == 3 && x == 3) {
        return(bugs[[below]][1, ])
      } else {
        return(bugs[[here]][y + 1, x])
      }
    }
  }

  countNeighboursRec <- function(bugs, x, y, l) {
    n <- 0
    dx <- c(-1, 1, 0, 0)
    dy <- c(0, 0, -1, 1)

    for(i in 1:4) {
      # Well, that simplified a lot ;P
      n <- n + sum(getNeighbourRec(bugs, x, y, l, dx[i], dy[i]) == "#")
    }
    n
  }

  evolveRec <- function(bugs) {
    levels <- as.numeric(names(bugs))
    levelRange <- range(levels)

    # Expand level list if necessary (makes count neighbours easier)
    if(any(bugs[[as.character(levelRange[1])]] == "#")) {
      bugs[[as.character(levelRange[1] - 1)]] <- matrix(".", gridDim, gridDim)
      levels <- as.numeric(names(bugs))
    }

    if(any(bugs[[as.character(levelRange[2])]] == "#")) {
      bugs[[as.character(levelRange[2] + 1)]] <- matrix(".", gridDim, gridDim)
      levels <- as.numeric(names(bugs))
    }

    newBugs <- list()
    for(l in levels) {
      ll <- as.character(l)
      newLevel <- matrix(".", gridDim, gridDim)
      for(x in 1:gridDim) {
        for(y in 1:gridDim) {
          if(!(x == 3 && y == 3)) {
            nNeigh <- countNeighboursRec(bugs, x, y, l)
            if(bugs[[ll]][y, x] == "#") {
              if(nNeigh == 1) {
                newLevel[y, x] <- "#"
              }
            } else {
              if(nNeigh > 0 && nNeigh < 3) {
                newLevel[y, x] <- "#"
              }
            }
          }
        }
      }

      newBugs[[ll]] <- newLevel
    }
    newBugs
  }

  prntRec <- function(bugs) {
    lvls <- as.numeric(names(bugs))
    for(l in sort(lvls)) {
      if(any(bugs[[as.character(l)]] == "#")) {
        cat(sprintf("Level %d:\n", l))
        prnt(bugs[[as.character(l)]])
        cat("\n\n")
      }
    }
  }

  p2Bugs <- list()
  p2Bugs[["0"]] <- bugs

  cat("-------------------------------------\n")
  cat(sprintf("Iteration %d\n", 0))
  cat("-------------------------------------\n")
  prntRec(p2Bugs)

  if(grepl("example", path)) {
    for(i in 1:10) {
      p2Bugs <- evolveRec(p2Bugs)
      cat("-------------------------------------\n")
      cat(sprintf("Iteration %d\n", i))
      cat("-------------------------------------\n")
      prntRec(p2Bugs)
    }
  }

}
