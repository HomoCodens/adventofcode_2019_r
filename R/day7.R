day7 <- function(path = "inst/input/day7/input.txt") {
  tape <- readICCTape(path)

  getPerms <- function(v) {
    if(length(v) == 1) {
      v
    } else {
      out <- NULL
      for(i in seq_along(v)) {
        out <- rbind(out, cbind(v[i], getPerms(v[-i])))
      }
      out
    }
  }

  perms <- getPerms(0:4)

  doTheThing <- function(perm, permDone) {

    latestOut <- 0
    pipeEA <- iccPipe(c(perm[1], 0), output = function(x){ latestOut <<- x })
    pipeAB <- iccPipe(perm[2])
    pipeBC <- iccPipe(perm[3])
    pipeCD <- iccPipe(perm[4])
    pipeDE <- iccPipe(perm[5])

    runIntCodeComputer(list(
      list(
        tape = tape,
        iccin = pipeEA,
        iccout = pipeAB
      ),
      list(
        tape = tape,
        iccin = pipeAB,
        iccout = pipeBC
      ),
      list(
        tape = tape,
        iccin = pipeBC,
        iccout = pipeCD
      ),
      list(
        tape = tape,
        iccin = pipeCD,
        iccout = pipeDE
      ),
      list(
        tape = tape,
        iccin = pipeDE,
        iccout = pipeEA,
        done = function(tape) {
          permDone(latestOut)
        }
      )
    ))
  }

  acc <- iccOutputAccumulator()

  for(i in seq(nrow(perms))) {
    doTheThing(perms[i, ], function(x) {
      acc$acc(x)
      if(i == nrow(perms)) {
        message(max(acc$values()))
      }
    })
  }

  perms2 <- getPerms(5:9)

  acc <- iccOutputAccumulator()

  for(i in seq(nrow(perms2))) {
    doTheThing(perms2[i, ], function(x) {
      acc$acc(x)
      if(i == nrow(perms2)) {
        message(max(acc$values()))
      }
    })
  }
}
