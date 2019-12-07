day7 <- function(path) {
  tape <- as.numeric(strsplit(readLines(path), ",")[[1]])

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

  doTheThing <- function(perm) {
    pipe <- iccPipe(0)

    for(p in perm) {
      iccin <- iccInput(c(p, pipe()))
      runIntCodeComputer(tape, iccin = iccin, iccout = pipe)
    }

    pipe()
  }

  allOutputs <- apply(perms, 1, doTheThing)

  maxOutput <- which.max(allOutputs)

  message(sprintf("The largest possible output is %d, produced by phase sequence %s...",
                  allOutputs[maxOutput],
                  paste(perms[maxOutput, ], collapse = ", ")))
}
