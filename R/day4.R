day4 <- function(path = "inst/input/day4/input.txt") {
  range <- as.numeric(strsplit(readLines(path), "-")[[1]])

  isPasswordOK <- function(p, part = 1) {
    parts <- (p %% 10^(6:1)) %/% 10^(5:0)

    p >= 100000 &&
      p <= 999999 &&
      all(diff(parts) >= 0) &&
      # Actually, this should not work (max(table(1xxxx1)) == 2 o.o)
      max(table(parts) > 1) &&
      (part == 1 ||
         any(table(parts) == 2))
  }

  nPasswords <- sum(sapply(seq(range[1], range[2]), isPasswordOK, part = 1))
  message(sprintf("There are %d valid passwords in %d-%d", nPasswords, range[1], range[2]))

  nPasswords2 <- sum(sapply(seq(range[1], range[2]), isPasswordOK, part = 2))
  message(sprintf("There are %d valid passwords meeting the new criteria in %d-%d", nPasswords2, range[1], range[2]))
}

day4v2 <- function(path) {
  range <- as.numeric(strsplit(readLines(path), "-")[[1]])

  runDay4 <- function(lowerBound,
                      upperBound,
                      digits = lowerBound,
                      pos = 1,
                      part = 1,
                      nDigits = length(digits),
                      debug = FALSE) {
    # Skip ahead to the first possible candidate
    if(pos == 1) {
      maxD = which(digits == max(digits))[1]
      digits[maxD:nDigits] <- digits[maxD]
    }

    # if(debug) {
    #   message(sprintf("At level %d...", pos))
    #   message(sprintf("digits: %s", paste(digits, collapse = "")))
    # }

    if(pos <= nDigits) {
      start <- ifelse(all(digits[1:pos] == lowerBound[1:pos]), lowerBound[pos], digits[pos - 1])
      nValid <- 0
      done <- FALSE
      for(d in start:9) {
        digits[pos] <- d
        out <- runDay4(lowerBound, upperBound, digits, pos + 1, part, nDigits, debug)
        if(debug) {
          if(out$done) {
            message("Got done signal from below...")
            message(sprintf("Do %sconsider myself to be done", ifelse(all(digits[1:pos] >= upperBound[1:pos]), "", "not ")))
          }
        }
        nValid <- nValid + out$nValid
        done <- out$done || all(digits[1:pos] >= upperBound[1:pos])
        if(done) {
          break;
        }
      }

      list(
        nValid = nValid,
        done = done
      )
    } else {

      isDone <- all(digits >= upperBound)
      isValid <- ((any(diff(digits) == 0)) &&
        (part == 1 || {
          # How most un-R-like...
          currentRun <- 1
          runLengths <- c()
          for(i in seq(5)) {
            if(digits[i] == digits[i+1]) {
              currentRun <- currentRun + 1
            } else {
              runLengths <- c(runLengths, currentRun)
              currentRun <- 1
            }
          }
          runLengths <- c(runLengths, currentRun)
          any(runLengths == 2)
        })) && (!isDone || all(digits == upperBound))

      if(debug) {
        message(sprintf("Checking validity of %s...", paste(digits, collapse = "")))
        message(sprintf("does %slook valid...", ifelse(isValid, "", "not ")))
      }

      list(
        nValid = ifelse(isValid, 1, 0),
        done = isDone
      )
    }
  }

  lowerBound <- (range[1] %% 10^(6:1)) %/% 10^(5:0)
  upperBound <- (range[2] %% 10^(6:1)) %/% 10^(5:0)
  out1 <- runDay4(lowerBound, upperBound)

  nPasswords <- out1$nValid
  message(sprintf("There are %d valid passwords in %d-%d", nPasswords, range[1], range[2]))

  out2 <- runDay4(lowerBound, upperBound, part = 2)
  nPasswords2 <- out2$nValid
  message(sprintf("There are %d valid passwords meeting the new criteria in %d-%d", nPasswords2, range[1], range[2]))
}
