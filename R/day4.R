day4 <- function(path) {
  range <- as.numeric(strsplit(readLines(path), "-")[[1]])

  isPasswordOK <- function(p) {
    parts <- (p %% 10^(6:1)) %/% 10^(5:0)

    p >= 100000 &&
      p <= 999999 &&
      all(diff(parts) >= 0) &&
      max(table(parts) > 1)
  }

  nPasswords <- sum(sapply(seq(range[1], range[2]), isPasswordOK))
  message(sprintf("There are %d valid passwords in %d-%d", nPasswords, range[1], range[2]))
}
