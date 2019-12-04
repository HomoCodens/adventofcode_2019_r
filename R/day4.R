day4 <- function(path) {
  range <- as.numeric(strsplit(readLines(path), "-")[[1]])

  isPasswordOK <- function(p, part = 1) {
    parts <- (p %% 10^(6:1)) %/% 10^(5:0)

    p >= 100000 &&
      p <= 999999 &&
      all(diff(parts) >= 0) &&
      max(table(parts) > 1) &&
      (part == 1 ||
         any(table(parts) == 2))
  }

  nPasswords <- sum(sapply(seq(range[1], range[2]), isPasswordOK, part = 1))
  message(sprintf("There are %d valid passwords in %d-%d", nPasswords, range[1], range[2]))

  nPasswords2 <- sum(sapply(seq(range[1], range[2]), isPasswordOK, part = 2))
  message(sprintf("There are %d valid passwords meeting the new criteria in %d-%d", nPasswords2, range[1], range[2]))
}
