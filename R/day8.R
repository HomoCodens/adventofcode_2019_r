day8 <- function(path) {
  digits <- as.numeric(strsplit(readLines(path), "")[[1]])

  w <- 25
  h <- 6
  d <- length(digits)/(w*h)

  # Now if this isn't what R was made for...
  # Carefuls: The layers are transposed (cause dim fills by col)
  layers <- digits
  dim(layers) <- c(h, w, d)

  nZeros <- apply(layers, 3, function(x){sum(sum(x == 0))})
  maxZeros <- which.min(nZeros)

  checkLayer <- layers[, , maxZeros]
  solution1 <- sum(sum(checkLayer == 1)) * sum(sum(checkLayer == 2)) # sum sum sum

  message(solution1)
}
