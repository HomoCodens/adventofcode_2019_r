day22 <- function(path = "inst/input/day22/input.txt") {
  l <- readLines(path)

  parseLine <- function(line) {
    if(grepl("^deal into", line)) {
      list(
        op = "rev"
      )
    } else if(grepl("^cut", line)) {
      list(
        op = "cut",
        n = as.numeric(gsub("cut ", "", line)))
    } else if(grepl("^deal with", line)) {
      list(
        op = "deal",
        n = as.numeric(gsub("deal with increment ", "", line)))
    }
  }

  shuffles <- lapply(l, parseLine)

  shuffleAway <- function(shuffles, nCards, cardToTrack) {
    pos <- cardToTrack
    for(s in shuffles) {
      if(s$op == "rev") {
        pos <- nCards - pos - 1
      } else if(s$op == "cut") {
        pos <- (nCards - s$n + pos) %% nCards
      } else {
        pos <- (pos*s$n) %% nCards  # Note to self: %% > *
      }
    }
    pos
  }

  order2deck <- function(order) {
    sapply(do.call(seq, as.list(range(order))), function(x) {
      which(order == x)
    }) - 1
  }

  if(grepl("example", path)) {
    return(order2deck(shuffleAway(shuffles, 10, 0:9)))
  }

  message("Part 1: ", shuffleAway(shuffles, 10007, 2019))

  # nSuperDeck <- 119315717514047
  # nShuffles <- 101741582076661
  # i <- 1
  # pos <- 2020
  # while(i <= nShuffles) {
  #   if(i %% 10000 == 0) {
  #     message(i)
  #   }
  #
  #   pos <- shuffleAway(rev(shuffles), nSuperDeck, pos)
  #
  #   if(pos == 2020) {
  #     message("Hey, we cycled after ", i, " shuffles. Go figure!")
  #   }
  #   #message(i, " - ", pos)
  #   i <- i + 1
  # }

  # Hey, it's about learning, right?
  # https://www.reddit.com/r/adventofcode/comments/ee0rqi/2019_day_22_solutions/fbnkaju/
  # Not so much a coding challenge thog

  getParams <- function(shuffles, mod, offset = 0, increment = 1) {
    for(s in shuffles) {
      if(s$op == "rev") {
        increment <- -increment
        offset <- offset + increment
      } else if(s$op == "cut") {
        offset <- offset + increment*s$n
      } else {
        increment <- increment*gmp::powm(s$n, mod - 2, mod)
      }
    }

    list(
      increment = gmp::mod.bigz(increment, mod),
      offset = gmp::mod.bigz(offset, mod)
    )
  }

  nSuperDeck <- 119315717514047
  nShuffles <- 101741582076661

  # Get offset_diff and increment_mul for 1 pass
  o1 <- getParams(shuffles, nSuperDeck)

  increment <- gmp::powm(o1$increment, nShuffles, nSuperDeck)
  offset <- gmp::mod.bigz(o1$offset * (1 - gmp::powm(o1$increment, nShuffles, nSuperDeck)) *
    gmp::powm(1 - o1$increment, nSuperDeck - 2, nSuperDeck), nSuperDeck)

  gmp::mod.bigz(offset + 2020*increment, nSuperDeck)
}
