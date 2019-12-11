stackXplosion <- function(maxDepth) {
  # Recurse the sh*t out of the R stack
  boomBot <- function(d) {
    message("Now at depth ", d, "...")
    message("In frame ", sys.nframe(), "...")
    if(d == maxDepth) {
      message("Ok I give up...")
    } else {
      message("That Inception joke")
      boomBot(d + 1)
    }
  }

  boomBot(0)
}


stackBNice <- function(maxDepth) {
  boomBot <- function(d) {
    message("Now at depth ", d, "...")
    message("In frame ", sys.nframe(), "...")
    if(d == maxDepth) {
      message("Ok I give up...")
    } else {
      message("That Inception joke")
      do.call(boomBot, list(d + 1), envir = sys.frame(sys.parent()))
    }
  }

  boomBot(0)
}
