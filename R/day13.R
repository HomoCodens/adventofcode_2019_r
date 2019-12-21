day13 <- function(path = "inst/input/day13/input.txt") {
  tape <- readICCTape(path)

  # 0 is an empty tile. No game object appears in this tile.
  # 1 is a wall tile. Walls are indestructible barriers.
  # 2 is a block tile. Blocks can be broken by the ball.
  # 3 is a horizontal paddle tile. The paddle is indestructible.
  # 4 is a ball tile. The ball moves diagonally and bounces off objects.
  sprites <- c(
    " ",
    "X",
    "#",
    "-",
    "*"
  )

  pixels <- matrix(sprites[1])
  rendererState <- 0 # 0 - awaiting x, 1 - awaiting y, 2 - awaiting type

  xCoord <- 0
  yCoord <- 0

  score <- 0

  # Tracking stuff for the bot
  yPaddle <- NA
  xPaddle <- NA
  lastXBall <- NA
  lastYBall <- NA
  dxBall <- NA
  dYBall <- NA
  dxPaddle <- NA

  # Just for readability
  PADDLE <- 3
  BALL <- 4


  renderer <- function(vin) {
    if(rendererState == 0) {
      x <- vin + 1 # 1 based indexing
      if(ncol(pixels) < x) {
        pixels <<- cbind(pixels, matrix(sprites[1], nrow(pixels), x - ncol(pixels)))
      }

      xCoord <<- x
      rendererState <<- 1
    } else if(rendererState == 1) {
      y <- vin + 1
      if(nrow(pixels) < y) {
        pixels <<- rbind(pixels, matrix(sprites[1], y - nrow(pixels), ncol(pixels)))
      }

      yCoord <<- y
      rendererState <<- 2
    } else {

      if(vin == PADDLE) {
        xPaddle <<- xCoord
        yPaddle <<- yCoord
      }

      if(vin == BALL) {
        dxBall <<- xCoord - lastXBall
        dyBall <<- yCoord - lastYBall
        lastXBall <<- xCoord
        lastYBall <<- yCoord
      }

      if(xCoord == 0 && yCoord == 1) {
        score <<- vin
      } else {
        spriteId <- vin + 1
        pixels[yCoord, xCoord] <<- sprites[spriteId]
      }

      #if(xCoord == ncol(pixels) && yCoord == ncol(pixels)) {
        cat("\014")
        cat(paste(apply(pixels, 1, paste, collapse = ""), collapse = "\n"))
        cat("\n\n")
        cat(sprintf("Player score: %d", score))
        cat("\n\n")
      #}

      rendererState <<- 0
    }
  }

  invisible(runIntCodeComputer(list(
    list(
      tape = tape,
      iccout = renderer,
      done = function(tape) {
        nBlocks <- sum(sum(pixels == sprites[3]))
        message(sprintf("There are %d blocks left...", nBlocks))
      }
    ))))

  tape <- tapeSet(tape, 0, 2)

  iccCabinetBot <- function() {
    # # We don't know anything yet
    # if(is.na(xPaddle) || is.na(lastXBall)) {
    #   return(0)
    # }

    out <- sign(lastXBall - xPaddle)

    return(out)

    # Premature optimization
    # if(dyBall < 0) {
    #   # Ball is moving up, just try to match its position
    #   return(sign(xPaddle - lastXBall))
    # } else if(dyBall > 0) {
    #   # Ball is moving down, solve for its impact position (this ignores walls tho)
    #
    # }
    #
    # return(0)
  }

  invisible(runIntCodeComputer(list(
    list(
      tape = tape,
      iccout = renderer,
      iccin = iccCabinetBot,
      done = function(tape) {
        message("BOOM!")
        message("Final score: ", score)
      }
    ))))
}
