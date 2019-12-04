#' @import data.table
day3 <- function(path) {
  wires <- readLines(path)

  parseWire <- function(wire, id) {
    segments <- strsplit(wire, ",")[[1]]

    parseSegment <- function(segment, x, y) {
      dir <- substr(segment, 1, 1)
      dist <- as.numeric(substr(segment, 2, nchar(segment)))
      switch(dir,
             "U" = {
               xout <- x
               yout <- y + dist
               list(
                 coords = data.frame(
                   x = x,
                   y = (y + 1):yout
                 ),
                 x = xout,
                 y = yout
               )
             },
             "D" = {
               xout <- x
               yout <- y - dist
               list(
                 coords = data.frame(
                   x = x,
                   y = rev(yout:(y - 1))
                 ),
                 x = xout,
                 y = yout
               )
             },
             "R" = {
               xout <- x + dist
               yout <- y
               list(
                 coords = data.frame(
                   x = (x + 1):xout,
                   y = yout
                 ),
                 x = xout,
                 y = yout
               )
             },
             "L" = {
               xout <- x - dist
               yout <- y
               list(
                 coords = data.frame(
                   x = rev(xout:(x - 1)),
                   y = yout
                 ),
                 x = xout,
                 y = yout
               )
             }
      )
    }

    coords <- list()
    x <- 0
    y <- 0
    for(i in seq_along(segments)) {
      segment <- parseSegment(segments[i], x, y)
      x <- segment$x
      y <- segment$y
      coords[[i]] <- segment$coords
    }

    coords[[length(coords) + 1]] <- data.frame(
      x = 0,
      y = 0
    )

    rbindlist(coords)[, id := id][, timeToReach := c(1:(.N-1), 0)]
  }

  wire_coords <- data.table()
  for(i in seq_along(wires)) {
    wire_coords <- rbind(wire_coords, parseWire(wires[i], i))
  }

  crossings <- wire_coords[duplicated(wire_coords, by = c("x", "y")) & !duplicated(wire_coords, by = c("x", "y", "id")), .(x, y)]
  crossings <- merge(crossings, wire_coords, by = c("x", "y"))
  crossings <- crossings[, manhattanDist := abs(x) + abs(y)][manhattanDist > 0]
  solution1 <- crossings[, min(manhattanDist)]
  message(sprintf("The crossing closest to the origin is %d away", solution1))

  solution2 <- crossings[, .(timeToReachBoth = sum(timeToReach)), by = c("x", "y")][, min(timeToReachBoth)]
  message(sprintf("The timing-optimal crossing is %d", solution2))
}
