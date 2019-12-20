day20 <- function(path = "inst/input/day20/input.txt") {
  map <- t(unname(sapply(readLines(path), function(x)strsplit(x, "")[[1]])))

  # Look ma, I lernd a trick!
  # ...aaand it got stupid because %in% turns matrices into vectors...
  chars <- which(map != "." & map != " " & map != "#", arr.ind = TRUE)

  # Mark portals on the map proper
  for(i in seq(nrow(chars))) {
    y <- chars[i, 1]
    x <- chars[i, 2]

    if({xx <- map[y - 1, x]; length(xx) > 0 && xx == "."}) {
      map[y-1, x] <- paste0(map[y, x], map[y+1, x])
    } else if(y + 1 < nrow(map) && {xx <- map[y + 1, x]; xx == "."}) {
      map[y+1, x] <- paste0(map[y - 1, x], map[y, x])
    } else if({xx <- map[y, x - 1]; length(xx) > 0 && xx == "."}) {
      map[y, x - 1] <- paste0(map[y, x], map[y, x + 1])
    } else if(x + 1 < ncol(map) && {xx <- map[y, x + 1]; xx == "."}) {
      map[y, x + 1] <- paste0(map[y, x - 1], map[y, x])
    }
  }

  # Remove portal labels/border
  map <- map[3:(nrow(map)-2), 3:(ncol(map) - 2)]
  # Fill donut hole
  map[map %in% c(LETTERS, " ")] <- "#"


  getNeighbours <- function(map, node, recursive = FALSE) {
    y <- node$row
    x <- node$col
    l <- node$level

    neighbours <- rstack()

    if({xx <- map[y - 1, x]; length(xx) > 0 && xx != "#"}) {
      neighbours <- insert_top(neighbours, data.frame(row = y - 1, col = x, level = l))
    }
    if(y + 1 <= nrow(map) && {xx <- map[y + 1, x]; xx != "#"}) {
      neighbours <- insert_top(neighbours, data.frame(row = y + 1, col = x, level = l))
    }
    if({xx <- map[y, x - 1]; length(xx) > 0 && xx != "#"}) {
      neighbours <- insert_top(neighbours, data.frame(row = y, col = x - 1, level = l))
    }
    if(x + 1 <= ncol(map) && {xx <- map[y, x + 1]; xx != "#"}) {
      neighbours <- insert_top(neighbours, data.frame(row = y, col = x + 1, level = l))
    }

    # If on a portal
    if(map[y, x] != "." && map[y, x] != "AA" && map[y, x] != "ZZ") {
      #browser()

      #message("Zooming through ", map[y, x])
      portalLocations <- which(map == map[y, x], arr.ind = TRUE)
      otherEnd <- data.frame(as.list(
        portalLocations[!apply(portalLocations, 1, function(x)x["row"] == node$row && x["col"] == node$col), ]))

      portalPermitted <- FALSE
      otherEnd$level <- node$level
      if(recursive) {
        if(otherEnd$row == 1 || otherEnd$row == nrow(map) ||
           otherEnd$col == 1 || otherEnd$col == ncol(map)) {
          # Outer portal, move up
          otherEnd$level <- node$level + 1
          portalPermitted <- TRUE
        } else if(l > 0 && otherEnd$row > 1 && otherEnd$col > 1) {
          otherEnd$level <- node$level - 1
          portalPermitted <- TRUE
        }
      }
      if(portalPermitted || !recursive) {
        neighbours <- insert_top(neighbours, otherEnd)
      }
    }

    as.data.frame(neighbours)
  }

  getNodeId <- function(node, suffix) {
    paste(c(node, suffix), collapse = "-")
  }

  doTheThing <- function(map, recursive = FALSE) {
    if(recursive && grepl(path, "example2")) {
      stop("Nope, that won't work!")
    }

    start <- data.frame(which(map == "AA", arr.ind = TRUE))
    start$level <- 0
    end <- data.frame(which(map == "ZZ", arr.ind = TRUE))
    end$level <- 0
    dists <- list()
    dists[[getNodeId(start, "AA")]] <- 0

    steps <- rstack()

    edge <- rdeque()
    edge <- insert_back(edge, start)
    done <- list()
    done[[getNodeId(start, "AA")]] <- TRUE
    while(!all(peek_front(edge) == end)) {
      node <- peek_front(edge)

      if(map[node$row, node$col] != ".") {
        message("Pulsing... at node ", map[node$row, node$col])
        print(node)
      }


      edge <- without_front(edge)
      neighbours <- getNeighbours(map, node, recursive)
      nodeId <- getNodeId(node, map[node$row, node$col])


      for(i in seq(nrow(neighbours))) {
        id <- getNodeId(neighbours[i, ], map[neighbours[i, "row"], neighbours[i, "col"]])
        steps <- insert_top(steps, data.frame(from = nodeId, to = id, stringsAsFactors = FALSE))
        if(!is.null(done[[id]])) {
          next;
        }

        done[[id]] <- TRUE
        dists[[id]] <- dists[[nodeId]] + 1
        edge <- insert_back(edge, neighbours[i, ])
      }
    }

    at <- getNodeId(end, "ZZ")
    pathTaken <- rstack()
    while(at != getNodeId(start, "AA")) {
      pathTaken <- insert_top(pathTaken, at)

      nextStep <- peek_top(steps)
      while(nextStep$to != at) {
        steps <- without_top(steps)
        nextStep <- peek_top(steps)
      }
      at <- nextStep$from
    }


    message(dists[[getNodeId(end, "ZZ")]])
    pathTaken
  }

  #doTheThing(map, FALSE)

  aa <- doTheThing(map, TRUE)
}
