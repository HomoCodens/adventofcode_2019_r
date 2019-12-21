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
    y <- node[1]
    x <- node[2]
    l <- node[3]
    d <- node[4] + 1

    neighbours <- fdeque(initialSize = 8)

    if({xx <- map[y - 1, x]; length(xx) > 0 && xx != "#"}) {
      neighbours$push(c(y - 1, x, l, d))
    }
    if(y + 1 <= nrow(map) && {xx <- map[y + 1, x]; xx != "#"}) {
      neighbours$push(c(y + 1, x, l, d))
    }
    if({xx <- map[y, x - 1]; length(xx) > 0 && xx != "#"}) {
      neighbours$push(c(y, x - 1, l, d))
    }
    if(x + 1 <= ncol(map) && {xx <- map[y, x + 1]; xx != "#"}) {
      neighbours$push(c(y, x + 1, l, d))
    }

    # If on a portal
    if(map[y, x] != "." && map[y, x] != "AA" && map[y, x] != "ZZ") {
      #browser()

      #message("Zooming through ", map[y, x])
      portalLocations <- which(map == map[y, x], arr.ind = TRUE)
      otherEnd <- portalLocations[!apply(portalLocations, 1, function(x)x["row"] == node[1] && x["col"] == node[2]), ]
      otherEnd <- c(otherEnd, l, d)

      portalPermitted <- FALSE
      if(recursive) {
        if(otherEnd[1] == 1 || otherEnd[1] == nrow(map) ||
           otherEnd[2] == 1 || otherEnd[2] == ncol(map)) {
          # Outer portal, move up
          otherEnd[3] <- l + 1
          portalPermitted <- TRUE
        } else if(l > 0 && otherEnd[1] > 1 && otherEnd[2] > 1) {
          otherEnd[3] <- l - 1
          portalPermitted <- TRUE
        }
      }
      if(portalPermitted || !recursive) {
        neighbours$push(otherEnd)
      }
    }

    neighbours$values()
  }

  getNodeId <- function(node, suffix) {
    #paste(c(node, suffix), collapse = "-")
    sprintf("%d-%d-%d-%s", node[1], node[2], node[3], suffix)
  }

  doTheThing <- function(map, recursive = FALSE) {
    if(recursive && grepl("example2", path)) {
      stop("Nope, that won't work!")
    }

    start <- c(which(map == "AA", arr.ind = TRUE), 0, 0)
    end <- c(which(map == "ZZ", arr.ind = TRUE), 0, NA)

    steps <- fdeque()

    edge <- fdeque()
    edge$push(start)
    done <- c(getNodeId(start, "AA"))
    doneSize <- 1
    doneElems <- 1
    while(!all(edge$peekBack()[1:3] == end[1:3])) {
      node <- edge$shift()
      nodeId <- getNodeId(node, map[node[1], node[2]])

      if(map[node[1], node[2]] != ".") {
        message("Pulsing... at node ", nodeId)
      }

      neighbours <- getNeighbours(map, node, recursive)

      for(n in neighbours) {
        id <- getNodeId(n, map[n[1], n[2]])
        steps$push(c(nodeId, id))
        if(id %in% done) {
          next;
        }

        if(doneSize == doneElems) {
          done <- c(done, vector("character", doneSize))
          doneSize <- 2*doneSize
        }

        doneElems <- doneElems + 1
        done[doneElems] <- id

        edge$push(n)
      }
    }

    at <- getNodeId(end, "ZZ")
    pathTaken <- fdeque()
    while(at != getNodeId(start, "AA")) {
      pathTaken$push(at)

      nextStep <- steps$pop()
      while(nextStep[2] != at) {
        nextStep <- steps$pop()
      }
      at <- nextStep[1]
    }


    # Sorry, nothing to see here at the moment
    # message("the last dist")
    pathTaken$values()
  }

  #doTheThing(map, FALSE)

  t0 <- Sys.time()
  aa <- doTheThing(map, TRUE)
  t1 <- Sys.time()

}
