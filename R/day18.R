day18 <- function(path = "inst/input/day18/input.txt") {
  l <- readLines(path)
  map <- do.call(rbind, unlist(lapply(l, strsplit, ""), recursive = FALSE))

  o <- which(map == "@")
  xo <- col(map)[o]
  yo <- row(map)[o]

  getAdjacentNodes <- function(map, x, y) {

    edge <- fdeque()
    edge$push(c(x, y, 0))

    done <- matrix(FALSE, nrow(map), ncol(map))
    neighbours <- fdeque(32)

    dy <- c(0, 0, -1, 1)
    dx <- c(-1, 1, 0, 0)

    while(edge$size() > 0) {
      at <- edge$shift()

      x <- at[1]
      y <- at[2]
      dist <- at[3]

      done[y, x] <- TRUE

      for(i in 1:4) {
        xx <- x + dx[i]
        yy <- y + dy[i]
        cand <- map[yy, xx]
        if(cand != "#" && !done[yy, xx]) {
          if(cand != ".") {
            neighbours$push(list(symbol = cand, dist = dist + 1))
          }

          if(!cand %in% LETTERS) {
            edge$push(c(xx, yy, dist + 1))
          }
        }
      }
    }
    rbindlist(neighbours$values())
  }


  # 1) Build a graph of every Thing worth visiting + distances --------------
  edges <- fdeque()

  for(node in which(!map %in% c("#", "."))) {
    xo <- col(map)[node]
    yo <- row(map)[node]
    neighbourhood <- getAdjacentNodes(map, xo, yo)
    neighbourhood$from <- map[node]
    edges$push(neighbourhood)
  }

  edges <- rbindlist(edges$values())[, .(from, to = symbol, dist)]

  # getAdjacentNodes of graph representation
  walkTheGraph <- function(graph, from, keys) {
    openDoors <- toupper(keys)
    edge <- fdeque()
    edge$push(list(from, 0))

    reachables <- fdeque()

    done <- vector(length = nrow(graph))
    names(done) <- graph[, unique(from)]

    while(edge$size() > 0) {
      at <- edge$shift()

      node <- at[[1]]
      d <- at[[2]]

      done[node] <- TRUE

      neighbours <- graph[from == node]
      # Whee, data.tables
      for(i in 1:nrow(neighbours)) {
        to <- neighbours[i, to]
        dist <- neighbours[i, dist]

        if(!done[to]) {
          if(to %in% letters && !to %in% keys) {
            reachables$push(list(node = to, dist = d + dist))
          }

          done[to] <- TRUE

          if(to %in% c(letters, openDoors)) {
            edge$push(list(to, dist + d))
          }
        }
      }
      # neighbours[, {
      #   message("Checking neighbour ", to)
      #   if(!done[to]) {
      #     message("Not been here yet...")
      #     done[to] <- TRUE
      #
      #     reachables$push(list(to, dist + d))
      #
      #     if(to %in% c(letters, openDoors)) {
      #       edge$push(list(to, dist + d))
      #       message("Oh, passable... going on...", to)
      #     }
      #   }
      # }, by = 1:nrow(neighbours)]
    }
    rbindlist(reachables$values())
  }

  quicksaves <- list()
  nCalls <- 0

  doTheMarine <- function(graph, at = "@", keysGathered = NULL) {
    # I don't think there was quicksaving in Doom tho
    worldId <- paste(c(keysGathered, "-", at), collapse = "")

    nCalls <<- nCalls + 1
    if(nCalls %% 10 == 0) {
      message(nCalls)
      sqs <- length(quicksaves)
      message("size of quicksaves: ", sqs)
      if(sqs > 0) {
        nHits <- sum(sapply(quicksaves, `[[`, "hits"))
        message("Number of cache hits: ", nHits)
        message(sprintf("Ratio: %.1f%%", 100*nHits/nCalls))
      }
    }
    if(!is.null(quicksaves[[worldId]])) {
      #message("Ohai, a cache!")
      quicksaves[[worldId]]$hits <<- quicksaves[[worldId]]$hits + 1
      return(quicksaves[[worldId]]$value)
    } else {
      reachableKeys <- walkTheGraph(graph, at, keysGathered)

      if(reachableKeys[, .N == 0]) {
        return(0)
      }

      optimum <- Inf
      for(i in 1:nrow(reachableKeys)) {
        d <- doTheMarine(graph, reachableKeys[i, node], sort(c(keysGathered, reachableKeys[i, node])))

        dist <- reachableKeys[i, dist]

        if(dist + d < optimum) {
          optimum <- dist + d
        }
      }

      quicksaves[[worldId]] <<- list(
        value = optimum,
        hits = 0)
      return(optimum)
    }
  }

  # Soo I just now realized that the @ symbolizes where we are... at *badum-tss*
  doTheMarine(edges)
}


# ## Sigh... R is so not the tool for this
# day18 <- function(path = "inst/input/day18/input.txt") {
#   l <- readLines(path)
#   map <- do.call(rbind, unlist(lapply(l, strsplit, ""), recursive = FALSE))
#
#   # Yes, it matters
#   one2four <- seq(4)
#
#   findAvailableKeys <- function(map, x, y) {
#     edge <- fdeque()
#     edge$push(c(x, y, 0))
#
#     visited <- matrix(FALSE, nrow(map), ncol(map))
#
#     keys <- fdeque()
#
#     dy <- c(0, 0, -1, 1)
#     dx <- c(-1, 1, 0, 0)
#
#     freeTypes <- c(letters, ".", "@")
#     blockingTypes <- c(LETTERS, "#")
#     keyTypes <- letters
#
#     while(!edge$empty()) {
#       at <- edge$shift()
#
#       x <- at[1]
#       y <- at[2]
#       dist <- at[3]
#
#       for(i in one2four) {
#         xx <- x + dx[i]
#         yy <- y + dy[i]
#         cand <- map[yy, xx]
#         if(!cand %in% blockingTypes) {
#           id <- sprintf("%d-%d", xx, yy)
#           if(!visited[yy, xx]) {
#             edge$push(c(xx, yy, dist + 1))
#
#             if(cand %in% keyTypes) {
#               keys$push(list(x = xx, y = yy, dist = dist + 1, keyId = cand))
#             }
#
#             visited[yy, xx] <- TRUE
#           }
#         }
#       }
#     }
#
#     keys$values()
#   }
#
#   quicksaves <- list()
#   nCalls <- 0
#
#   doTheMarine <- function(map, keysFound = NULL) {
#     o <- which(map == "@")
#     x <- col(map)[o]
#     y <- row(map)[o]
#
#     nCalls <<- nCalls + 1
#
#     if(nCalls > 1000) {
#       return(0)
#     }
#
#     if(!(nCalls %% 100)) {
#       message(nCalls)
#       message("Btw, we now have ", length(quicksaves), " saves to go on.")
#       if(length(quicksaves)) {
#         nHits <- sum(sapply(quicksaves, `[[`, "strikes"))
#         message("Those we have hit ", nHits, " times")
#         message(sprintf("Cache ratio: %.1f%%", 100*nHits/nCalls))
#       }
#     }
#
#     saveId <- paste(c(keysFound, o), collapse = "")
#     #message(saveId)
#     if(!is.null(quicksaves[[saveId]])) {
#       #message("Struck the cache!")
#       quicksaves[[saveId]]$strikes <<- quicksaves[[saveId]]$strikes + 1
#       return(quicksaves[[saveId]]$value)
#     } else {
#       keys <- findAvailableKeys(map, x, y)
#       if(is.null(keys[[1]])) {
#         return(0)
#       }
#       map[o] <- "."
#
#       out <- 0
#       if(length(keys) > 0) {
#         optimum <- Inf
#         for(key in keys) {
#           newMap <- map
#           newMap[newMap == toupper(key$keyId)] <- "."
#           newMap[newMap == key$keyId] <- "."
#           newMap[key$y, key$x] <- "@"
#           d <- doTheMarine(newMap, sort(c(keysFound, key$keyId)))
#           if(d + key$dist < optimum) {
#             optimum <- d + key$dist
#           }
#         }
#         out <- optimum
#       }
#       quicksaves[[saveId]] <<- list(
#         value = out,
#         strikes = 0
#       )
#
#       return(out)
#     }
#   }
#
#   doTheMarine(map)
# }

# day18 <- function(path = "inst/input/day18/input.txt") {
#   l <- readLines(path)
#   map <- do.call(rbind, unlist(lapply(l, strsplit, ""), recursive = FALSE))
#
#   o <- which(map == "@")
#   xo <- col(map)[o]
#   yo <- row(map)[o]
#
#   getAdjacentNodes <- function(map, x, y) {
#
#     edge <- list(
#       list(
#         x = x,
#         y = y,
#         dist = 0
#       )
#     )
#
#     done <- c()
#     neighbours <- list()
#
#     dy <- c(0, 0, -1, 1)
#     dx <- c(-1, 1, 0, 0)
#
#     while(length(edge) > 0) {
#       at <- edge[[1]]
#       edge <- edge[-1]
#
#       x <- at$x
#       y <- at$y
#       dist <- at$dist
#
#       done <- c(done, paste(c(x, y), collapse = "-"))
#
#       for(i in seq(4)) {
#         xx <- x + dx[i]
#         yy <- y + dy[i]
#         cand <- map[yy, xx]
#         if(cand != "#") {
#           id <- paste(c(xx, yy), collapse = "-")
#           if(!(id %in% done)) {
#             if(cand != ".") {
#               neighbours[[cand]] <- dist + 1
#             }
#
#             if(!cand %in% LETTERS) {
#               edge[[length(edge) + 1]] <- list(
#                 x = xx,
#                 y = yy,
#                 dist = dist + 1
#               )
#             }
#           }
#         }
#       }
#     }
#     neighbours
#   }
#
#
#   # 1) Build a graph of every Thing worth visiting + distances --------------
#   edges <- data.table()
#
#   for(node in which(!map %in% c("#", "."))) {
#     xo <- col(map)[node]
#     yo <- row(map)[node]
#     neighbourhood <- getAdjacentNodes(map, xo, yo)
#     edges <- rbind(edges, data.table(
#       from = map[node],
#       to = names(neighbourhood),
#       dist = unlist(neighbourhood)
#     ))
#   }
#
#   removeFromWorld <- function(graph, node) {
#     incoming <- graph[to == node]
#
#     newEdges <- data.table()
#     for(i in seq(nrow(incoming)-1)) {
#       for(j in (i+1):nrow(incoming)) {
#         newEdges <- rbind(newEdges, data.table(
#           from = c(incoming[j, from], incoming[i, from]),
#           to = c(incoming[i, from], incoming[j, from]),
#           dist = incoming[i, dist] + incoming[j, dist]
#         ))
#       }
#     }
#
#     rbind(
#       graph[from != node & to != node],
#       newEdges
#     )[, .SD[dist == min(dist)], by = c("from", "to")] # This smells slow
#   }
#
#   quicksaves <- list()
#   nCalls <- 1
#
#   doTheMarine <- function(graph,  keysGathered = NULL, atKey = NULL) {
#     # I don't think there was quicksaving in Doom tho
#     worldId <- paste(c(keysGathered, "-", atKey), collapse = "")
#
#     nCalls <<- nCalls + 1
#     if(nCalls %% 100 == 0) {
#       message(nCalls)
#       message("size of quicksaves: ", length(quicksaves))
#       message("Number of cache hits: ", sum(sapply(quicksaves, `[[`, "hits")))
#     }
#     if(!is.null(quicksaves[[worldId]])) {
#       #message("Ohai, a cache!")
#       quicksaves[[worldId]]$hits <<- quicksaves[[worldId]]$hits + 1
#       return(quicksaves[[worldId]]$value)
#     } else {
#       graph <- graph[to != "@"]
#       candidates <- graph[from == "@" & to %in% letters]
#
#       optimum <- Inf
#       for(i in seq(nrow(candidates))) {
#         newGraph <- graph[from != "@"]
#
#         newDist <- Inf
#         if(nrow(newGraph) == 0) {
#           # Recursion hath ended
#           newDist <- candidates[i, dist]
#         } else {
#           key <- candidates[i, to]
#           door <- toupper(key)
#
#           # "Move"
#           newGraph[newGraph == key] <- "@"
#
#           # Open door
#           if(newGraph[, any(to == door)]) {
#             newGraph <- removeFromWorld(newGraph, door)
#           }
#
#           if(newGraph[, !all(to == "@")]) {
#             out <- doTheMarine(newGraph, sort(c(keysGathered, key)), key)
#
#             newDist <- out + candidates[i, dist]
#           } else {
#             # Nother way to end
#             newDist <- candidates[i, dist]
#           }
#         }
#
#         if(newDist < optimum) {
#           optimum <- newDist
#         }
#       }
#
#       quicksaves[[worldId]] <<- list(
#         value = optimum,
#         hits = 0)
#       return(optimum)
#     }
#   }
#
#   # Soo I just now realized that the @ symbolizes where we are... at *badum-tss*
#   doTheMarine(edges)
# }

# day18 <- function(path = "inst/input/day18/input.txt") {
#   l <- readLines(path)
#   map <- do.call(rbind, unlist(lapply(l, strsplit, ""), recursive = FALSE))
#
#   findAvailableKeys <- function(map, x, y) {
#     edge <- list(
#       list(
#         x = x,
#         y = y,
#         dist = 0
#       )
#     )
#
#     done <- c()
#     keys <- data.table()
#
#     dy <- c(0, 0, -1, 1)
#     dx <- c(-1, 1, 0, 0)
#
#     freeTypes <- c(letters, ".", "@")
#     blockingTypes <- c(LETTERS, "#")
#     keyTypes <- letters
#
#     while(length(edge) > 0) {
#       at <- edge[[1]]
#       edge <- edge[-1]
#
#       done <- c(done, paste(c(at$x, at$y), collapse = "-"))
#
#       x <- at$x
#       y <- at$y
#       dist <- at$dist
#
#       for(i in seq(4)) {
#         xx <- x + dx[i]
#         yy <- y + dy[i]
#         cand <- map[yy, xx]
#         if(!cand %in% blockingTypes) {
#           id <- paste(c(xx, yy), collapse = "-")
#           if(!id %in% done) {
#             edge[[length(edge) + 1]] <- list(
#               x = xx,
#               y = yy,
#               dist = dist + 1)
#
#             if(cand %in% keyTypes) {
#               keys <- rbind(keys, data.table(
#                 x = xx,
#                 y = yy,
#                 dist = dist + 1,
#                 keyId = cand
#               ))
#             }
#           }
#         }
#       }
#     }
#
#     keys
#   }
#
#   quicksaves <- list()
#   nCalls <- 0
#
#   doTheMarine <- function(map, keysFound = NULL) {
#     o <- which(map == "@")
#     x <- col(map)[o]
#     y <- row(map)[o]
#
#     nCalls <<- nCalls + 1
#     if(!(nCalls %% 10)) {
#       message(nCalls)
#       message("Btw, we now have ", length(quicksaves), " saves to go on.")
#       if(length(quicksaves)) {
#         message("Those we have hit ", sum(sapply(quicksaves, `[[`, "strikes")), " times")
#       }
#     }
#
#     saveId <- paste(c(keysFound, o), collapse = "")
#     #message(saveId)
#     if(!is.null(quicksaves[[saveId]])) {
#       #message("Struck the cache!")
#       quicksaves[[saveId]]$strikes <<- quicksaves[[saveId]]$strikes + 1
#       return(quicksaves[[saveId]]$value)
#     } else {
#       keys <- findAvailableKeys(map, x, y)
#       map[o] <- "."
#
#       out <- 0
#       if(nrow(keys) > 0) {
#         optimum <- Inf
#         for(i in seq(nrow(keys))) {
#           key <- keys[i, ]
#           newMap <- map
#           newMap[newMap == toupper(key$keyId)] <- "."
#           newMap[newMap == key$keyId] <- "."
#           newMap[key$y, key$x] <- "@"
#           d <- doTheMarine(newMap, sort(c(keysFound, key$keyId)))
#           if(d + key$dist < optimum) {
#             optimum <- d + key$dist
#           }
#         }
#         out <- optimum
#       }
#       quicksaves[[saveId]] <<- list(
#         value = out,
#         strikes = 0
#       )
#
#       return(out)
#     }
#   }
#
#   doTheMarine(map)
# }

# day18 <- function(path = "inst/input/day18/input.txt") {
#   l <- readLines(path)
#   map <- do.call(rbind, unlist(lapply(l, strsplit, ""), recursive = FALSE))
#
#   o <- which(map == "@")
#   xo <- col(map)[o]
#   yo <- row(map)[o]
#
#   getAdjacentNodes <- function(map, x, y) {
#
#     edge <- list(
#       list(
#         x = x,
#         y = y,
#         dist = 0
#       )
#     )
#
#     done <- c()
#     neighbours <- list()
#
#     dy <- c(0, 0, -1, 1)
#     dx <- c(-1, 1, 0, 0)
#
#     while(length(edge) > 0) {
#       at <- edge[[1]]
#       edge <- edge[-1]
#
#       x <- at$x
#       y <- at$y
#       dist <- at$dist
#
#       done <- c(done, paste(c(x, y), collapse = "-"))
#
#       for(i in seq(4)) {
#         xx <- x + dx[i]
#         yy <- y + dy[i]
#         cand <- map[yy, xx]
#         if(cand != "#") {
#           id <- paste(c(xx, yy), collapse = "-")
#           if(!(id %in% done)) {
#             if(cand != ".") {
#               neighbours[[cand]] <- dist + 1
#             }
#
#             if(!cand %in% LETTERS) {
#               edge[[length(edge) + 1]] <- list(
#                 x = xx,
#                 y = yy,
#                 dist = dist + 1
#               )
#             }
#           }
#         }
#       }
#     }
#     neighbours
#   }
#
#
#   # 1) Build a graph of every Thing worth visiting + distances --------------
#   edges <- data.table()
#
#   for(node in which(!map %in% c("#", "."))) {
#     xo <- col(map)[node]
#     yo <- row(map)[node]
#     neighbourhood <- getAdjacentNodes(map, xo, yo)
#     edges <- rbind(edges, data.table(
#       from = map[node],
#       to = names(neighbourhood),
#       dist = unlist(neighbourhood)
#     ))
#   }
#
#   removeFromWorld <- function(graph, node) {
#     incoming <- graph[to == node]
#
#     newEdges <- data.table()
#     for(i in seq(nrow(incoming)-1)) {
#       for(j in (i+1):nrow(incoming)) {
#         newEdges <- rbind(newEdges, data.table(
#           from = c(incoming[j, from], incoming[i, from]),
#           to = c(incoming[i, from], incoming[j, from]),
#           dist = incoming[i, dist] + incoming[j, dist]
#         ))
#       }
#     }
#
#     rbind(
#       graph[from != node & to != node],
#       newEdges
#     )[, .SD[dist == min(dist)], by = c("from", "to")] # This smells slow
#   }
#
#   doTheMarine <- function(graph, quickSaves = list(), keysGathered = NULL) {
#     # I don't think there was quicksaving in Doom tho
#     worldId <- paste(keysGathered, collapse = "")
#     if(!is.null(quickSaves[[worldId]])) {
#       world <- quickSaves[[worldId]]
#     } else {
#       graph <- graph[to != "@"]
#       candidates <- graph[from == "@" & to %in% letters]
#       graph <- graph[from != "@"]
#       for(i in nrow(candidates)) {
#         graph <- removeFromWorld(graph, toupper(candidates[i, to]))
#       }
#     }
#   }
#
#   # doTheMarine <- function(graph, dist = 0, best = Inf) {
#   #   message("Best: ", best, " Current: ", dist)
#   #
#   #   # We don't care about walking TO where we are
#   #   graph <- graph[to != "@"]
#   #
#   #   candidates <- graph[from == "@" & to %in% letters]
#   #
#   #   # Pruning clause
#   #   if(dist >= best) {
#   #     message("Nopeing with ", candidates[, .N], " candidates!!!")
#   #     return(
#   #       list(
#   #         dist = dist,
#   #         best = dist
#   #       )
#   #     )
#   #     message("If you can read this: RUN!!!")
#   #   }
#   #
#   #   # message(candidates[, to])
#   #
#   #   optimum <- Inf
#   #   for(i in seq(nrow(candidates))) {
#   #     newGraph <- graph[from != "@"]
#   #
#   #     if(nrow(newGraph) == 0) {
#   #       # Recursion hath ended
#   #       newDist <- candidates[i, dist]
#   #       message("Reached bottom for the umpteenth time with ", newDist + dist, "...")
#   #       if(best > newDist + dist) {
#   #         best <- newDist + dist
#   #         message("New highscore: ", best)
#   #       }
#   #     } else {
#   #       key <- candidates[i, to]
#   #       door <- toupper(key)
#   #
#   #       # "Move"
#   #       newGraph[newGraph == key] <- "@"
#   #
#   #       # Open door
#   #       if(newGraph[, any(to == door)]) {
#   #         if(newGraph[, !all(to %in% c(door, "@"))]) {
#   #           newGraph <- removeFromWorld(newGraph, door)
#   #           out <- doTheMarine(newGraph, dist + candidates[i, dist], best)
#   #
#   #           newDist <- out$dist + candidates[i, dist]
#   #           if(best > out$best) {
#   #             best <- out$best
#   #           }
#   #         } else {
#   #           # Nother way to end
#   #           newDist <- candidates[i, dist]
#   #           if(best > newDist + dist) {
#   #             best <- newDist + dist
#   #             message("New highscore, the other way round! ", best)
#   #           }
#   #         }
#   #       }
#   #     }
#   #
#   #     if(newDist < optimum) {
#   #       optimum <- newDist
#   #     }
#   #   }
#   #
#   #   list(
#   #     dist = optimum,
#   #     best = best
#   #   )
#   # }
#
#   # Soo I just now realized that the @ symbolizes where we are... at *badum-tss*
#   doTheMarine(edges)
# }

# day18 <- function(path = "inst/input/day18/input.txt") {
#   l <- readLines(path)
#   map <- do.call(rbind, unlist(lapply(l, strsplit, ""), recursive = FALSE))
#
#   findAvailableKeys <- function(map, x, y) {
#     edge <- list(
#       list(
#         x = x,
#         y = y,
#         dist = 0
#       )
#     )
#
#     done <- c()
#     keys <- list()
#
#     dy <- c(0, 0, -1, 1)
#     dx <- c(-1, 1, 0, 0)
#
#     freeTypes <- c(letters, ".", "@")
#     blockingTypes <- c(LETTERS, "#")
#     keyTypes <- letters
#
#     while(length(edge) > 0) {
#       at <- edge[[1]]
#       edge <- edge[-1]
#
#       done <- c(done, paste(c(at$x, at$y), collapse = "-"))
#
#       x <- at$x
#       y <- at$y
#       dist <- at$dist
#
#       for(i in seq(4)) {
#         xx <- x + dx[i]
#         yy <- y + dy[i]
#         cand <- map[yy, xx]
#         if(!cand %in% blockingTypes) {
#           id <- paste(c(xx, yy), collapse = "-")
#           if(!id %in% done) {
#             edge[[length(edge) + 1]] <- list(
#               x = xx,
#               y = yy,
#               dist = dist + 1)
#
#             if(cand %in% keyTypes) {
#               keys[[cand]] <- list(
#                 x = xx,
#                 y = yy,
#                 dist = dist + 1,
#                 keyId = cand
#               )
#             }
#           }
#         }
#       }
#     }
#
#     keys
#   }
#
#   doTheMarine <- function(map) {
#     o <- which(map == "@")
#     x <- col(map)[o]
#     y <- row(map)[o]
#
#     keys <- findAvailableKeys(map, x, y)
#     if(length(keys) == 0) {
#       return(0)
#     } else {
#       message(sapply(keys, '[[', "keyId"))
#       optimum <- Inf
#       for(i in seq(length(keys))) {
#         key <- keys[[i]]
#         newMap <- map
#         newMap[o] <- "."
#         newMap[key$y, key$x] <- "@"
#         # Open mungo bean (or something like that...)
#         newMap[newMap == toupper(key$keyId)] <- "."
#         d <- doTheMarine(newMap) + key$dist
#         if(d < optimum) {
#           optimum <- d
#         }
#       }
#
#       return(optimum)
#     }
#   }
#
#   doTheMarine(map)
# }

# findAvailableKeysSlowly <- function(map, x, y) {
#   edge <- data.table(
#     x = x,
#     y = y,
#     type = "@",
#     dist = 0
#   )
#   done <- data.table()
#   keys <- data.table()
#
#   while(nrow(edge)) {
#     # "pop"
#     at <- edge[1, ]
#     edge <- edge[-1]
#
#     dy <- c(0, 0, -1, 1)
#     dx <- c(-1, 1, 0, 0)
#
#     # Huh...
#     # https://stackoverflow.com/questions/4452039/rs-equivalent-to-ind2sub-sub2ind-in-matlab/14296902#14296902
#     neighbours <- map[cbind(at$y + dy, at$x + dx)]
#     free <- !grepl("[#A-Z]", neighbours)
#
#     candidates <- data.table(
#       x = at$x + dx[free],
#       y = at$y + dy[free],
#       type = neighbours[free],
#       dist = at$dist + 1)
#
#     if(nrow(done) == 0) {
#       done <- at
#       edge <- candidates
#     } else {
#       done <- rbind(done, at)
#       candidates <- candidates[done[candidates, on = c("x", "y")][, is.na(dist)]]
#       edge <- rbind(edge, candidates)
#     }
#
#     keys <- rbind(keys, candidates[grepl("[a-z]", type), .(x = x, y = y, keyId = type, dist = dist)])
#   }
#
#   keys
# }
