day15 <- function(path = "inst/input/day15/input.txt") {
  tape <- readICCTape(path)

  repairDroidProgram <- function(frameTime = 0.2, debug = 0) {

    debugFcn <- function(msg, lvl) {
      if(lvl <= debug) {
        message(msg)
      }
    }
    map <- matrix("d")
    pos <- c(0, 0)
    dir <- 1
    mapOffset <- c(1, 1) # Position of origin on the map, needed to calculate matrix indices

    graphOfMap <- NULL
    posOfThing <- c(NA, NA)

    # Strategy 1: move north until hitting a wall, then hug that wall
    atWall <- FALSE
    wallAt <- NA

    lastPosWasLeak <- FALSE

    stepsTaken <- 0

    getMapCoords <- function(x) {
      rev(x) + mapOffset
    }

    getV <- function(direction) {
      if(direction == 1) {
        c(0, -1)
      } else if(direction == 2) {
        c(0, 1)
      } else if(direction == 3) {
        c(-1, 0)
      } else {
        c(1, 0)
      }
    }

    growMap <- function(direction) {
      debugFcn(sprintf("Growing map in direction %d...", direction), 1)

      if(direction == 1) {
        map <<- rbind(matrix(" ", 1, ncol(map)), map)
        mapOffset[1] <<- mapOffset[1] + 1
      } else if(direction == 2) {
        map <<- rbind(map, matrix(" ", 1, ncol(map)))
      } else if(direction == 3) {
        map <<- cbind(matrix(" ", nrow(map), 1), map)
        mapOffset[2] <<- mapOffset[2] + 1
      } else {
        map <<- cbind(map, matrix(" ", nrow(map), 1))
      }
    }

    function(vIn = NULL) {
      debugFcn(paste("Map is this big: ", paste(dim(map), collapse = ", ")), 3)

      if(!is.null(vIn)) { # Getting state from robot
        debugFcn(sprintf("Got in a %d...", vIn), 1)

        target <- pos + getV(dir)

        posOnMap <- getMapCoords(pos)
        targetOnMap <- getMapCoords(target)

        stepWasMade <- FALSE

        # Note: The origin being in a cul de sac may not be the case for all inputs
        if(map[targetOnMap[1], targetOnMap[2]] == "O") {
          message(sprintf("Well, the fastest way to get there is %d long",
                          distances(graphOfMap, "0-0", paste(posOfThing, collapse = "-"))))
          message(sprintf("Also, to fill the whole sector with sweet sweet O2 will take %d minutes",
                          max(distances(graphOfMap, paste(posOfThing, collapse = "-")))))
          stop("No other way of getting this thing to stop...")
        }

        if(vIn == 0) {        # Wall

          debugFcn(sprintf("Whoops, there's a wall at (%d, %d)", targetOnMap[1], targetOnMap[2]), 1)

          map[targetOnMap[1], targetOnMap[2]] <<- "#"
          wallAt <<- dir
        } else if(vIn == 1) { # Movement successful

          stepsTaken <<- stepsTaken + 1
          debugFcn("Moving...", 1)
          nodeId <- paste(pos, collapse = "-")
          targetId <- paste(target, collapse = "-")

          map[posOnMap[1], posOnMap[2]] <<- ifelse(lastPosWasLeak, "X",
                                                   ifelse(all(pos == c(0, 0)), "O", "."))


          pos <<- target
          map[targetOnMap[1], targetOnMap[2]] <<- "d"
          lastPosWasLeak <<- FALSE
          stepWasMade <- TRUE
        } else if(vIn == 2) { # Target reached

          debugFcn("Found ze leak!", 1)
          nodeId <- paste(pos, collapse = "-")
          targetId <- paste(target, collapse = "-")


          map[posOnMap[1], posOnMap[2]] <<- "."
          pos <<- target
          posOfThing <<- pos

          # Presumably the droid will halt at this point
          # NOPE!
          map[targetOnMap[1], targetOnMap[2]] <<- "D"

          lastPosWasLeak <<- TRUE
          stepWasMade <- TRUE
        }

        if(stepWasMade) {
          if(is.null(graphOfMap)) {
            graphOfMap <<- graph(c(nodeId, targetId), directed = FALSE)
          } else {
            if(!targetId %in% names(V(graphOfMap))) {
              graphOfMap <<- add_vertices(graphOfMap, 1, name = targetId)
            }
            if(!are_adjacent(graphOfMap, nodeId, targetId)) {
              graphOfMap <<- add_edges(graphOfMap, c(nodeId, targetId))
            }
          }
        }

        cat("\014")
        cat(paste(apply(map, 1, paste, collapse = ""), collapse = "\n"))
        cat("\n")

        Sys.sleep(frameTime)
      } else { # Robot requesting command
        debugFcn("Droid requesting command...", 1)

        # north (1), south (2), west (3), and east (4)
        posOnMap <- getMapCoords(pos)

        debugFcn(sprintf("Droid currently at (%d, %d) (map coords (%d, %d))",
                         pos[1], pos[2], posOnMap[1], posOnMap[2]), 1)

        # Not hit a wall yet and at top of map
        if(!atWall && posOnMap[1] == 1) {
          growMap(1)
          1
        } else {
          if(!atWall) {
            # Mwahaha, naming
            wallAt <<- 1
            atWall <<- TRUE
          }

          # Hug the wall on the left side of teh droid
          if(wallAt == 1) {
            if(map[posOnMap[1] - 1, posOnMap[2]] %in% c(" ", ".")) {
              dir <<- 1
              wallAt <<- 3
            } else {
              dir <<- 4
            }
          } else if(wallAt == 2) {
            if(map[posOnMap[1] + 1, posOnMap[2]] %in% c(" ", ".")) {
              dir <<- 2
              wallAt <<- 4
            } else {
              dir <<- 3
            }
          } else if(wallAt == 3) {
            if(map[posOnMap[1], posOnMap[2] - 1] %in% c(" ", ".")) {
              dir <<- 3
              wallAt <<- 2
            } else {
              dir <<- 1
            }
          } else {
            if(map[posOnMap[1], posOnMap[2] + 1] %in% c(" ", ".")) {
              dir <<- 4
              wallAt <<- 1
            } else {
              dir <<- 2
            }
          }

          if(posOnMap[1] == 1) {
            growMap(1)
            posOnMap <- getMapCoords(pos)
          }
          if(posOnMap[1] == nrow(map)) {
            growMap(2)
            posOnMap <- getMapCoords(pos)
          }
          if(posOnMap[2] == 1) {
            growMap(3)
            posOnMap <- getMapCoords(pos)
          }
          if(posOnMap[2] == ncol(map)) {
            growMap(4)
            posOnMap <- getMapCoords(pos)
          }

          debugFcn(sprintf("Sending out a %d...", dir), 1)
          dir
        }
      }
    }
  }

  droid <- repairDroidProgram(0.0, debug = 0)

  runIntCodeComputer(list(
    list(
      tape = tape,
      iccin = droid,
      iccout = droid
    )))
}
