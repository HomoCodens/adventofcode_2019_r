day6 <- function(path) {
  orbits <- fread(path, sep = ")", head = FALSE)
  names(orbits) <- c("orbitee", "orbiter")

  nodes <- list()
  for(i in seq(nrow(orbits))) {
    orbiter <- orbits[i, orbiter]
    orbitee <- orbits[i, orbitee]

    if(is.null(nodes[[orbitee]])) {
      nodes[[orbitee]] <- Node$new(orbitee)
    }

    if(is.null(nodes[[orbiter]])) {
      nodes[[orbiter]] <- Node$new(orbiter)
    }

    nodes[[orbitee]]$AddChildNode(nodes[[orbiter]])
  }


  # For the first time in forever, there'll be music, there'll be light... *sing*
  COM <- nodes[["COM"]]

  # data.tree is not so smart after all
  getNOrbits <- function(n) {
    agg <- 0
    if(length(n$children) > 0) {
      agg <- sum(sapply(n$children, getNOrbits))
    }
    agg + n$level - 1
  }

  message(sprintf("There are %d orbits in total.", getNOrbits(COM)))

  message(sprintf("To get to Santa we need %d orbital transfers...", Distance(FindNode(COM, "SAN"), FindNode(COM, "YOU")) - 2))
}
