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

  # COM$Prune(pruneFun = function(x){Distance(COM, x) < 9})
  #
  # COM$Do(function(n) {
  #   n$nOrbits <- n$level
  # })

  message(sprintf("There are %d orbits in total.", sum(ToDataFrameTree(COM, "level")$level - 1)))

  # COM$Do(function(n) {
  #   n$nOrbits <- Cumulate(n, "nOrbits", sum)
  # }, traversal = "post-order")
  #
  # print(COM, "nOrbits")
}
