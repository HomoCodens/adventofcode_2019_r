day14 <- function(path = "inst/input/day14/input.txt") {
  l <- readLines(path)

  parseReagent <- function(x) {
    # regmatches is so burdensome
    thing <- gsub("\\d+ ([A-Z]+)$", "\\1", x)
    amount <- as.numeric(gsub("(\\d+) [A-Z]+$", "\\1", x))

    out <- list()
    out[[thing]] <- amount
    out
  }

  formulae <- list()
  for(ll in l) {
    parts <- strsplit(ll, " => ")[[1]]

    output <- unlist(parseReagent(parts[2]))

    input <- unlist(lapply(strsplit(parts[1], ", ")[[1]], parseReagent), recursive = FALSE)

    formulae[[names(output)]] <- list(
      output = output,
      input = input
    )
  }

  getComplexities <- function(graph, at, complexity) {
    complexities <- numeric(length(graph))
    names(complexities) <- names(graph)
    complexities[at] <- complexity

    thingsToProcess <- names(graph[[at]]$input)

    if(length(thingsToProcess) == 1 && thingsToProcess == "ORE") {
      out <- complexity
      names(out) <- at
      return(out)
    }

    for(t in thingsToProcess) {
      x <- getComplexities(graph, t, complexity + 1)
      complexities[names(x)] <- pmax(complexities[names(x)], x)
    }

    complexities
  }

  complexities <- sort(getComplexities(formulae, "FUEL", 0))

  calculateOreNeeded <- function(formulae, complexities, nFuel = 1) {
    needed <- numeric(length(formulae) + 1)
    names(needed) <- c(names(formulae), "ORE")

    haveded <- needed

    needed["FUEL"] <- nFuel

    for(reagent in names(complexities)) {
      reaction <- formulae[[reagent]]
      remainder <- needed[reagent] - haveded[reagent]
      nReactions <- ceiling(remainder/reaction$output)
      needed[reagent] <- 0

      for(ingredient in names(reaction$input)) {
        needed[ingredient] <- needed[ingredient] + nReactions*reaction$input[[ingredient]]
      }

      haveded[reagent] <- nReactions*reaction$output - remainder
    }

    needed["ORE"]
  }


  lb <- 1
  ub <- 1e12
  # Divide, conquer, success!
  while(ub - lb > 1) {
    mb <- lb + floor((ub-lb)/2)
    oreNeeded <- calculateOreNeeded(formulae, complexities, mb)
    message(sprintf("[%.0f, %.0f], testing %.0f, needing %.0f ore", lb, ub, mb, oreNeeded))
    if(oreNeeded <= 1e12) {
      lb <- mb
    } else {
      ub <- mb
    }
  }

  message("We'll need a grand total of ", calculateOreNeeded(formulae, complexities, nFuel = 1), " ore, cap'n!")
  message("With that much ore we can make ", lb, " many fuels.")
}
