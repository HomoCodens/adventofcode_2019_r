day12 <- function(path) {
  l <- readLines(path)

  moons <- data.table(
    x = as.numeric(gsub("<x=(.*?),.*", "\\1", l)),
    y = as.numeric(gsub(".*y=(.*?),.*", "\\1", l)),
    z = as.numeric(gsub(".*z=(.*)>", "\\1", l)),
    vx = 0,
    vy = 0,
    vz = 0
  )

  sumilate <- function(bodies, steps) {
    for(i in seq(steps)) {
      for(j in seq(nrow(bodies))) {
        bodies[j, vx := vx + sum(sign(bodies[-j, x] - x))]
        bodies[j, vy := vy + sum(sign(bodies[-j, y] - y))]
        bodies[j, vz := vz + sum(sign(bodies[-j, z] - z))]
      }

      bodies[, x := x + vx][, y := y + vy][, z := z + vz]
    }

    bodies
  }

  moons1 <- sumilate(data.table(moons), 1000)
  pot1 <- moons1[, sum((abs(x)+abs(y)+abs(z))*(abs(vx)+abs(vy)+abs(vz)))]

  message(pot1)

  sumilateLoop <- function(x0, vx0) {
    steps <- 0
    x <- x0
    vx <- vx0
    repeat { # Never thougt I'd ever use that one
      if(steps %% 1000 == 0) {
        message(steps)
      }
      for(i in seq(length(x))) {
        vx[i] <- vx[i] + sum(sign(x[-i] - x[i]))
      }
      x <- x + vx
      steps <- steps + 1

      if(all(x0 == x & vx == vx0)) {
        break;
      }
    }

    steps
  }

  lx <- sumilateLoop(moons[, x], moons[, vx])
  ly <- sumilateLoop(moons[, y], moons[, vy])
  lz <- sumilateLoop(moons[, z], moons[, vz])
}
