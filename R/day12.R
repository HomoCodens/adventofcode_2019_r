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
}
