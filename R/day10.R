# TODO: document cleverness. now sleep tho
day10 <- function(path) {
  l <- readLines(path)

  asteroids <- do.call(rbind, lapply(l, function(x){strsplit(x, "")[[1]]}))

  w <- ncol(asteroids)
  h <- nrow(asteroids)

  ind <- which(t(asteroids) == "#") - 1
  x <- ind %% w
  y <- floor(ind / w)

  coords <- data.table(x = x, y = y)

  my_little_atan2 <- function(x, y) {
    if(x == 0 && y < 0) {
      -pi/2
    } else if(x == 0 && y > 0) {
      pi/2
    } else if(x < 0 && y == 0) {
      -pi
    } else if(x > 0) {
      atan(y/x)
    } else if(x < 0 && y > 0) {
      atan(y/x) + pi
    } else {
      atan(y/x) - pi
    }
  }

  get_n_visible <- function(coords, id) {
    pos <- coords[id, ]
    coords <- coords[-id, ]


    # Shift origin to asteroid i
    coords[, x := x - pos$x][, y := y - pos$y]

    coords[, angle := my_little_atan2(x, y), by = seq(nrow(coords))]

    coords[, uniqueN(angle)]

    #coords[, mhd := abs(x) + abs(y)]
    #coords[, visible := TRUE]

    # n_blocked <- 0
    # for(i in seq(nrow(coords))) {
    #   if(!coords[i, visible]) {
    #     # What we already don't see can't block our LOS
    #     next;
    #   }
    #   for(j in seq(nrow(coords))) {
    #     if(i == j) {
    #       next;
    #     }
    #
    #     # alternatively: by angle, get the closest one, count
    #     if(coords[i, angle] == coords[j, angle] && coords[j, mhd] < coords[i, mhd]) {
    #       message(sprintf("It sure does look like (%d, %d) is blocking line of sight to (%d, %d)",
    #                       coords[j, x], coords[j, y], coords[i, x], coords[i, y]))
    #       n_blocked <- n_blocked + 1
    #     }
    #   }
    #}

    #nrow(coords) - n_blocked
  }

  n_visible <- sapply(1:nrow(coords), function(i){get_n_visible(coords, i)})
  besteroid <- which.max(n_visible)

  message(max(n_visible))

  best_pos <- coords[besteroid, ]
  coords <- coords[-besteroid, ]

  coords[, x_rel := x - best_pos$x][, y_rel := y - best_pos$y]
  # Cheat a little to get the gun pointing "up" and rotating the right way
  coords[, angle := my_little_atan2(-y_rel, x_rel), by = seq(nrow(coords))]
  coords[angle < 0, angle := angle + 2*pi]
  coords[, dist := sqrt(x_rel*x_rel + y_rel*y_rel)]
  setorder(coords, angle, dist)
  coords[, sweep := 1:.N, by = angle]
  setorder(coords, sweep, angle, dist)

  coords[200, message(sprintf("It appears the one at (%d, %d) gets obliterated as the 200th. That's %d.", x, y, 100*x+y))]
}
