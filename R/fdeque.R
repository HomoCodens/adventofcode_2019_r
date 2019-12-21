fdeque <- function(initialSize = 1) {
  # Welcome to side effect hell! *Mwahahahaaa*


  # state -------------------------------------------------------------------

  head <- 0
  tail <- 1
  nElements <- 0
  size <- initialSize  # Start at size 1 to avoid weird edge cases
  data <- vector("list", initialSize)



  # housekeeping ------------------------------------------------------------

  bumpHead <- function() {
    head <<- (head %% size) + 1
  }

  dumpHead <- function() {
    head <<- ifelse(head == 1, size, head - 1)
  }

  bumpTail <- function() {
    tail <<- (tail %% size) + 1
  }

  dumpTail <- function() {
    tail <<- ifelse(tail == 1, size, tail - 1)
  }

  grow <- function() {
      if(tail == 1) {
        # Data neatly fills whole array
        data <<- c(data, vector("list", size))
      } else {
        # Data is split somewhere in the middle
        data <<- c(data[1:head], vector("list", size), data[tail:size])
        tail <<- tail + size
      }
      size <<- size*2
  }

  # helpers -----------------------------------------------------------------

  empty <- function() {
    nElements == 0
  }

  contains <- function(x) {
    if(any(data[1:head] == x)) {
      return(TRUE)
    } else if(tail > 1 && any(data[tail:size] == x)) {
        return(TRUE)
    }

    return(FALSE)
  }

  values <- function() {
    if(tail == 1) {
      data[tail:head]
    } else {
      c(data[head:1], data[size:tail])
    }
  }

  # front ops ---------------------------------------------------------------

  push <- function(x) {
    if(nElements == size) {
      grow()
    }

    bumpHead()
    data[[head]] <<- x
    nElements <<- nElements + 1
  }

  pop <- function() {
    if(nElements == 0) {
      stop("This what ever it is is empty!")
    }

    nElements <<- nElements - 1
    out <- data[[head]]
    dumpHead()
    out
  }

  peekFront <- function() {
    data[[head]]
  }

  # back ops (if you're into that kind of thing) -----------------------------

  unshift <- function(x) {
    if(nElements == size) {
      grow()
    }

    dumpTail()
    data[[tail]] <<- x
    nElements <<- nElements + 1
  }

  shift <- function() {
    if(nElements == 0) {
      stop("This stack is empty!")
    }

    nElements <<- nElements - 1
    out <- data[[tail]]
    bumpTail()
    out
  }

  peekBack <- function() {
    data[[tail]]
  }


  list(
    push = push,
    pop = pop,
    peekFront = peekFront,
    unshift = unshift,
    shift = shift,
    peekBack = peekBack,
    empty = empty,
    contains = contains,
    values = values
  )
}
