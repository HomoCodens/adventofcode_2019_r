if(!require("datastructures")) {
  install.packages("datastructures")
}

if(!require("rstackdeque")) {
  install.packages("rstackdeque")
}

if(!require("dequer")) {
  install.packages("dequer")
}

stoopyStackInsert <- function(st, x) {
  if(st$head == length(st)) {
    st$data <- c(st$data, st$data)
  }

  st$head <- st$head + 1
  st$data[[st$head]] <- x

  st
}

stoopyStack <- function() {
  list(
    head = 0,
    data = list(1)
  )
}

closyStack <- function() {
  head <- 0
  data <- list(1)

  insert <- function(x) {
    if(head == length(data)) {
      data <<- c(data, data)
    }

    head <<- head + 1
    data[[head]] <<- x
  }

  list(
    insert = insert
  )
}


nSamples <- 10000

microbenchmark::microbenchmark(rstackdeque = {
  st <- rstackdeque::rstack()
  for(i in seq(nSamples)) {
    st <- rstackdeque::insert_top(st, i)
  }
}, datastructures = {
  st <- datastructures::stack()
  for(i in seq(nSamples)) {
    st <- datastructures::insert(st, i)
  }
}, dequer = {
  st <- dequer::stack()
  for(i in seq(nSamples)) {
    dequer::push(st, i)
  }
}, stoopyStack = {
  st <- stoopyStack()
  for(i in seq(nSamples)) {
    st <- stoopyStackInsert(st, i)
  }
}, closyStack = {
  st <- closyStack()
  for(i in seq(nSamples)) {
    st$insert(i)
  }
}, times = 20)

# Hrmmm....

#> Unit: milliseconds
#>            expr      min         lq      mean     median       uq       max
#>     rstackdeque 166.5885 180.859851 426.12172 194.006301 219.0693 2521.0269
#>  datastructures 175.6856 182.285201 973.69553 188.402101 649.7139 4789.9682
#>          dequer  74.6677  78.809501 383.04002  84.963850  96.5524 3978.7078
#>     stoopyStack 414.2794 447.273652 692.18672 455.884051 873.1116 1995.1046
#>      closyStack   8.2765   8.476851  10.07082   8.773201  10.9940   16.3553
#>  neval
#>     20
#>     20
#>     20
#>     20
#>     20
