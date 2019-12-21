microbenchmark::microbenchmark(
  rstackdeque = {
    st <- rstackdeque::rpqueue()
    for(i in seq(nSamples)) {
      st <- rstackdeque::insert_back(st, i)
      if(popCnt %% popEvery == 0) {
        out <- rstackdeque::peek_front(st)
        st <- rstackdeque::without_front(st)
      }
      popCnt <- popCnt + 1
    }
  },
  datastructures = {
    st <- datastructures::queue()
    for(i in seq(nSamples)) {
      st <- datastructures::insert(st, i)
      if(popCnt %% popEvery == 0) {
        out <- datastructures::pop(st)
      }
      popCnt <- popCnt + 1
    }
  },
  fdeque = {
    st <- fdeque()
    for(i in seq(nSamples)) {
      st$push(i)
      if(popCnt %% popEvery == 0){
        out <- st$pop()
      }
      popCnt <- popCnt + 1
    }
  },
  setup = {
    nSamples <- 10000
    popCnt <- 1
    popEvery <- 3
  }, times = 20)

microbenchmark::microbenchmark(nalloc = {
  a <- list()
  for(i in seq(nSamples)) {
    a[[i]] <- i
  }
},
pralloc = {
  a <- vector(mode = "list", length = nSamples)
  for(i in seq(nSamples)) {
    a[[i]] <- i
  }
},
setup = {
  nSamples <- 100000
})
