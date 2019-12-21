library(datastructures)

nKeys <- 10000
nToGet <- nKeys%/%10
keyLength <- 8 # Password must be at least 8 characters long ;P
chars <- sample(c(letters, LETTERS), nKeys*keyLength, TRUE)
dim(chars) <- c(nKeys, keyLength)
keys <- apply(chars, 1, paste, collapse = "")

# todo: benchmark this too
listwise <- list()
hashwise <- hashmap()

microbenchmark::microbenchmark(hashmap = {
  hashwise <- hashmap()
  for(k in keys) {
    hashwise[k] <- TRUE
  }
},
listwise = {
  listwise <- list()
  for(k in keys) {
    listwise[k] <- TRUE
  }
},
times = 10)



keysToGet <- sample(keys, nToGet)
microbenchmark::microbenchmark(hashmap = {
  for(k in keysToGet) {
    out <- hashwise[k]
  }
},
listwise = {
  for(k in keysToGet) {
    out <- listwise[k]
  }
},
setup = {
  keysToGet <- sample(keys, nToGet)
})
