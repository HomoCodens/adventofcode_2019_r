
# iccInput tests ----------------------------------------------------------

context("iccInput")

expecter <- function(i) {
  function(x) {
    expect_equal(x, i)
  }
}

test_that("it returns the values supplied", {
  iccin <- iccInput(1:3)

  expecters <- lapply(1:3, expecter)

  for(i in 1:3) {
    iccin(expecters[[i]])
  }
})

test_that("it loops values", {
  iccin <- iccInput(1:3, loop = TRUE)

  expecters <- lapply(c(1:3, 1), expecter)

  for(i in 1:4) {
    iccin(expecters[[i]])
  }
})

test_that("it throws if no loop", {
  iccin <- iccInput(1:3)
  expect_error(sapply(1:4, function(x)iccin()))
})



# iccOutputAccumulator tests ----------------------------------------------


context("iccOutputAccumulator")

test_that("it does work", {
  o <- iccOutputAccumulator()
  o$acc(1)
  o$acc(2)
  expect_equal(getOutput(o), 1:2)
})

test_that("it prints", {
  o <- iccOutputAccumulator(print = TRUE)
  expect_message(o$acc(1), "1")
})


# IntCodeComputer tests ---------------------------------------------------


context("IntCodeComputer")

test_output_is <- function(expected) {
  function(actual) {
    expect_equal(actual, expected)
  }
}

test_that("day 2 example programs", {
  out1 <- runIntCodeComputer(c(1,9,10,3,2,3,11,0,99,30,40,50))
  expect_equal(out1, c(3500,9,10,70,
                       2,3,11,0,
                       99,
                       30,40,50))

  out2 <- runIntCodeComputer(c(1,0,0,0,99))
  expect_equal(out2, c(2,0,0,0,99))

  out3 <- runIntCodeComputer(c(2,3,0,3,99))
  expect_equal(out3, c(2,3,0,6,99))

  out4 <- runIntCodeComputer(c(2,4,4,5,99,0))
  expect_equal(out4, c(2,4,4,5,99,9801))

  out5 <- runIntCodeComputer(c(1,1,1,4,99,5,6,0,99))
  expect_equal(out5, c(30,1,1,4,2,5,6,0,99))
})

test_that("param modes", {
  out1 <- runIntCodeComputer(c(1002,4,3,4,33))
  expect_equal(out1, c(1002,4,3,4,99))
})

test_that("negative integers", {
  out1 <- runIntCodeComputer(c(1101,100,-1,4,0))
  expect_equal(out1, c(1101, 100, -1, 4, 99))
})

test_that("I/O", {
  vIn <- 123

  iccin <- iccInput(vIn)

  iccout <- function(x) {
    expect_equal(x, vIn)
  }

  runIntCodeComputer(c(3,0,4,0,99), iccin = iccin, iccout = iccout)
})

test_that("comparators", {
  # Using position mode, consider whether the input is equal to 8;
  # output 1 (if it is) or 0 (if it is not).
  prog1 <- c(3,9,8,9,10,9,4,9,99,-1,8)

  runIntCodeComputer(prog1, iccin = iccInput(7), iccout = test_output_is(0))
  runIntCodeComputer(prog1, iccin = iccInput(8), iccout = test_output_is(1))

  # Using position mode, consider whether the input is less than 8;
  # output 1 (if it is) or 0 (if it is not).
  prog2 <- c(3,9,7,9,10,9,4,9,99,-1,8)

  runIntCodeComputer(prog2, iccin = iccInput(7), iccout = test_output_is(1))
  runIntCodeComputer(prog2, iccin = iccInput(8), iccout = test_output_is(0))
  runIntCodeComputer(prog2, iccin = iccInput(9), iccout = test_output_is(0))

  # Using immediate mode, consider whether the input is equal to 8;
  # output 1 (if it is) or 0 (if it is not).
  prog3 <- c(3,3,1108,-1,8,3,4,3,99)

  runIntCodeComputer(prog3, iccin = iccInput(7), iccout = test_output_is(0))
  runIntCodeComputer(prog3, iccin = iccInput(8), iccout = test_output_is(1))

  # Using position mode, consider whether the input is less than 8;
  # output 1 (if it is) or 0 (if it is not).
  prog4 <- c(3,9,7,9,10,9,4,9,99,-1,8)

  runIntCodeComputer(prog4, iccin = iccInput(7), iccout = test_output_is(1))
  runIntCodeComputer(prog4, iccin = iccInput(8), iccout = test_output_is(0))
  runIntCodeComputer(prog4, iccin = iccInput(9), iccout = test_output_is(0))
})

test_that("jumps", {
  # Here are some jump tests that take an input,
  # then output 0 if the input was zero or 1 if the input was non-zero:

  prog1 <- c(3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9) # (using position mode)

  runIntCodeComputer(prog1, iccin = iccInput(0), iccout = test_output_is(0))
  runIntCodeComputer(prog1, iccin = iccInput(1337), iccout = test_output_is(1))

  prog2 <- c(3,3,1105,-1,9,1101,0,0,12,4,12,99,1) # (using immediate mode)

  runIntCodeComputer(prog2, iccin = iccInput(0), iccout = test_output_is(0))
  runIntCodeComputer(prog2, iccin = iccInput(1337), iccout = test_output_is(1))
})

test_that("day 6", {
  prog <- c(3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
            1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
            999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99)

  runIntCodeComputer(prog, iccin = iccInput(7), iccout = test_output_is(999))
  runIntCodeComputer(prog, iccin = iccInput(8), iccout = test_output_is(1000))
  runIntCodeComputer(prog, iccin = iccInput(9), iccout = test_output_is(1001))
})
