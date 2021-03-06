
# iccInput tests ----------------------------------------------------------

context("iccInput")

test_that("it returns the values supplied", {
  iccin <- iccInput(1:3)

  for(i in 1:3) {
    expect_equal(iccin(), i)
  }
})

test_that("it loops values", {
  iccin <- iccInput(1:3, loop = TRUE)

  expected <- c(1:3, 1)

  for(i in 1:4) {
    expect_equal(iccin(), expected[i])
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
  expect_equal(o$values(), 1:2)
})

test_that("it prints", {
  o <- iccOutputAccumulator(print = TRUE)
  expect_message(o$acc(1), "1")
})


# IntCodeComputer tests ---------------------------------------------------


context("IntCodeComputer")

test_arg_is <- function(expected) {
  function(actual) {
    expect_equal(actual, expected)
  }
}

test_that("day 2 example programs", {
  runIntCodeComputer(list(
    list(
      tape = c(1,9,10,3,2,3,11,0,99,30,40,50),
      done = test_arg_is(c(3500,9,10,70,
                            2,3,11,0,
                            99,
                            30,40,50)))))

  runIntCodeComputer(list(
    list(
      tape = c(1,0,0,0,99),
      done = test_arg_is(c(2,0,0,0,99)))))

  runIntCodeComputer(list(
    list(
      tape = c(2,3,0,3,99),
      done = test_arg_is(c(2,3,0,6,99)))))

  runIntCodeComputer(list(
    list(
      tape = c(2,4,4,5,99,0),
      done = test_arg_is(c(2,4,4,5,99,9801)))))

  runIntCodeComputer(list(
    list(
      tape = c(1,1,1,4,99,5,6,0,99),
      done = test_arg_is(c(30,1,1,4,2,5,6,0,99)))))
})

test_that("param modes", {
  out1 <- runIntCodeComputer(list(
    list(
      tape = c(1002,4,3,4,33),
      done = test_arg_is(c(1002,4,3,4,99)))))

  runIntCodeComputer(list(
    list(
      tape = c(1101, 5, -3, 0, 99),
      done = test_arg_is(c(2, 5, -3, 0, 99))
    )))
})

test_that("negative integers", {
  out1 <- runIntCodeComputer(list(
    list(
      tape = c(1101,100,-1,4,0),
      done = test_arg_is(c(1101,100,-1,4,99)))))
})

test_that("I/O", {
  vIn <- 123

  iccin <- iccInput(vIn)

  iccout <- function(x) {
    expect_equal(x, vIn)
  }

  runIntCodeComputer(list(
    list(
      tape = c(3,0,4,0,99),
      iccin = iccin,
      iccout = iccout)))
})

test_that("comparators", {
  # Using position mode, consider whether the input is equal to 8;
  # output 1 (if it is) or 0 (if it is not).
  prog1 <- c(3,9,8,9,10,9,4,9,99,-1,8)

  runIntCodeComputer(list(list(tape = prog1, iccin = iccInput(7), iccout = test_arg_is(0))))
  runIntCodeComputer(list(list(tape = prog1, iccin = iccInput(8), iccout = test_arg_is(1))))

  # Using position mode, consider whether the input is less than 8;
  # output 1 (if it is) or 0 (if it is not).
  prog2 <- c(3,9,7,9,10,9,4,9,99,-1,8)

  runIntCodeComputer(list(list(tape = prog2, iccin = iccInput(7), iccout = test_arg_is(1))))
  runIntCodeComputer(list(list(tape = prog2, iccin = iccInput(8), iccout = test_arg_is(0))))
  runIntCodeComputer(list(list(tape = prog2, iccin = iccInput(9), iccout = test_arg_is(0))))

  # Using immediate mode, consider whether the input is equal to 8;
  # output 1 (if it is) or 0 (if it is not).
  prog3 <- c(3,3,1108,-1,8,3,4,3,99)

  runIntCodeComputer(list(list(tape = prog3, iccin = iccInput(7), iccout = test_arg_is(0))))
  runIntCodeComputer(list(list(tape = prog3, iccin = iccInput(8), iccout = test_arg_is(1))))

  # Using position mode, consider whether the input is less than 8;
  # output 1 (if it is) or 0 (if it is not).
  prog4 <- c(3,9,7,9,10,9,4,9,99,-1,8)

  runIntCodeComputer(list(list(tape = prog4, iccin = iccInput(7), iccout = test_arg_is(1))))
  runIntCodeComputer(list(list(tape = prog4, iccin = iccInput(8), iccout = test_arg_is(0))))
  runIntCodeComputer(list(list(tape = prog4, iccin = iccInput(9), iccout = test_arg_is(0))))
})

test_that("jumps", {
  # Here are some jump tests that take an input,
  # then output 0 if the input was zero or 1 if the input was non-zero:

  prog1 <- c(3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9) # (using position mode)

  runIntCodeComputer(list(list(tape = prog1, iccin = iccInput(0), iccout = test_arg_is(0))))
  runIntCodeComputer(list(list(tape = prog1, iccin = iccInput(1337), iccout = test_arg_is(1))))

  prog2 <- c(3,3,1105,-1,9,1101,0,0,12,4,12,99,1) # (using immediate mode)

  runIntCodeComputer(list(list(tape = prog2, iccin = iccInput(0), iccout = test_arg_is(0))))
  runIntCodeComputer(list(list(tape = prog2, iccin = iccInput(1337), iccout = test_arg_is(1))))
})

test_that("day 6", {
  prog <- c(3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
            1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
            999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99)

  runIntCodeComputer(list(list(tape = prog, iccin = iccInput(7), iccout = test_arg_is(999))))
  runIntCodeComputer(list(list(tape = prog, iccin = iccInput(8), iccout = test_arg_is(1000))))
  runIntCodeComputer(list(list(tape = prog, iccin = iccInput(9), iccout = test_arg_is(1001))))
})
