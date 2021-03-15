test_that("check_output works", {
  expect_equal(check_output(2, Double()), 2)
  expect_error(check_output(2L, Double()))
})

test_that("check_arg works", {
  x <- 1
  y <- 2
  expect_equal(check_arg(x, Double()), NULL)
  expect_equal(check_arg(y, Double(), .bind = TRUE), NULL)
  expect_equal(y <- 3, 3)
  expect_error(y <- 3L)
  # commenting because according to rhub doesn't work on
  # Windows Server 2008 R2 SP1, R-devel, 32/64 bit
  # expect_error(check_arg(x, Integer()))
  # expect_error(check_arg(y, Integer(), .bind = TRUE))
})

test_that("declare works", {
  expect_equal(declare("x", value = 1), 1)
  expect_equal(declare("x", value = data.frame(a=1)), data.frame(a=1))
  sys_time <- Sys.time()
  expect_equal(declare("x", value = sys_time), sys_time)
  foobar_obj <- structure(1, class = c("foo", "bar"))
  expect_equal(declare("x", value = foobar_obj), foobar_obj)
  #expect_error(? x <- stop("!!!"))

  expect_equal(declare("x", Double(), value = 1), 1)
  expect_equal(declare("x", Double, value = 1), 1)
  expect_equal(x <- 2, 2)
  expect_error(x <- 2L)
  expect_error(declare("x", Double(), value = 1L))

  expect_equal(declare("x", Double(), value = 1, const = TRUE), 1)
  expect_equal(x, 1)
  expect_error(x <- 2)
  expect_error(declare("x", Double(), value = 1L, const = TRUE))

  expect_equal(Double() ? x <- 1, 1)
  expect_error(Double() ? x <- 1L)
})
