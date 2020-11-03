test_that("question mark works", {
  # outside of function def
  expect_equal(? x <- 1, 1)
  expect_equal(? (x) <- 1, 1)
  expect_equal(Double() ? x, NULL)
  expect_equal(Double() ? x <- 1, 1)
  expect_equal(Double() ? (x) <- 1, 1)

  # regular help
  expect_error(?mean, NA)

  # function def
  expect_error(
    regexp=NA,
    fun <- Double() ? function(x = ?~ Symbol(), y = ?+ Double(), z = 1 ? Double()) {
      ?mean
      ? foo <- 1
      ? (foo) <- 1
      Double() ? bar <- 1
      Double() ? (bar) <- 1
      Double() ? baz
      baz <- 1
      if(TRUE) return(foo)
      foo
    })
  expect_error(
    regexp=NA,
    ? function(...= ? Double()) {}
    )
  expect_error(
    ? function(...= ?+ Double()) {}
  )
  expect_error(
    regexp=NA,
    ? function(...= ?~ Double()) {}
  )
  expect_error(
    regexp=NA,
    ? function(...= ? Dots(2)) {}
  )
  expect_error(
    regexp=NA,
    ? function(...= ?~ Dots(2)) {}
  )
  expect_error(
    regexp=NA,
    Function() ? fun1 <- Double() ? function() {1}
  )
})

# detach("package:typed");covr::report()

# `?` <- typed::`?`

