test_that("#47", {
  f <- typed::Integer() ? function() {
    typed::Double() ? foo <- local({
      return(as.double(1))
    })
    return(as.integer(1))
  }

  expect_error(f(), NA)
})

