
test_that("`Any` works", {
  expect_equal(Any()(1), 1)

  expect_equal(Any(1)(1), 1)
  expect_error(Any(2)(1))
})

test_that("`Logical` works", {
  expect_equal(Logical()(TRUE), TRUE)
  expect_error(Logical()(1))

  expect_equal(Logical(1)(TRUE), TRUE)
  expect_error(Logical(2)(TRUE))

  expect_equal(Logical(null_ok = TRUE)(NULL), NULL)
  expect_error(Logical(null_ok = FALSE)(NULL))
})

test_that("`Integer` works", {
  expect_equal(Integer()(1L), 1L)
  expect_error(Integer()(1))

  expect_equal(Integer(1)(1L), 1L)
  expect_error(Integer(2)(1L))

  expect_equal(Integer(null_ok = TRUE)(NULL), NULL)
  expect_error(Integer(null_ok = FALSE)(NULL))
})

test_that("`Double` works", {
  expect_equal(Double()(1), 1)
  expect_error(Double()(1L))

  expect_equal(Double(1)(1), 1)
  expect_error(Double(2)(1))

  expect_equal(Double(null_ok = TRUE)(NULL), NULL)
  expect_error(Double(null_ok = FALSE)(NULL))
})

test_that("`Character` works", {
  expect_equal(Character()("a"), "a")
  expect_error(Character()(1))

  expect_equal(Character(1)("a"), "a")
  expect_error(Character(2)("a"))

  expect_equal(Character(null_ok = TRUE)(NULL), NULL)
  expect_error(Character(null_ok = FALSE)(NULL))
})

test_that("`Raw` works", {
  expect_equal(Raw()(as.raw("1")), as.raw("1"))
  expect_error(Raw()(1))

  expect_equal(Raw(1)(as.raw("1")), as.raw("1"))
  expect_error(Raw(2)(as.raw("1")))

  expect_equal(Raw(null_ok = TRUE)(NULL), NULL)
  expect_error(Raw(null_ok = FALSE)(NULL))
})

test_that("`List` works", {
  x <- list(a=1, b=2)
  y <- list(1, 2)
  df <- as.data.frame(x)
  expect_equal(List()(x), x)
  expect_error(List()(1))

  expect_equal(List(2)(x), x)
  expect_error(List(1)(x))

  expect_equal(List(each=Double())(x), x)
  expect_error(List(each=Integer())(x))

  expect_equal(List(each=Double())(y), y)
  expect_error(List(each=Integer())(y))

  expect_equal(List()(df), df)
  expect_error(List(data_frame_ok = FALSE)(df))

  expect_equal(List(null_ok = TRUE)(NULL), NULL)
  expect_error(List(null_ok = FALSE)(NULL))
})

test_that("`Null` works", {
  expect_equal(Null()(NULL), NULL)
  expect_error(Null()(1))
})

test_that("`Closure` works", {
  expect_equal(Closure()(mean), mean)
  expect_error(Closure()(1))

  expect_equal(Closure(null_ok = TRUE)(NULL), NULL)
  expect_error(Closure(null_ok = FALSE)(NULL))
})

test_that("`Special` works", {
  expect_equal(Special()(`<-`), `<-`)
  expect_error(Special()(1))

  expect_equal(Special(null_ok = TRUE)(NULL), NULL)
  expect_error(Special(null_ok = FALSE)(NULL))
})

test_that("`Builtin` works", {
  expect_equal(Builtin()(max), max)
  expect_error(Builtin()(1))

  expect_equal(Builtin(null_ok = TRUE)(NULL), NULL)
  expect_error(Builtin(null_ok = FALSE)(NULL))
})

test_that("`Environment` works", {
  expect_equal(Environment()(.GlobalEnv), .GlobalEnv)
  expect_error(Environment()(1))

  expect_equal(Environment(null_ok = TRUE)(NULL), NULL)
  expect_error(Environment(null_ok = FALSE)(NULL))
})

test_that("`Symbol` works", {
  expect_equal(Symbol()(quote(a)), quote(a))
  expect_error(Symbol()(1))

  expect_equal(Symbol(null_ok = TRUE)(NULL), NULL)
  expect_error(Symbol(null_ok = FALSE)(NULL))
})

test_that("`Pairlist` works", {
  x <- pairlist(a=1, b=2)
  y <- pairlist(1, 2)
  expect_equal(Pairlist()(x), x)
  expect_error(Pairlist()(1))

  expect_equal(Pairlist(2)(x), x)
  expect_error(Pairlist(1)(x))

  expect_equal(Pairlist(each=Double())(x), x)
  expect_error(Pairlist(each=Integer())(x))

  expect_equal(Pairlist(each=Double())(y), y)
  expect_error(Pairlist(each=Integer())(y))

  expect_equal(Pairlist(null_ok = TRUE)(NULL), NULL)
  expect_error(Pairlist(null_ok = FALSE)(NULL))
})

test_that("`Language` works", {
  expect_equal(Language()(quote(a + b)), quote(a + b))
  expect_error(Language()(1))

  expect_equal(Language(null_ok = TRUE)(NULL), NULL)
  expect_error(Language(null_ok = FALSE)(NULL))
})

test_that("`Expression` works", {
  expect_equal(Expression()(expression(a)), expression(a))
  expect_error(Expression()(1))

  expect_equal(Expression(1)(expression(a)), expression(a))
  expect_error(Expression(2)(expression(a)))

  expect_equal(Expression(null_ok = TRUE)(NULL), NULL)
  expect_error(Expression(null_ok = FALSE)(NULL))
})

test_that("`Function` works", {
  expect_equal(Function()(mean), mean)
  expect_error(Function()(1))

  expect_equal(Function(null_ok = TRUE)(NULL), NULL)
  expect_error(Function(null_ok = FALSE)(NULL))
})

test_that("`Factor` works", {
  expect_equal(Factor()(factor("a")), factor("a"))
  expect_error(Factor()(1))

  expect_equal(Factor(1)(factor("a")), factor("a"))
  expect_error(Factor(2)(factor("a")))

  expect_equal(Factor(levels = "a")(factor("a")), factor("a"))
  expect_error(Factor(levels = "a")(factor("b")))

  expect_equal(Factor(null_ok = TRUE)(NULL), NULL)
  expect_error(Factor(null_ok = FALSE)(NULL))
})

test_that("`Matrix` works", {
  expect_equal(Matrix()(matrix(1)), matrix(1))
  expect_error(Matrix()(1))

  expect_equal(Matrix(1)(matrix(1)), matrix(1))
  expect_error(Matrix(2)(matrix(1)))

  expect_equal(Matrix(,1)(matrix(1)), matrix(1))
  expect_error(Matrix(,2)(matrix(1)))

  expect_equal(Matrix(null_ok = TRUE)(NULL), NULL)
  expect_error(Matrix(null_ok = FALSE)(NULL))
})

test_that("`Array` works", {
  expect_equal(Array()(matrix(1)), matrix(1))
  expect_error(Array()(1))

  expect_equal(Array(c(1,1))(matrix(1)), matrix(1))
  expect_error(Array(c(1,2))(matrix(1)))

  expect_equal(Array(null_ok = TRUE)(NULL), NULL)
  expect_error(Array(null_ok = FALSE)(NULL))
})

test_that("`Data.frame` works", {
  expect_equal(Data.frame()(data.frame(a=1, b=2L)), data.frame(a=1, b=2L))
  expect_error(Data.frame()(1))

  expect_equal(Data.frame(1)(data.frame(a=1, b=2L)), data.frame(a=1, b=2L))
  expect_error(Data.frame(2)(data.frame(a=1, b=2L)))

  expect_equal(Data.frame(,2)(data.frame(a=1, b=2L)), data.frame(a=1, b=2L))
  expect_error(Data.frame(,1)(data.frame(a=1, b=2L)))

  expect_equal(Data.frame(each = Double())(data.frame(a=1, b=2)), data.frame(a=1, b=2))
  expect_error(Data.frame(each = Double())(data.frame(a=1, b=2L)))

  expect_equal(Data.frame(null_ok = TRUE)(NULL), NULL)
  expect_error(Data.frame(null_ok = FALSE)(NULL))
})

test_that("`Date` works", {
  x <- Sys.Date()
  expect_equal(Date()(x), x)
  expect_error(Date()(1))

  expect_equal(Date(1)(x), x)
  expect_error(Date(2)(x))

  expect_equal(Date(null_ok = TRUE)(NULL), NULL)
  expect_error(Date(null_ok = FALSE)(NULL))
})

test_that("`Time` works", {
  x <- Sys.time()
  expect_equal(Time()(x), x)
  expect_error(Time()(1))

  expect_equal(Time(1)(x), x)
  expect_error(Time(2)(x))

  expect_equal(Time(null_ok = TRUE)(NULL), NULL)
  expect_error(Time(null_ok = FALSE)(NULL))
})

test_that("`Dots` works", {
  x <- list(a=1, b=2)
  y <- list(1, 2)

  expect_equal(Dots(2)(x), x)
  expect_error(Dots(1)(x))

  expect_equal(Dots(each=Double())(x), x)
  expect_error(Dots(each=Integer())(x))

  expect_equal(Dots(each=Double())(y), y)
  expect_error(Dots(each=Integer())(y))
})
