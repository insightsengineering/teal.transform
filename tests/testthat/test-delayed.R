test_that("delay works", {
  out <- list(names = character(), select = character())
  dout <- delay(out)
  expect_s3_class(dout, "delayed")
  expect_true(is.delayed(dout))
  expect_equal(resolved(dout), out)
})

test_that("is.delayed works", {
  d <- datasets("a")
  v <- variables("b")
  da <- datasets("a", "a")
  expect_true(is.delayed(d))
  expect_true(is.delayed(da))
  expect_true(is.delayed(v))
  expect_true(is.delayed(variables("b", "b")))
  expect_true(is.delayed(d & v))
})
