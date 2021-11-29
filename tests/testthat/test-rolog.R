test_that("consult works with default arguments", 
{
  expect_true(consult())
})

test_that("once works with default arguments", 
{
  l = list(X=as.symbol("a"))
  attr(l, "query") = "member(X, [a, b, 3, 4.0, true, Y])"

  expect_equal(once(), l)
})

test_that("findall works with default arguments", 
{
  l1 = list(X=as.symbol("a"))
  l2 = list(X="b")
  l3 = list(X=3L)
  l4 = list(X=4)
  l5 = list(X=TRUE)
  l6 = list(Y=expression(X))
  l = list(l1, l2, l3, l4, l5, l6)
  attr(l, "query") = "member(X, [a, b, 3, 4.0, true, Y])"
  
  expect_equal(findall(), l)
})
