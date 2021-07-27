test_that("consult works with default arguments", 
{
  expect_true(consult())
})

test_that("once works with default arguments", 
{
  l = list(X=1)
  attr(l, 'query') = 'member(X, [1.0, 2.0, 3.0])'

  expect_equal(once(), l)
})

test_that("findall works with default arguments", 
{
  l1 = list(X=1)
  l2 = list(X=2)
  l3 = list(X=3)
  l = list(l1, l2, l3)
  attr(l, 'query') = 'member(X, [1.0, 2.0, 3.0])'
  
  expect_equal(findall(), l)
})
