test_that("once works with default arguments", 
  {
    l = list(X=1)
    attr(l, 'query') = 'member(X, [1.0, 2.0, 3.0])'
    
    expect_equal(once(), l)
  })
