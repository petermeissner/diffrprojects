#### tools =====================================================================
context("\ntools") # ===========================================================

context("tools modus")

test_that("easy examples work properly", {
  expect_true(  modus(1)==1 )
  expect_true(  modus(2)==2 )
  expect_true(  modus(1:10, warn = FALSE) == 1 )
  expect_true(  modus(10:1, warn = FALSE) ==10 )
  expect_warning( modus(c(1,1,2,2)) )
}
)



context("tools which_token()")

test_that("easy examples work properly", {
  expect_true(  which_token( x =        1, y1 =        1, y2 =           1 )  ==        1 )
  expect_true(  which_token( x =        2, y1 =   c(2,1), y2 =      c(2,1) )  ==        1 )
  expect_true(  which_token( x =        1, y1 =   c(2,1), y2 =      c(2,1) )  ==        2 )
  expect_equal( which_token( x =      1:2, y1 =   c(2,1), y2 =      c(2,1) ),      c(2,1) )
  expect_equal( which_token( x = c(7,2,4), y1 = c(1,3,7), y2 = c(2,6,2000) ),    c(3,1,2) )
  expect_equal( which_token( x =      1:4, y1 = c(1,3,7), y2 = c(2,6,2000) ), c(1,1,2,2))
  expect_true(  is.na(which_token( x =     2001, y1 = c(1,3,7), y2 = c(2,6,2000) ))       )
}
)



