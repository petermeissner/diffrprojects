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


context("tools as.data.frame") # ===============================================

test_that("as.data.frame methods work", {
  dp <- diffrproject$new()
  dp$text_add(text = "a\nb")
  dp$text_add(text = "")
  dp$text_add(text = "b\n")
  dp$text_link()
  names(dp$text)
  dp$text_align()

  expect_true({
    is.data.frame(as.data.frame(dp$alignment)) &
    all(dim(as.data.frame(dp$alignment)) == c(3, 10))
  })

  expect_true({
    is.data.frame(as.data.frame(dp$link)) &
      all(dim(as.data.frame(dp$alignment)) == c(3, 10))
  })
})


