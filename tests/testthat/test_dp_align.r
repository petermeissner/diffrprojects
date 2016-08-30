#### diffrproject ==============================================================

context("\ndp_align") # ====================================================



context("diffrproject dp_align") # =================================================

test_that("diffrproject alignment works", {
  expect_true({
    dp <- diffrproject$new()
    dp$text_add(text = "a\nb")
    dp$text_add(text = "")
    dp$text_add(text = "b\n")
    dp$text_link()
    names(dp$text)
    dp$text_align()

    length(dp$alignment)==2 &
      all(dim(as.data.frame(dp$alignment)) == c(3, 10))
  })
  expect_true({
    dp <- diffrproject$new()
    dp$text_add("")
    dp$text_add("")
    dp$text_link()
    dp$text_align()

    length(dp$alignment)==1 &
    all(dim(as.data.frame(dp$alignment)) == c(0, 10))
  })
  expect_true({
    dp <- diffrproject$new()
    dp$text_add("a\n")
    dp$text_add("")
    dp$text_link()
    dp$text_align()

    length(dp$alignment)==1 &
      all(dim(as.data.frame(dp$alignment)) == c(1, 10))
  })
  expect_true({
    dp <- diffrproject$new()
    dp$text_add("")
    dp$text_add("b\n")
    dp$text_link()
    dp$text_align()

    length(dp$alignment)==1 &
      all(dim(as.data.frame(dp$alignment)) == c(1, 10))
  })
})


