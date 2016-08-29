#### diffrproject ==============================================================

context("\ndp_align") # ====================================================



context("diffrproject dp_align") # =================================================

test_that("diffrproject alignment workds", {
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
})


