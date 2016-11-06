#### diffr =====================================================================
context("\ndiff_align") # ===========================================================

context("diff_align")

test_that("diff_align works on basic level", {
  expect_error({    diff_align()         })
  expect_error({    diff_align("")       })
  expect_error({    diff_align("","")    },NA)
  expect_error({    diff_align("1","2")  },NA)
  expect_error({    diff_align("","2")   },NA)
  expect_error({    diff_align("1","")   },NA)
  expect_true({
    all(
      names(
        diff_align("","")
      ) ==
      names(
        diff_align("1","2")
      )
    )
  })
  expect_true({
    all(c(letters[1:3]) %in% diff_align("a\nb\nc","")$token_1)
  })
  expect_true({
    all(c(letters[1:3]) %in% diff_align("", "a\nb\nc")$token_2)
  })
  expect_true({
    d <- diff_align("a\nb\nc", "a\nb\nc")
    all(c(letters[1:3]) %in% d$token_1) &
    all(c(letters[1:3]) %in% d$token_2)
  })
  expect_true({
    d <- diff_align("d\ne\nf", "a\nb\nc")
    all(c(letters[4:6]) %in% d$token_1) &
      all(c(letters[1:3]) %in% d$token_2)
  })
  expect_true({
    d <- diff_align("a\nc\ne", "a\nb\nc")
    all( letters[c(1,3,5)] %in% d$token_1) &
    all( letters[1:3]      %in% d$token_2)
  })
})

test_that("diff_align works on level 2", {
  expect_true({
    d <- diff_align("a\nc\ne", "a\nb\nc", maxDist = Inf)
    all( "change" %in% d$type)
  })
  expect_true({
    d <- diff_align("a\nc\ne", "a\nb\nc", maxDist = 0)
    all( !("change" %in% d$type) )
  })
})

test_that("diff_align works with ignore option", {
  expect_true({
    d <-
      diff_align(
        stringb::text_c(letters[1:10], "\n"),
        stringb::text_c(letters[7:12], "\n"),
        ignore = function(x){x[1,]}
      )
    all(sort(table(d$type)) == c(1,1,14))
  })
})

test_that("diff_align works with clean option", {
  expect_true({
    d <-
      diff_align(
        text1 = stringb::text_c(stringb::text_c("a",letters[1:10]), "\n"),
        text2 = stringb::text_c(stringb::text_c("b",letters[7:12]), "\n"),
        clean = function(x){text_replace(x,"\\w","")}
      )
    all(  grepl("^a",d$token_1) | is.na(d$token_1) ) &
    all(  grepl("^b",d$token_2) | is.na(d$token_2) )
  })
})













