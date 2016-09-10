#### diffrproject ==============================================================

context("\ndp_align") # ====================================================



context("diffrproject text_align()") # =================================================

test_that("diffrproject text_align works", {
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



context("diffrproject alignment_delete()") # =================================================

test_that("diffrproject alignment_delete works", {
  expect_error({
    dp <- diffrproject$new()
    dp$alignment_delete(1)
  })
  expect_warning({
    dp <- diffrproject$new()
    dp$alignment_delete(1,1,1)
  })
  expect_error({
    dp <- diffrproject$new()
    dp$options$warning <- FALSE
    dp$alignment_delete(1,1)
  }, NA)
  expect_true({
    dp <- diffrproject$new()
    dp$options$warning <- FALSE
    any( class(dp$alignment_delete(1,1)) %in% "diffrproject" )
  })
  expect_true({
    dp <- diffrproject$new()
    dp$text_add("")
    dp$text_add("b\n")
    dp$text_link()
    dp$text_align()
    dp$alignment
    dp$alignment_delete(1,1)
    dim(dp$alignment[[1]])[1]==0
  })
  expect_true({
    dp <- diffrproject$new()
    dp$text_add("kdsajhfsadlkfhdsaf")
    dp$text_add("asdfklsadfm.samdfasf")
    dp$text_add("asdfklseadasdfwqwwer.samdfagsf")
    dp$text_link()
    dp$text_align(tokenizer = function(x){text_tokenize(x, "")})
    dp$alignment
    dp$alignment_delete(1,1)
    dp$alignment_delete(2,1)
    sum(
      vapply(
        dp$alignment,
        function(x){sum(x$alignment_i %in% 1)},
        1
      )
    )==0
  })
  expect_true({
    dp <- diffrproject$new()
    dp$text_add("kdsajhfsadlkfhdsaf")
    dp$text_add("asdfklsadfm.samdfasf")
    dp$text_add("asdfklseadasdfwqwwer.samdfagsf")
    dp$text_link()
    dp$text_align(tokenizer = function(x){text_tokenize(x, "")})
    dp$alignment
    dp$alignment_delete(1, type = "insertion")
    dp$alignment_delete(2,type="insertion")
    sum(
      vapply(
        dp$alignment,
        function(x){sum(x$type %in% "insertion")},
        1
      )
    )==0
  })
})









context("diffrproject alignment_data_set()") # =================================================

test_that("diffrproject alignment_data_set()", {
  expect_error({
    dp <-
      diffrproject$
      new()$
      text_add(list("abcd", "bcdaa", "ccdabbcd"))$
      text_link()$
      debug()$
      text_align(tokenizer=function(x){text_tokenize(x,"")})
    dp$alignment_data_set()
  })

  expect_true({
    dp <-
      diffrproject$
      new()$
      text_add(list("abcd", "bcdaa", "ccdabbcd"))$
      text_link()$
      debug()$
      text_align(tokenizer=function(x){text_tokenize(x,"")})
    dp$alignment_data_set(link=1, alignment_i = 1, x="test_var", val=3)
    dp$alignment_data_set(link=1, alignment_i = 1:4, x="y", val=2)
    dp$alignment_data_set(link=2, alignment_i = 1:9, x="y", val=2)
    df <- as.data.frame(dp$alignment_data)
    all(
      c("alignment_i", "hl", "link") %in% names(df)
    )
  })

  expect_true({
    dp <-
      diffrproject$
      new()$
      text_add(list("abcd", "bcdaa", "ccdabbcd"))$
      text_link()$
      debug()$
      text_align(tokenizer=function(x){text_tokenize(x,"")})
    dp$alignment_data_set(link=1, alignment_i = 1, x="test_var", val=3)
    dp$alignment_data_set(link=1, alignment_i = 1:4, x="y", val=2)
    dp$alignment_data_set(link=2, alignment_i = 1:9, x="y", val=2)

    dp$alignment_delete(1:2,2:9)
    df <- as.data.frame(dp$alignment_data)
    all(df$alignment_i==1)
  })

  expect_true({
    dp <-
      diffrproject$
      new()$
      text_add(list("abcd", "bcdaa", "ccdabbcd"))$
      text_link()$
      debug()$
      text_align(tokenizer=function(x){text_tokenize(x,"")})

    dp$alignment

    dp$alignment_data_set(link=1, alignment_i = 1:3, x="test_var", val=3)
    dp$alignment_data_set(link=1, alignment_i = 1, x="test_var", val=4, hl=1)
    dp$alignment_data_set(link=1, alignment_i = 2, x="test_var", val=4, hl=-1)

    df <- as.data.frame(dp$alignment_data)
    all(df$val==c(4,3,3))
  })
})







context("diffrproject alignment_data_code()") # =================================================

test_that("diffrproject alignment_data_code()", {
  expect_error({
    dp <-
      diffrproject$
      new()$
      text_add(list("abcd", "bcdaa", "ccdabbcd"))$
      text_link()$
      debug()$
      text_align(tokenizer=function(x){text_tokenize(x,"")})
    dp$alignment_data_code()
  })

  expect_true({
    dp <-
      diffrproject$
      new()$
      text_add(list("abcd", "bcdaa", "ccdabbcd"))$
      text_link()$
      debug()$
      text_align(tokenizer=function(x){text_tokenize(x,"")})
    dp$alignment_code(link=1, alignment_i = 1, x="test_var", val=3)
    dp$alignment_code(link=1, alignment_i = 1:4, x="y", val=2)
    dp$alignment_code(link=2, alignment_i = 1:9, x="y", val=2)
    df <- as.data.frame(dp$alignment_data)
    all(
      c("alignment_i", "hl", "link") %in% names(df)
    )
  })

  expect_true({
    dp <-
      diffrproject$
      new()$
      text_add(list("abcd", "bcdaa", "ccdabbcd"))$
      text_link()$
      debug()$
      text_align(tokenizer=function(x){text_tokenize(x,"")})
    dp$alignment_code(link=1, x="pattern", pattern="a")
    df <- dp$alignment_data_full(1)
    all(
      df$alignment_i == c(1,5)
    )
  })

  expect_true({
    dp <-
      diffrproject$
      new()$
      text_add(list("abcd", "bcdaa", "ccdabbcd"))$
      text_link()$
      debug()$
      text_align(tokenizer=function(x){text_tokenize(x,"")})
    dp$alignment_code(link=1, x="pattern", pattern1="a")
    df <- dp$alignment_data_full(1)
    df$alignment_i==1
  })

  expect_true({
    dp <-
      diffrproject$
      new()$
      text_add(list("abcd", "bcdaa", "ccdabbcd"))$
      text_link()$
      debug()$
      text_align(tokenizer=function(x){text_tokenize(x,"")})
    dp$alignment_code(link=1, x="pattern", pattern2="a")
    df <- dp$alignment_data_full(1)
    all(
      df$alignment_i==c(1,5)
    )
  })

})



























