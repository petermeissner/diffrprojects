#### tools =====================================================================
context("\ntools") # ===========================================================

context("tools modus")

context("tools modus")

test_that("easy examples work properly", {
  expect_true(  modus(1)==1 )
  expect_true(  modus(2)==2 )
  expect_true(  modus(1:10, warn = FALSE) == 1 )
  expect_true(  modus(10:1, warn = FALSE) ==10 )
  expect_warning( modus(c(1,1,2,2)) )
  expect_true(  all(diffrprojects:::modus(c(1,1,2,3,2), multimodal=TRUE)==c(1,2)) )
  expect_warning(  all(diffrprojects:::modus(c(1,1,2,3,2), multimodal=TRUE)==c(1,2)),NA )
}
)



context("tools dp_text_base_data ")

test_that("dp_text_base_data works properly", {
  dp <- diffrproject$new()
  expect_error({
    dp_text_base_data(dp)
  },NA)
  expect_true({
    dp$text_add(text="abcd")
    dim(dp_text_base_data(dp))[1]==1
  },NA)
  expect_true({
    dp$text_add(text="abcd")
    dim(dp_text_base_data(dp))[1]==2
  },NA)
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

  expect_error({
    dp <-
      diffrproject$
      new()$
      text_add(list("abcd", "bcdaa", "ccdabbcd"))$
      text_link()$
      debug()$
      text_align(tokenizer=function(x){text_tokenize(x,"")})

    dp$alignment_data_set(link=1, alignment_i = 1, x="test_var", val=3)

    df <- as.data.frame(dp$alignment_data)
  }, NA)
})


context("tools is_minimum") # ===================================================

test_that("", {
  expect_true({
    all(
      diffrprojects:::is_minimum(1:2) == c(TRUE, FALSE)
    )
  })
  expect_true({
    all(
      diffrprojects:::is_minimum(c(1,1,2)) == c(TRUE, TRUE, FALSE)
    )
  })
  expect_true({
    all(
      diffrprojects:::is_minimum(c(10000,10001,10000)) == c(TRUE, FALSE, TRUE)
    )
  })
  expect_true({
    all(
      is.na(diffrprojects:::is_minimum(c(10000,NA,10000)))
    )
  })
  expect_true({
    all(
        diffrprojects:::is_minimum(c(10000,134214234,10000), unique = TRUE)==c(TRUE,FALSE,FALSE)
    )
  })
})


context("tools is_unique ") # ===================================================

test_that("", {
  expect_true({
    all(
      diffrprojects:::is_unique(1:10) == rep(TRUE, 10)
    )
  })
  expect_true({
    all(
      diffrprojects:::is_unique(c(1,2,1)) == c(FALSE, TRUE, FALSE)
    )
  })
  expect_true({
      identical(
        diffrprojects:::is_unique(c(1,NA,1, 2)),
        c(FALSE, NA, FALSE, TRUE)
      )
  })
})


context("tools is_duplicate") # ===================================================

test_that("", {
  expect_true({
    all(
      diffrprojects:::is_duplicate(c(1,1,1)) == c(TRUE,TRUE,TRUE)
    )
  })
})


context("tools get_list_item ") # ===================================================

test_that("", {
  expect_true({
    all(
     is.na(
       diffrprojects:::get_list_item(list(a=1:2, 3), "a")
     )
    )
  })
  expect_true({
    sum(
      is.na(
        diffrprojects:::get_list_item(list(a=c(a=1,b=2), 3), "a")
      )
    )==1
  })
  expect_true({
    identical(
      diffrprojects:::get_list_item(list(list(a=1, b=2), data.frame(a=1,b=2), list(c=3)), "a"),
      c(1,1,NA)
    )
  })
  expect_true({
    identical(
      diffrprojects:::get_list_item(
        list(list(a=1, b=2), data.frame(a=1,b=2), list(c=3)), "a" , unlist=FALSE
      ),
      list(1,1,NA)
    )
  })
})




context("tools is_between")

test_that("is_between works", {
  expect_true({
    !diffrprojects:::is_between(1,2,3)
  })
  expect_true({
    diffrprojects:::is_between(2,2,3)
  })
  expect_true({
    diffrprojects:::is_between(3,2,3)
  })
  expect_true({
    diffrprojects:::is_between(-1,-2,3)
  })
  expect_true({
    is.na(diffrprojects:::is_between(NA,-2,3))
  })
})

context("tools rbind_fill")

test_that("rbind_fill works", {
  expect_true({
    df1 <- data.frame(x=1)
    df2 <- data.frame(x=1)
    all(
      dim(diffrprojects:::rbind_fill(df1, df2))==c(2,1)
    )
  })
  expect_true({
    df1 <- data.frame(x=1)
    df2 <- data.frame(y=1)
    all(
      dim(diffrprojects:::rbind_fill(df1, df2))==c(2,2)
    )
  })
  expect_true({
    df1 <- data.frame()
    df2 <- data.frame(y=1)
    all(
      dim(
        diffrprojects:::rbind_fill(df1, df2)
      )==c(1,1)
    )
  })
  expect_true({
    df1 <- data.frame(x=1)
    df2 <- data.frame()
    all(
      dim(
        diffrprojects:::rbind_fill(df1, df2)
      )==c(1,1)
    )
  })
  expect_true({
    df1 <- data.frame()
    df2 <- data.frame()
    all(
      dim(
        diffrprojects:::rbind_fill(df1, df2)
      )==c(0,0)
    )
  })
  expect_true({
    df1 <- NULL
    df2 <- data.frame()
    all(
      dim(
        diffrprojects:::rbind_fill(df1, df2)
      )==c(0,0)
    )
  })
  expect_true({
    df1 <- NULL
    df2 <- NULL
    all(
      dim(
        diffrprojects:::rbind_fill(df1, df2)
      )==c(0,0)
    )
  })
  expect_true({
    df1 <- data.frame()
    df2 <- NULL
    all(
      dim(
        diffrprojects:::rbind_fill(df1, df2)
      )==c(0,0)
    )
  })
  expect_true({
    df1 <- NULL
    df2 <- data.frame(x=1:3)
    all(
      dim(
        diffrprojects:::rbind_fill(df1, df2)
      )==c(3,1)
    )
  })
  expect_true({
    df1 <- data.frame(z=letters[8:10], a=1)
    df2 <- NULL
    all(
      dim(
        diffrprojects:::rbind_fill(df1, df2)
      )==c(3,2)
    )
  })
})


context("tools shift")

test_that("shift works", {
  expect_true({
    x <- 1:3
    all(
      diffrprojects:::shift(x) == x
    )
  })
  expect_true({
    x <- 1:3
    all(
      diffrprojects:::shift(x) == x,
      identical(diffrprojects:::shift(x,1), c(NA,1L,2L)),
      identical(diffrprojects:::shift(x,"forward"), c(NA,1L,2L)),
      identical(diffrprojects:::shift(x,"lag"), c(NA,1L,2L)),
      identical(diffrprojects:::shift(x,"right"), c(NA,1L,2L))
    )
  })
  expect_true({
    x <- 1:3
    all(
      identical(diffrprojects:::shift(x,-1), c(2L, 3L, NA)),
      identical(diffrprojects:::shift(x,"backward"), c(2L, 3L, NA)),
      identical(diffrprojects:::shift(x,"lead"), c(2L, 3L, NA)),
      identical(diffrprojects:::shift(x,"left"), c(2L, 3L, NA))
    )
  })
  expect_true({
    x <- 1:3
    all(
      identical(diffrprojects:::shift(x, 1,  invert=TRUE), c(2L, 3L, NA) ),
      identical(diffrprojects:::shift(x, -1, invert=TRUE), c(NA,1L,2L)   )
    )
  })
  expect_true({
    x <- 1:3
    all(
      all(is.na(diffrprojects:::shift(x, 3))),
      length(is.na(diffrprojects:::shift(x, 3)))==3,
      all(is.na(diffrprojects:::shift(x, 5))),
      length(is.na(diffrprojects:::shift(x, 5)))==3,
      all(is.na(diffrprojects:::shift(x, -5))),
      length(is.na(diffrprojects:::shift(x, -5)))==3,
      all(is.na(diffrprojects:::shift(x, -3))),
      length(is.na(diffrprojects:::shift(x, -3)))==3
    )
  })
})





context("tools get_vector_element()")

test_that("tools get_vector_element() works", {
  x <- 1L:10L
  a <- letters[1:10]
  expect_identical( diffrprojects:::get_vector_element(a,1), a[1])
  expect_identical( diffrprojects:::get_vector_element(a,2), a[1:2])
  expect_identical( diffrprojects:::get_vector_element(a,2,3), a[3:4])
  expect_identical(
    diffrprojects:::get_vector_element(a,2,3),
    a[3:4]
  )
  expect_identical(
    diffrprojects:::get_vector_element(a,2,3,7),
    a[3:7]
  )
  expect_identical(
    diffrprojects:::get_vector_element(a,2,7,3),
    a[7:3]
  )
  expect_identical(
    diffrprojects:::get_vector_element(a,from=3,to=3),
    a[3:3]
  )
  expect_identical(
    diffrprojects:::get_vector_element(a,from=0,to=10),
    a
  )
  expect_error(
    diffrprojects:::get_vector_element(a)
  )
  expect_identical(
    diffrprojects:::get_vector_element(a, length=1, from = 4),
    a[4]
  )
  expect_identical(
    diffrprojects:::get_vector_element(a, length=1, to = 4),
    a[4]
  )
})





context("tools dim1 dim2 seq_dim1")

test_that("dim1 dim2 seq_dim1 works", {
  expect_error({
    diffrprojects:::dim1()
  })
  expect_true({
    diffrprojects:::dim1(1)==1
  })
  expect_true({
    is.null(diffrprojects:::dim2(1))
  })
  expect_true({
    all(
      diffrprojects:::dim2(data.frame())==0,
      diffrprojects:::dim1(data.frame())==0
    )
  })
  expect_true({
    all(
      diffrprojects:::dim2(data.frame(1:10))==1,
      diffrprojects:::dim1(data.frame(1:10))==10
    )
  })
  expect_true({
    all(
      length(diffrprojects:::seq_dim1(data.frame(4:5)))==2,
      length(diffrprojects:::seq_dim1(4:5)) ==2
    )
  })
})




context("tools dp_arrange")

test_that("dp_arrange works", {
  expect_identical({
    diffrprojects:::dp_arrange(data.frame(i=10:9))
  }, data.frame(i=10:9))
  expect_identical({
    diffrprojects:::dp_arrange(data.frame(i=10:9), i)
  }, data.frame(i=9:10))
  expect_identical(
    {
      diffrprojects:::dp_arrange(data.frame(i=10:9, b=1), i)
    },
    {
      x <- data.frame(i=9:10, b=1)
      row.names(x) <- 2:1
      x
    }
  )
})















