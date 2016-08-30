#### diffrproject ==============================================================

context("\ndp_base") # ====================================================



context("diffrproject init") # =================================================

test_that("diffrproject can be created", {
  expect_true({
    dp <- diffrproject$new()
    "diffrproject"  %in% class(dp)
  })
})



context("diffrproject text_add") # ===============================

test_that("text can be added", {
  expect_error({
    dp <- diffrproject$new()
    dp$text_add(text="")
  }, NA)
  expect_error({
    dp <- diffrproject$new()
    dings <- rtext::rtext$new("")
    dp$text_add(rtext=dings)
  }, NA)
  expect_error({
    dp <- diffrproject$new()
    dp$text_add("", name = 1)
  }, NA)
  expect_error({
    dp <- diffrproject$new()
    dp$text_add(rtext = rtext::rtext$new(""), name = "a")
  }, NA)
  expect_true({
    dp <- diffrproject$new()
    dp$text_add(rtext =rtext::rtext$new(""), name = "a")
    dp$text_add(rtext =rtext::rtext$new(""), name = "b")
    dp$text_add(text="", name="c")
    dp$text_add(text="", name="c")
    length(dp$text) == 3
  })
  expect_true({
    dp <- diffrproject$new()
    dp$text_add(rtext =rtext::rtext$new(""))
    dp$text_add(rtext =rtext::rtext$new(""))
    dp$text_add(text="")
    dp$text_add(text="")
    length(dp$text) == 4
  })
  expect_true({
    dp <- diffrproject$new()
    dp$text_add(rtext =rtext::rtext$new(""))
    dp$text_add(rtext =rtext::rtext$new(""))
    dp$text_add(rtext =rtext::rtext$new(""))
    dp$text_add(rtext =rtext::rtext$new(""))
    dp$text_add(rtext =rtext::rtext$new(""), name = "a")
    dp$text_add(rtext =rtext::rtext$new(""), name = "b")
    dp$text_add(rtext =rtext::rtext$new(""), name = "c")
    dp$text_add(rtext =rtext::rtext$new(""), name = "a")
    length(dp$text) == 7
  })
})


test_that("names and ids are unique", {
  expect_true({
    dp <- diffrproject$new()
    dp$text_add(rtext =rtext::rtext$new(""))
    dp$text_add(rtext =rtext::rtext$new(""))
    dp$text_add(rtext =rtext::rtext$new(""))
    dp$text_add(rtext =rtext::rtext$new(""))
    dp$text_add(rtext =rtext::rtext$new(""), name = "a")
    dp$text_add(rtext =rtext::rtext$new(""), name = "b")
    dp$text_add(rtext =rtext::rtext$new(""), name = "c")
    dp$text_add(rtext =rtext::rtext$new(""), name = "a")
    all(names(dp$text) == unique(names(dp$text)))
  })
  expect_true({
    dp <- diffrproject$new()
    dp$text_add(rtext =rtext::rtext$new(""))
    dp$text_add(rtext =rtext::rtext$new(""))
    dp$text_add(rtext =rtext::rtext$new("a"))
    dp$text_add(rtext =rtext::rtext$new(""))
    dp$text_add(rtext =rtext::rtext$new("123"), name = "a")
    dp$text_add(rtext =rtext::rtext$new(""), name = "b")
    dp$text_add(rtext =rtext::rtext$new("123"), name = "c")
    dp$text_add(rtext =rtext::rtext$new("123"), name = "a")
    ids <- vapply(dp$text, `[[`, "", "id")
    all(ids == unique(ids))
  })

  testfiles <- rtext:::testfile(pattern="rc_\\d.txt", full.names = TRUE)
  expect_true({
    dp <- diffrproject$new()
    dp$text_add(text_file = testfiles)
    length(dp$text) == 3
  })

  testfiles <- rtext:::testfile(pattern="rc_\\d.txt", full.names = TRUE)
  texts <- lapply(testfiles, text_read, n = 5)
  expect_true({
    dp <- diffrproject$new()
    dp$text_add(text = texts)
    length(dp$text) == 3
  })

})

context("diffrproject text_delete") # ===============================

test_that("text can be deleted", {
  expect_true({
    dp <- diffrproject$new()
    dp$text_add(rtext =rtext::rtext$new(""))
    dp$text_add(rtext =rtext::rtext$new(""))
    dp$text_add(rtext =rtext::rtext$new(""))
    dp$text_add(rtext =rtext::rtext$new(""))
    dp$text_add(rtext =rtext::rtext$new(""), name = "a")
    dp$text_add(rtext =rtext::rtext$new(""), name = "b")
    dp$text_add(rtext =rtext::rtext$new(""), name = "c")
    dp$text_add(rtext =rtext::rtext$new(""), name = "a")
    dp$text_delete()
    dp$text_delete()
    dp$text_delete()
    dp$text_delete()
    dp$text_delete()
    dp$text_delete()
    dp$text_delete()
    length(dp$text) == 0
  })
  expect_true({
    dp <- diffrproject$new()
    dp$text_add(rtext =rtext::rtext$new(""))
    dp$text_add(rtext =rtext::rtext$new(""))
    dp$text_add(rtext =rtext::rtext$new(""))
    dp$text_add(rtext =rtext::rtext$new(""))
    dp$text_add(rtext =rtext::rtext$new(""), name = "a")
    dp$text_add(rtext =rtext::rtext$new(""), name = "b")
    dp$text_add(rtext =rtext::rtext$new(""), name = "c")
    dp$text_add(rtext =rtext::rtext$new(""), name = "a")
    dp$text_delete(1)
    length(dp$text) == 6
  })
  expect_true({
    dp <- diffrproject$new()
    dp$text_add(rtext =rtext::rtext$new(""))
    dp$text_add(rtext =rtext::rtext$new(""))
    dp$text_add(rtext =rtext::rtext$new(""))
    dp$text_add(rtext =rtext::rtext$new(""))
    dp$text_add(rtext =rtext::rtext$new(""), name = "a")
    dp$text_add(rtext =rtext::rtext$new(""), name = "b")
    dp$text_add(rtext =rtext::rtext$new(""), name = "c")
    dp$text_add(rtext =rtext::rtext$new(""), name = "a")
    dp$text_delete("b")
    dp$text_delete("b")
    length(dp$text) == 6
  })
  expect_true({
    dp <- diffrproject$new()
    dp$text_add(rtext =rtext::rtext$new("1"))
    dp$text_add(rtext =rtext::rtext$new("2"))
    dp$text_add(rtext =rtext::rtext$new("3"))
    dp$text_add(rtext =rtext::rtext$new("4"), name = "a")
    ID <- dp$text$a$id
    dp$text_add(rtext =rtext::rtext$new("5"))
    dp$text_add(rtext =rtext::rtext$new("6"), name = "b")
    dp$text_add(rtext =rtext::rtext$new("7"), name = "c")
    dp$text_delete(id=ID)
    !("a"  %in% names(dp$text))
  })
  expect_true({
    dp <- diffrproject$new()
    dp$text_add(rtext =rtext::rtext$new(""))
    dp$text_add(rtext =rtext::rtext$new(""))
    dp$text_add(rtext =rtext::rtext$new(""))
    dp$text_add(rtext =rtext::rtext$new(""), name = "a")
    ID <- dp$text$a$id
    dp$text_add(rtext =rtext::rtext$new(""))
    dp$text_add(rtext =rtext::rtext$new(""), name = "b")
    dp$text_add(rtext =rtext::rtext$new(""), name = "c")
    dp$text_delete(id=ID)
    !("a"  %in% names(dp$text))
  })
})











