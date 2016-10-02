context("\ndp_export")

context("dp_export : imported equals exported")

test_that("imported equals exported", {
  dp <-
    diffrproject$new()$
    text_add(list("aaa\nbb\ncccc\ndd\nee\nff\ny\n", "bb\ncccd\ndd\nddd\nee\nff\n"))$
    text_link()$
    text_align( maxDist = 1 )$
    alignment_code(1,1:24, "wuppah", 1)$
    alignment_code(1,c(3,8,9,11), "womppah", 1)$
    debug()

  dp$text_code_regex(text=1, x="a", pattern="a", val=TRUE)
  dp$text_code_regex(text=2, x="b", pattern="[^a]", val=TRUE)

  dp$text_data_inherit()

  dp_orig <- dp$clone(deep=TRUE)

  db_path <- tempfile()
  dp$export_sqlite(db_path)

  dp_loaded <- diffrproject$new()
  dp_loaded$import_sqlite(db_path)

  expect_true({dp_orig$meta$ts_created == dp_loaded$meta$ts_created })
  expect_true({dp_orig$meta$db_path    == dp_loaded$meta$db_path })
  expect_true({dp_orig$meta$file_path  == dp_loaded$meta$file_path })
  expect_true({dp_orig$meta$project_id == dp_loaded$meta$project_id })

  expect_true({
    orig   <- dp_orig$alignment[[1]]
    loaded <- dp_loaded$alignment[[1]]
    all(orig[!is.na(orig)] == loaded[!is.na(loaded)])
  })

  expect_true({
    orig   <- dp_orig$alignment_data[[1]][["womppah"]]
    loaded <- dp_loaded$alignment_data[[1]][["womppah"]]
    all(orig[!is.na(orig)] == loaded[!is.na(loaded)])
  })

  expect_true({
    orig   <- dp_orig$alignment_data[[1]][["wuppah"]]
    loaded <- dp_loaded$alignment_data[[1]][["wuppah"]]
    all(orig[!is.na(orig)] == loaded[!is.na(loaded)])
  })

  expect_true({
    dp_orig$text[[1]]$id == dp_loaded$text[[1]]$id
  })
  expect_true({
    dp_orig$text[[1]]$sourcetype == dp_loaded$text[[1]]$sourcetype
  })
  expect_true({
    dp_orig$text[[1]]$encoding == dp_loaded$text[[1]]$encoding
  })
  expect_true({
    dp_orig$text[[1]]$save_file == dp_loaded$text[[1]]$save_file | is.na(dp_loaded$text[[1]]$save_file)
  })
  expect_true({
    dp_orig$text[[1]]$text_file == dp_loaded$text[[1]]$text_file | is.na(dp_loaded$text[[1]]$text_file)
  })
  expect_true({
    all(
      dp_orig$text[[1]]$get("char") == dp_loaded$text[[1]]$get("char")
    )
  })
  expect_true({
    all(
      dp_orig$text[[1]]$get("char_data")[[1]] ==
      dp_loaded$text[[1]]$get("char_data")[[1]]
    ) &
    all(
      dp_orig$text[[1]]$get("char_data")[[2]] ==
      dp_loaded$text[[1]]$get("char_data")[[2]]
    )
  })

})
