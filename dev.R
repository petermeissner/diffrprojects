#### ---------------------------------------------------------------------------
#
# library(diffrprojects)
#
# dp <-
#   diffrproject$new()$
#   text_add(list("aaa\nbb\ncccc\ndd\nee\nff\ny\n", "bb\ncccd\ndd\nddd\nee\nff\n"))$
#   text_link()$
#   text_align( maxDist = 1 )$
#   alignment_code(1,1:24, "wuppah", 1)$
#   alignment_code(1,c(3,8,9,11), "womppah", 1)
#
# dp$text_code_regex(text=1, x="a", pattern="a", val=TRUE)
# dp$text_code_regex(text=2, x="b", pattern="[^a]", val=TRUE)
#
# dp$text_data_inherit()
#
# dp$debug()
#
# dp$export_sqlite()
#
#


#### ---------------------------------------------------------------------------

# library(diffrprojects)
#
# dp <-
#   diffrproject$new()$
#   text_add(list(prometheus_late, prometheus_early))$
#   text_link()$
#   text_align( maxDist = 1 )
#
# dp$text_code_regex(text=1, x="you", pattern="du|Du", val=TRUE)
# dp$text_code_regex(text=1, x="me", pattern="ich|Ich", val=TRUE)
#
# dp$text_code_regex(text=2, x="you", pattern="du|Du", val=TRUE)
# dp$text_code_regex(text=2, x="me", pattern="ich|Ich", val=TRUE)
#
# dp$debug()
#
# dp$tokenize_text_data_words()
#
#
# sort_alignment(dp$alignment[[1]], ti1 = "token_i_1", ti2 = "token_i_2" )
#
#
# dp$alignment_data_full(1, FALSE)


#### ---------------------------------------------------------------------------


library(diffrprojects)


dp <-
  diffrproject$new()$
  text_add(list(prometheus_late, prometheus_early))$
  text_link()$
  text_align( maxDist = 1 )

# dp$text_code_regex(text=1, x="you", pattern="du|Du", val=TRUE)
# dp$text_code_regex(text=1, x="me", pattern="ich|Ich", val=TRUE)
#
# dp$text_code_regex(text=2, x="you", pattern="du|Du", val=TRUE)
# dp$text_code_regex(text=2, x="me", pattern="ich|Ich", val=TRUE)
#
# dp$alignment_code(1,1,"x", "muhaha")
# dp$alignment_code(1,1,"dings", "check this out")

 dp$debug()




























