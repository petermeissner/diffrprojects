#### ---------------------------------------------------------------------------

library(diffrprojects)
devtools::load_all()


#### ---------------------------------------------------------------------------

text_path  <- "~/Dropbox/IDEP_Database/rawdata/AUT/txts"

text_files <- list.files(text_path, pattern = "txt", full.names = TRUE)

text1 <- rtext$new(text_file=text_files[13], encoding="latin1")$text_get(2000)
text2 <- rtext$new(text_file=text_files[14], encoding="latin1")$text_get(2000)

testfiles <- rtext:::testfile(pattern="rc_\\d.txt", full.names = TRUE)

#text1 <- rtext$new(text_file=stringb:::test_file("rc_2.txt"))$text_get()
#text2 <- rtext$new(text_file=stringb:::test_file("rc_3.txt"))$text_get()

#### ---------------------------------------------------------------------------


dp <-
  diffrproject$new()$
  text_add(list("aaa\nbb\ncccc\ndd\nee\nff\ny\n", "bb\ncccd\ndd\nddd\nee\nff\n"))$
  text_link()$
  debug()$
  text_align( maxDist = 1 )$
  alignment_code(1,1:24, "wuppah", 1)$
  alignment_code(1,c(3,8,9,11), "womppah", 1)$
  debug()

dp$text_code_regex(text=1, x="a", pattern=".*a.*", val=TRUE)
dp$text_code_regex(text=2, x="b", pattern=".*[^a].*", val=TRUE)

dp$text_data()

dp$alignment
dp$alignment_data


dp$text_data_inherit()

dp$text_data()


#### start

link = 1

















































