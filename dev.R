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
  diffrproject$
  new()$
  text_add(list(text1, text2))$
  text_link()$
  debug()$
  text_align()

dp$alignment
dp$alignment_data



















