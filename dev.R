#### ---------------------------------------------------------------------------

library(diffrprojects)
library(magrittr)
library(dplyr)
library(hellno)

library(stringb)
library(rtext)

#### ---------------------------------------------------------------------------


text_path  <- "~/Dropbox/IDEP_Database/rawdata/AUT/txts"
text_files <- list.files(text_path, pattern = "txt", full.names = TRUE)


dp <- diffrproject$new()
dp$text_add(
  rtext =
    rtext$new(text_file=text_files[1], encoding="latin1")
)

dp$text_add(
  rtext = rtext$new(text_file=text_files[2], encoding="latin1"),
  name = basename(text_files[2])
)

length(dp$texts)
names(dp$texts)

dp$texts_link(1,2)
dp$links

dp$texts_link(1,2, TRUE)

#### ---------------------------------------------------------------------------

