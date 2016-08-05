#### ---------------------------------------------------------------------------

library(diffrprojects)


#### ---------------------------------------------------------------------------


if( !length(grepl("Windows", as.list(Sys.getenv())$OS))==0 ){
  text_path  <- "/users/peter/Dropbox/IDEP_Database/rawdata/AUT/txts"
}else{
  text_path  <- "~/Dropbox/IDEP_Database/rawdata/AUT/txts"
}

text_files <- list.files(text_path, pattern = "txt", full.names = TRUE)
dp <- diffrproject$new()
dp$text_add(text_files, encoding="latin1")
dp$texts_link()


length(dp$texts)
names(dp$texts)
dp$text_data()



text1 <- rtext$new(text_file=text_files[13], encoding="latin1")
text2 <- rtext$new(text_file=text_files[14], encoding="latin1")

text1 <- rtext$new(text_file=stringb:::test_file("rc_2.txt"))
text2 <- rtext$new(text_file=stringb:::test_file("rc_3.txt"))

moc_linewise_bow <- function(text1, text2, ...){

  # tokenize
  text1_tokenized <- text_tokenize(text1$text_get(), "\n")
  text2_tokenized <- text_tokenize(text2$text_get(), "\n")

  text1_tokenized$token_i <- seq_along(text1_tokenized$token)
  text2_tokenized$token_i <- seq_along(text2_tokenized$token)

  # clean
  # ...

  # trivial matches
  matches <- merge(text1_tokenized, text2_tokenized, by="token")
  matches <-
    matches[
      is_unique(matches$token_i.x) & is_unique(matches$token_i.y)  ,
    ]
  matches <- matches[order(matches$token_i.x, matches$token_i.y),]


  # statistics
  sum(!(text1_tokenized$token_i %in% matches$token_i.x))
  sum(!(text2_tokenized$token_i %in% matches$token_i.y))

  # data
  data.frame(
    text1 = substr(text1_tokenized$token, 1,15),
    line1 = seq_along(text1_tokenized$from),
    line2 = seq_along(text2_tokenized$from)[m1],
    text2 = substr(text2_tokenized$token[m1], 1,15)
  )


}


# moc_template <- function(text1, text2, tokenize, clean, distance){
#
# }



#### ---------------------------------------------------------------------------

