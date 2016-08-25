#### ---------------------------------------------------------------------------

library(diffrprojects)
is_unique <- diffrprojects:::is_unique
is_minimum <- diffrprojects:::is_minimum
dim1 <- diffrprojects:::dim1
which_dist_min_absolute <- diffrprojects:::which_dist_min_absolute
choose_options <- diffrprojects:::choose_options
split_tt_by_length <- diffrprojects:::split_tt_by_length


library(dplyr)
library(data.table)
library(dtplyr)
library(Rcpp)





#### ---------------------------------------------------------------------------

text_path  <- "~/Dropbox/IDEP_Database/rawdata/AUT/txts"

text_files <- list.files(text_path, pattern = "txt", full.names = TRUE)

text1 <- rtext$new(text_file=text_files[13], encoding="latin1")$text_get()
text2 <- rtext$new(text_file=text_files[14], encoding="latin1")$text_get()

#text1 <- rtext$new(text_file=stringb:::test_file("rc_2.txt"))$text_get()
#text2 <- rtext$new(text_file=stringb:::test_file("rc_3.txt"))$text_get()

tokenizer <- text_tokenize_words
ignore    = function(...){FALSE}
clean     = function(token){token}
distance  = c("osa", "lv", "dl", "hamming", "lcs", "qgram", "cosine", "jaccard", "jw", "soundex")

#### ---------------------------------------------------------------------------


diffr <- function(
  text1     = NULL,
  text2     = NULL,
  tokenizer = function(text){text_tokenize_lines(text)},
  ignore    = NULL,
  clean     = NULL,
  distance  = c("osa", "lv", "dl", "hamming", "lcs", "qgram", "cosine", "jaccard", "jw", "soundex")
){}

  # tokenize
  message(" - tokenizing text")
  text1_tokenized <- tokenizer(text1)[1:3]
  text1_tokenized$token_i <- seq_along(text1_tokenized$token)

  text2_tokenized <- tokenizer(text2)[1:3]
  text2_tokenized$token_i <- seq_along(text2_tokenized$token)

  # clean
  if( !is.null(clean) ){
    message(" - cleaning token")
    text1_tokenized$token <- clean(text1_tokenized$token)
    text2_tokenized$token <- clean(text2_tokenized$token)
  }


  # ignore
  if( !is.null(ignore) ){
    message(" - ignoring token")
    text1_tokenized <- ignore(text1_tokenized)
    text2_tokenized <- ignore(text2_tokenized)
  }


  # alignment and distances
  if( is.character(distance) ){
    message(" - doing distance calculation and alignment")
    a <- stringdist::amatch(text1_tokenized$token, text2_tokenized$token, method=distance)
    alignment <- data.frame(text1_tokenized, text2_tokenized[a, ])
  }else{
    stop("using non standard distance functions is not implemented yet - sorry")
  }

















