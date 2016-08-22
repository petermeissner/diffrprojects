#### ---------------------------------------------------------------------------

library(diffrprojects)
is_unique <- diffrprojects:::is_unique
is_minimum <- diffrprojects:::is_minimum
dim1 <- diffrprojects:::dim1
which_dist_min_absolute <- diffrprojects:::which_dist_min_absolute


library(dplyr)
library(data.table)
library(dtplyr)
library(Rcpp)





#### ---------------------------------------------------------------------------

text_path  <- "~/Dropbox/IDEP_Database/rawdata/AUT/txts"

text_files <- list.files(text_path, pattern = "txt", full.names = TRUE)

text1 <- rtext$new(text_file=text_files[13], encoding="latin1")$text_get()
text2 <- rtext$new(text_file=text_files[14], encoding="latin1")$text_get()

text1 <- rtext$new(text_file=stringb:::test_file("rc_2.txt"))$text_get()
text2 <- rtext$new(text_file=stringb:::test_file("rc_3.txt"))$text_get()

tokenizer <- text_tokenize_words
clean     <- function(x){x}
distance  <- function(x,y){matrix(runif(length(x)*length(y), 0, 100), nrow=length(x), ncol=length(y))}

#### ---------------------------------------------------------------------------


moc <- function(
  text1     = NULL,
  text2     = NULL,
  tokenizer = text_tokenize_lines,
  clean     = function(x){x},
  distance  = NULL
){}

  # tokenize
  message(" - tokenizing")
  text1_tokenized <- tokenizer(text1)[1:3]
  text1_tokenized$token_i <- seq_along(text1_tokenized$token)

  text2_tokenized <- tokenizer(text2)[1:3]
  text2_tokenized$token_i <- seq_along(text2_tokenized$token)

  # clean
  message(" - cleaning")
  text1_tokenized$token <- clean(text1_tokenized$token)
  text2_tokenized$token <- clean(text2_tokenized$token)


  # alignment and distances

  #### trivial matches -- unique 1:1 matches
  message(" - trivial matching")
  res <- moc_helper_trivial_matches( tt1 = text1_tokenized, tt2 = text2_tokenized )

  #### matching text1 tokens and text2 tokens
  message(" - easy matching")
  tt1 <- text1_tokenized %>% subset( !(token_i %in% res$token_i_1) ) %>% data.table::as.data.table() %>% data.table::setkey(token)
  tt2 <- text2_tokenized %>% subset( !(token_i %in% res$token_i_2) ) %>% data.table::as.data.table() %>% data.table::setkey(token)

  dist         <- which_dist_min_absolute(tt1$token_i, res$token_i_1)
  tt1$min_dist_1 <- dist$minimum
  tmp          <- subset(res[dist$location, ], TRUE, c(token_i_1, token_i_2))
  names(tmp)   <- paste0("res_",names(tmp))
  tt1_tmp <- cbind(subset(tt1,TRUE, c(token, token_i, min_dist_1)), tmp)
  tt1_tmp <- suppressWarnings( left_join(tt1_tmp, subset(tt2, TRUE, c(token, token_i)), by="token") )
  names(tt1_tmp)[names(tt1_tmp)=="token_i.x"] <- "token_i_1"
  names(tt1_tmp)[names(tt1_tmp)=="token_i.y"] <- "token_i_2"

  tt1_tmp[, token := NULL]
  tt1_tmp[, res_token_i_1 := NULL]

  tt1_tmp$min_dist_2 <- 0L
  tt1_tmp$min_dist_2 <- abs(tt1_tmp$res_token_i_2 - tt1_tmp$token_i_2)

  tt1_tmp[, token := NULL]
  tt1_tmp[, res_token_i_2 := NULL]

  setorder(tt1_tmp, min_dist_1, min_dist_2, token_i_1, token_i_2)

  tt1_tmp <- subset(tt1_tmp, TRUE, c(token_i_1, token_i_2))
  tt1_tmp <- tt1_tmp[!is.na(tt1_tmp$token_i_2),]
  tt1_tmp[, min_dist_1 := NULL]
  tt1_tmp[, min_dist_2 := NULL]





















