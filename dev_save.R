# this is some trying out coming up with a general framework for
# non stanard distance functions
#### ---------------------------------------------------------------------------

library(diffrprojects)
is_unique <- diffrprojects:::is_unique
is_minimum <- diffrprojects:::is_minimum
dim1 <- diffrprojects:::dim1
which_dist_min_absolute <- diffrprojects:::which_dist_min_absolute
choose_options <- diffrprojects:::choose_options


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
ignore    = function(from, to, token, token_i){rep(FALSE, length(token))}
clean     = function(token){token}
distance  = function(token1, token2){matrix(0, nrow = length(token1), ncol = length(token2))}

#### ---------------------------------------------------------------------------


moc <- function(
  text1     = NULL,
  text2     = NULL,
  tokenizer = function(text){text_tokenize_lines(text)},
  ignore    = function(from, to, token, token_i){rep(FALSE, length(token))},
  clean     = function(token){token},
  distance  = function(token1, token2){matrix(0, nrow = length(token1), ncol = length(token2))}
){}

  # tokenize
  message(" - tokenizing text")
  text1_tokenized <- tokenizer(text1)[1:3]
  text1_tokenized$token_i <- seq_along(text1_tokenized$token)

  text2_tokenized <- tokenizer(text2)[1:3]
  text2_tokenized$token_i <- seq_along(text2_tokenized$token)

  # clean
  message(" - cleaning token")
  text1_tokenized$token <- clean(text1_tokenized$token)
  text2_tokenized$token <- clean(text2_tokenized$token)

  # ignore
  message(" - ignoring token")
  text1_tokenized <- text1_tokenized %>% dplyr::filter( !ignore(text1_tokenized) )
  text2_tokenized <- text2_tokenized %>% dplyr::filter( !ignore(text2_tokenized) )

  # alignment and distances

  #### trivial matches -- unique equal token matches
  message(" - trivial matching")
  res <-
    moc_helper_trivial_matches( tt1 = text1_tokenized, tt2 = text2_tokenized )


  #### easy matches -- text1 non-unique equal token matches
  message(" - easy matching 1")
  res <-
    rbind(
      res,
      moc_helper_easy_matches( tt1 = text1_tokenized, tt2 = text2_tokenized, res= res, type=1)
    )


  #### easy matches -- text2 non-unique equal token matches
  message(" - easy matching 2")
  res <-
    rbind(
      res,
      moc_helper_easy_matches( tt1 = text1_tokenized, tt2 = text2_tokenized, res= res, type=2)
    )

  #### easy matches -- text2 non-unique equal token matches
  message(" - easy matching 3")

  # prepare tt1 and tt2 as lists of data.frames
  tt1 <-
    text1_tokenized %>%
    filter( !(token_i %in% res$token_i_1) ) %>%
    mutate( token_length = nchar(token) ) %>%
    split(.$token_length) %>%
    lapply( dplyr::mutate, token_length = NULL ) %>%
    lapply(as.data.table) %>%
    lapply(setkey, token, token_i)


  tt2 <-
    text2_tokenized %>%
    filter( !(token_i %in% res$token_i_2) ) %>%
    mutate( token_length = nchar(token) ) %>%
    split(.$token_length) %>%
    lapply( dplyr::mutate, token_length = NULL ) %>%
    lapply(as.data.table) %>%
    lapply(setkey, token, token_i)

  tt_names <- unique(c(names(tt1), names(tt2)))

  # do the matches
  for( i in rev(seq_along(tt_names)) ) {
    cat(i, " ", append=TRUE)
    res <-
      moc_helper_easy_matches(
        tt1 = tt1[[tt_names[i]]],
        tt2 = tt2[[tt_names[i]]],
        res=res,
        type=3
      )
  }
  cat("\n")

  # finishing matching of no-change type
  res$type <- "no-change"
  res$diff <- 0

  #### using dist function to match remaining
  tt1 <-
    text1_tokenized %>%
    filter( !(token_i %in% res$token_i_1) )

  tt2 <-
    text2_tokenized %>%
    filter( !(token_i %in% res$token_i_2) )


  # long strings first
  a <- adist(rep(tt1$token), rep(tt2$token))
  pryr::object_size(a)



  # alignment via hungarian solution to assignment problem

  library(clue)

  m <- adist(tt1$token, tt2$token)
  solution_index_v <- as.numeric(solve_LSAP(m))
  solution_index_m <- as.matrix(cbind(seq_along(solution_index_v),solution_index_v))

  aligned <- cbind(tt1,tt2[solution_index,], dist = m [solution_index_m])




  #### using dist function to match remaining
  tt1 <-
    text1_tokenized %>%
    filter( !(token_i %in% res$token_i_1) )

  tt2 <-
    text2_tokenized %>%
    filter( !(token_i %in% res$token_i_2) )

  if( is.character(distance) ){
    a <- stringdist::amatch(text1_tokenized$token, text2_tokenized$token, method=distance, ...)
  }else{
    stop("Not Implemented!")
    distance(tt1, tt2)
  }

