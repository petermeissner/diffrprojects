#' if(getRversion() >= "2.15.1"){
#'   utils::globalVariables(
#'     c(
#'       "text1_tokenized", "text2_tokenized", "token_i"
#'     )
#'   )
#' }
#'
#' #' stub
#' #' @keywords internal
#' moc <- function(
#'   text1     = NULL,
#'   text2     = NULL,
#'   tokenizer = function(text){text_tokenize_lines(text)},
#'   ignore    = function(...){FALSE},
#'   clean     = function(token){token},
#'   distance  = function(token1, token2){matrix(0,nrow = length(token1), ncol = length(token2))},
#'   alignment = function(m){}
#' ){
#'   # alignment and distances
#'
#'   #### trivial matches -- unique equal token matches
#'   message(" - trivial matching")
#'   res <-
#'     moc_helper_trivial_matches( tt1 = text1_tokenized, tt2 = text2_tokenized )
#'
#'
#'   #### easy matches -- text1 non-unique equal token matches
#'   message(" - easy matching 1")
#'   res <-
#'     rbind(
#'       res,
#'       moc_helper_easy_matches( tt1 = text1_tokenized, tt2 = text2_tokenized, res= res, type=1)
#'     )
#'
#'
#'   #### easy matches -- text2 non-unique equal token matches
#'   message(" - easy matching 2")
#'   res <-
#'     rbind(
#'       res,
#'       moc_helper_easy_matches( tt1 = text1_tokenized, tt2 = text2_tokenized, res= res, type=2)
#'     )
#'
#'   #### easy matches -- text2 non-unique equal token matches
#'   message(" - easy matching 3")
#'
#'   # prepare tt1 and tt2 as lists of data.frames
#'   tt1 <-
#'     text1_tokenized %>%
#'     dplyr::filter( !(token_i %in% res$token_i_1) )
#'
#'   tt2 <-
#'     text2_tokenized %>%
#'     dplyr::filter( !(token_i %in% res$token_i_2) )
#'
#'   tt1_split <- split_tt_by_length(tt1)
#'   tt2_split <- split_tt_by_length(tt2)
#'
#'   tt_names <- unique(c(names(tt1_split), names(tt2_split)))
#'
#'   # do the matches
#'   for( i in rev(seq_along(tt_names)) ) {
#'     cat(i, " ", append=TRUE)
#'     res <-
#'       moc_helper_easy_matches(
#'         tt1 = tt1_split[[tt_names[i]]],
#'         tt2 = tt2_split[[tt_names[i]]],
#'         res=res,
#'         type=3
#'       )
#'   }
#'   cat("\n")
#'
#'   # finishing matching of no-change type
#'   res$type <- "no-change"
#'   res$diff <- 0
#' }
