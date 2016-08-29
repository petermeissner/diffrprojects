#' if(getRversion() >= "2.15.1"){
#'   utils::globalVariables(
#'     c(
#'       "token_i_1", "token_i_2",
#'       "text1_tokenized", "text2_tokenized",
#'       "token", ".", "...", "res_token_i_1", "res_token_i_2",
#'       "min_dist_1"
#'     )
#'   )
#' }
#'
#' #' splitting a tokenized text
#' #' @param tt tokenized text
#' #' @keywords internal
#' split_tt_by_length <- function(tt){
#'   tt %>%
#'     dplyr::mutate( token_length = nchar(token) ) %>%
#'     split( .$token_length ) %>%
#'     lapply( dplyr::mutate, token_length = NULL ) %>%
#'     lapply( as.data.table  ) %>%
#'     lapply( setkey, "token", "token_i"  )
#' }
#'
#'
#' #' trivial matches
#' #'
#' #' method of comparison helper function
#' #' @param tt1 tokenized text number 1
#' #' @param tt2 tokenized text number 2
#' #' @keywords internal
#' moc_helper_trivial_matches <- function(tt1, tt2){
#'   # preparation
#'   tt1 <- subset( tt1, is_unique(token), select=c("token", "token_i"))
#'   tt1 <- data.table::as.data.table(tt1)
#'   data.table::setkey("tt1", "token")
#'
#'   tt2 <- subset( tt2, is_unique(token), select=c("token", "token_i"))
#'   tt2 <- data.table::as.data.table(tt2)
#'   data.table::setkey("tt2", "token")
#'
#'   # merge / join
#'   matches <- suppressWarnings(dplyr::inner_join(tt1, tt2, by="token"))
#'              data.table::setkey(matches, "token_i.x", "token_i.y")
#'
#'   # clean up names
#'   names(matches) <-
#'     names(matches) %>%
#'     stringb::text_replace("\\.", "_") %>%
#'     stringb::text_replace("x", "1") %>%
#'     stringb::text_replace("y", "2")
#'
#'   # return
#'   return(matches)
#' }
#'
#' #' easy matches 1
#' #'
#' #' method of comparison helper function
#' #' @param tt1 tokenized text number 1
#' #' @param tt2 tokenized text number 2
#' #' @keywords internal
#' moc_helper_easy_matches <- function(tt1, tt2, res, type=c(1,2), fullreturn=TRUE){
#'   # check input
#'   if( is.null(tt1) | is.null(tt2) ){
#'     # return
#'     if( fullreturn ){
#'       return(res)
#'     }else{
#'       return(data.frame())
#'     }
#'   }
#'   # preparation
#'   tt1_tmp <-
#'     tt1 %>%
#'     subset(select = c("token", "token_i") ) %>%
#'     dplyr::filter(
#'       !(token_i %in% res$token_i_1)
#'     ) %>%
#'     as.data.table()
#'   setkey(tt1_tmp, "token_i")
#'
#'   tt2_tmp <-
#'     tt2 %>%
#'     dplyr::select(token, token_i) %>%
#'     dplyr::filter(
#'       !(token_i %in% res$token_i_2)
#'     ) %>%
#'     as.data.table()
#'   setkey(tt2_tmp, "token_i")
#'
#'   # decide which tokens (from text1 or from text2) should be unique
#'   if( type == 1){
#'     tt1_tmp <- tt1_tmp %>%  dplyr::filter( is_unique(token) )
#'   }else if( type == 2){
#'     tt2_tmp <- tt2_tmp %>%  dplyr::filter( is_unique(token) )
#'   }
#'
#'   # get and order possible matches
#'   matches <-
#'     suppressWarnings(
#'       moc_helper_get_options_ordered_by_dist(tt1_tmp, tt2_tmp, res)
#'     )
#'
#'   # process optional matches
#'   chosen <-
#'     choose_options(matches$token_i_1, matches$token_i_2, res$token_i_1, res$token_i_2) %>%
#'     as.data.table() %>%
#'     setkey("token_i_1")
#'
#'   # add token to get it rbind-ed to res
#'   tt1_tmp <- stats::setNames(tt1_tmp, c("token", "token_i_1"))
#'   chosen <- dplyr::left_join(chosen, tt1_tmp, by="token_i_1")
#'
#'   # return
#'   if( fullreturn ){
#'     return( rbind(res, data.table(chosen), fill=TRUE) )
#'   }else{
#'     return(chosen)
#'   }
#' }
#'
#'
#' #' get options for machtches
#' #'
#' #' method of comparison helper function
#' #' @param tt1 tokenized text number 1
#' #' @param tt2 tokenized text number 2
#' #' @param res data.frame of already matched
#' #' @import data.table
#' #' @keywords internal
#' moc_helper_get_options_ordered_by_dist <- function(tt1, tt2, res){
#'   # distance between availible token positions and positions of tokens already matched
#'   dist           <- which_dist_min_absolute(tt1$token_i, res$token_i_1)
#'   tt1$min_dist_1 <- dist$minimum
#'   # preapare information from res
#'   res_tmp <-
#'     res[dist$location, ] %>%
#'     dplyr::select(token_i_1, token_i_2) %>%
#'     stats::setNames( paste0("res_",names(.)) )
#'   # combine res with info from tt1
#'   tt1_tmp <-
#'     tt1 %>%
#'     dplyr::select(token, token_i, min_dist_1) %>%
#'     cbind(res_tmp)
#'   # join tt1 and tt2
#'   tt2_tmp <- dplyr::select(tt2, token, token_i)
#'   tt1_tmp <-
#'     tt1_tmp %>%
#'     dplyr::inner_join(tt2_tmp, by="token")
#'   names(tt1_tmp)[names(tt1_tmp)=="token_i.x"] <- "token_i_1"
#'   names(tt1_tmp)[names(tt1_tmp)=="token_i.y"] <- "token_i_2"
#'   tt1_tmp <- data.table::as.data.table(tt1_tmp)
#'   # delete columns
#'   tt1_tmp[, token := NULL]
#'   tt1_tmp[, res_token_i_1 := NULL]
#'   # add token_i_2 position distance
#'   tt1_tmp$min_dist_2 <- 0L
#'   tt1_tmp$min_dist_2 <- abs(tt1_tmp$res_token_i_2 - tt1_tmp$token_i_2)
#'   # delete columns
#'   tt1_tmp[, res_token_i_2 := NULL]
#'   # sort
#'   data.table::setorder(tt1_tmp, "min_dist_1", "min_dist_2", "token_i_1", "token_i_2")
#'   # delete columns
#'   tt1_tmp[, "min_dist_1" := NULL]
#'   tt1_tmp[, "min_dist_2" := NULL]
#'   # return
#'   return(tt1_tmp)
#' }
