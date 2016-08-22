#' trivial matches
#'
#' merhtod of comparison helper function
#' @param tt1 tokenized text number 1
#' @param tt2 tokenized text number 2
#' @export
moc_helper_trivial_matches <- function(tt1, tt2){
  # preparation
  tt1 <- subset( tt1, TRUE, c(token, token_i))
  tt1 <- data.table::as.data.table(tt1)
  data.table::setkey(tt1, token)

  tt2 <- subset( tt2, TRUE, c(token, token_i))
  tt2 <- data.table::as.data.table(tt2)
  data.table::setkey(tt2, token)

  # merge / join
  matches <-
    suppressWarnings(dplyr::inner_join(tt1, tt2, by="token"))
  data.table::setkey(matches, token_i.x, token_i.y)
  # clean up names
  names(matches) <-
    names(matches) %>%
    stringb::text_replace("\\.", "_") %>%
    stringb::text_replace("x", "1") %>%
    stringb::text_replace("y", "2")
  # keep unique matches only
  iffer <- unique(matches$token_i_1)
  matches <- matches[iffer, ]
  iffer <- unique(matches$token_i_2)
  matches <- matches[iffer, ]
  # return
  return(matches)
}
