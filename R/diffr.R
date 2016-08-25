#' FUNCTION_TITLE
#'
#' FUNCTION_DESCRIPTION
#'
#' @param text1 first text
#' @param text2 second text
#' @param tokenizer defaults to NULL which will trigger linewise tokenization;
#'        accepts a function that turns a text into a token data frame;
#'        a token data frame has at least three columns:
#'        from (first character of token),
#'        to (last character of token)
#'        token (the token)
#' @param ignore defaults to NULL which means that nothing is ignored;
#'        function that accepts a token data frame (see above) and returns a
#'        possibly subseted data frame of hte same form
#' @param clean defaults to NULL which means that nothing cleaned; accepts a
#'        function that takes a vector of tokens and returns a vector of same
#'        length - potentially clean up
#' @param distance defaults to Levenshtein ("lv"); see \link[stringdist]{amatch},
#'        \link[stringdist]{stringdist-metrics}, \link[stringdist]{stringdist}
#' @param ... further arguments passed through to distance function
#'
#' @return dataframe with tokens aligned according to distance
#'
#' @export
diffr <- function(
  text1     = NULL,
  text2     = NULL,
  tokenizer = NULL,
  ignore    = NULL,
  clean     = NULL,
  distance  = c("lv", "osa", "dl", "hamming", "lcs", "qgram", "cosine", "jaccard", "jw", "soundex"),
  ...
){
  # checking input
  if( is.function(distance) ){ stop("using non standard distance functions is not implemented yet - sorry") }
  stopifnot(
    !is.null(text1),
    is.character(text1),
    !is.null(text2),
    is.character(text2)
  )

  # assigning default options
  if( is.null(tokenizer) ){ tokenizer <- stringb::text_tokenize_lines }
  if( is.null(clean) ){     clean     <- function(x){x} }
  if( is.null(ignore) ){    ignore    <- function(x){x} }
  distance <- distance[1]

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
  text1_tokenized_prei <- text1_tokenized
  text2_tokenized_prei <- text2_tokenized
  text1_tokenized <- ignore(text1_tokenized)
  text2_tokenized <- ignore(text2_tokenized)

  # alignment and distances
  message(" - doing distance calculation and alignment")

  text1_tokenized <- setNames(text1_tokenized, c("from_1", "to_1", "token_1", "token_i_1"))
  text2_tokenized <- setNames(text2_tokenized, c("from_2", "to_2", "token_2", "token_2_1"))

  # distance
  a <-
    stringdist::amatch(
      text1_tokenized$token_1,
      text2_tokenized$token_2,
      method  = distance,
      ...
    )

  # alignment
  alignment <-
    data.frame(
      text1_tokenized,
      text2_tokenized[a, ]
    )

  alignment$distance <-
    stringdist::stringdist(
      alignment$token_1,
      alignment$token_2,
      method = distance
    )

  # type and distances
  alignment$type <- ""
  alignment$type[alignment$distance == 0]<-"no-change"
  alignment$type[alignment$distance >  0]<-"change"

  iffer <- is.na(alignment$token_1)
  alignment[iffer, "type"]     <- "insertion"
  alignment[iffer, "distance"] <- stringdist::stringdist("", alignment[iffer, "token_2"])

  iffer <- is.na(alignment$token_2)
  alignment[iffer, "type"]     <- "deletion"
  alignment[iffer, "distance"] <- stringdist::stringdist("", alignment[iffer, "token_1"])

  # non matches
  tmp <-
    subset(
      cbind(text1_tokenized, type="ignored"),
      !(text1_tokenized$token_i_1 %in% alignment$token_i_1)
    )
  alignment <-
    rtext:::rbind_fill(alignment, tmp)

  tmp <-
    subset(
      cbind(text2_tokenized, type="ignored"),
      !(text2_tokenized$token_i_2 %in% alignment$token_i_2)
    )
  alignment <-
    rtext:::rbind_fill(alignment, tmp)

  # return
  return(alignment)
}






















