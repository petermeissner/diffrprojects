if(getRversion() >= "2.15.1"){
  utils::globalVariables(
    c(
      "token_i_1", "token_i_2",
      "text1_tokenized", "text2_tokenized",
      "token", ".", "...", "res_token_i_1", "res_token_i_2",
      "min_dist_1"
    )
  )
}

#' algining texts
#'
#' Function aligns two texts side by side as a data.frame with change type and
#' distance given as well
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
#' @param verbose should function report on its doings via messages or not
#' @inheritParams stringdist::stringdist
#'
#' @return dataframe with tokens aligned according to distance
#'
#' @export
diff_align <- function(
  text1     = NULL,
  text2     = NULL,
  tokenizer = NULL,
  ignore    = NULL,
  clean     = NULL,
  distance  = c("lv", "osa", "dl", "hamming", "lcs", "qgram", "cosine", "jaccard", "jw", "soundex"),
  useBytes = FALSE,
  weight = c(d = 1, i = 1, s = 1, t = 1),
  maxDist = 0,
  q = 1,
  p = 0,
  nthread = getOption("sd_num_thread"),
  verbose = TRUE,
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
  if( length(text1) > 1){ text1 <- stringb::text_collapse(text1) }
  if( length(text2) > 1){ text2 <- stringb::text_collapse(text2) }
  distance <- distance[1]
  if(maxDist == 0){ maxDist <- 1e-150}

  # tokenize
  if( verbose ){ message(" - tokenizing text") }
  text1_tokenized <- tokenizer(text1)[1:3]
  text1_tokenized$token_i <- seq_along(text1_tokenized$token)

  text2_tokenized <- tokenizer(text2)[1:3]
  text2_tokenized$token_i <- seq_along(text2_tokenized$token)

  # clean
  if( verbose ){ message(" - cleaning token") }
  text1_tokenized_prec <- text1_tokenized
  text2_tokenized_prec <- text2_tokenized
  text1_tokenized$token <- clean(text1_tokenized$token)
  text2_tokenized$token <- clean(text2_tokenized$token)

  # ignore
  if( verbose ){ message(" - ignoring token") }
  text1_tokenized_prei <- text1_tokenized
  text2_tokenized_prei <- text2_tokenized
  text1_tokenized <- ignore(text1_tokenized)
  text2_tokenized <- ignore(text2_tokenized)

  # column naming
  text1_tokenized_prec <- stats::setNames(text1_tokenized_prec, c("from_1", "to_1", "token_1", "token_i_1"))
  text2_tokenized_prec <- stats::setNames(text2_tokenized_prec, c("from_2", "to_2", "token_2", "token_i_2"))
  text1_tokenized_prei <- stats::setNames(text1_tokenized_prei, c("from_1", "to_1", "token_1", "token_i_1"))
  text2_tokenized_prei <- stats::setNames(text2_tokenized_prei, c("from_2", "to_2", "token_2", "token_i_2"))
  text1_tokenized <- stats::setNames(text1_tokenized, c("from_1", "to_1", "token_1", "token_i_1"))
  text2_tokenized <- stats::setNames(text2_tokenized, c("from_2", "to_2", "token_2", "token_i_2"))

  # alignment and distances
  if( verbose ){ message(" - doing distance calculation and alignment") }

  # distance
  a <-
    stringdist::amatch(
      text1_tokenized$token_1,
      text2_tokenized$token_2,
      method  = distance,
      useBytes = useBytes,
      weight = weight,
      maxDist = maxDist,
      q = q,
      p = p,
      nthread = nthread,
      matchNA = FALSE
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
      method  = distance,
      useBytes = useBytes,
      weight = weight,
      q = q,
      p = p,
      nthread = nthread
    )

  # type and distances
  if( dim1(alignment) > 0 ){
    alignment$type <- ""
    alignment$type[alignment$distance == 0]<-"no-change"
    alignment$type[alignment$distance >  0]<-"change"

  alignment <-
    rtext:::rbind_fill(
      alignment,
      text1_tokenized[
        !(text1_tokenized$token_i_1 %in% alignment$token_i_1),
        ]
    )

  alignment <-
    rtext:::rbind_fill(
      alignment,
      text2_tokenized[
        !(text2_tokenized$token_i_2 %in% alignment$token_i_2),
        ]
    )

  iffer <- is.na(alignment$token_2)
  alignment[iffer, "type"]     <- "deletion"
  alignment[iffer, "distance"] <-
    stringdist::stringdist(
      "",
      alignment[iffer, "token_1"],
      method  = distance,
      useBytes = useBytes,
      weight = weight,
      q = q,
      p = p,
      nthread = nthread
    )

  iffer <- is.na(alignment$token_1)
  alignment[iffer, "type"]     <- "insertion"
  alignment[iffer, "distance"] <-
    stringdist::stringdist(
      "",
      alignment[iffer, "token_2"],
      method  = distance,
      useBytes = useBytes,
      weight = weight,
      q = q,
      p = p,
      nthread = nthread
    )

  }

  # non matches
  if( dim1(text1_tokenized_prei)>0 ){
    tmp <-
      subset(
        cbind(text1_tokenized_prei, type="ignored"),
        !(text1_tokenized_prei$token_i_1 %in% alignment$token_i_1)
      )
    alignment <-
      rtext:::rbind_fill(alignment, tmp)
  }

  if( dim1(text2_tokenized_prei)>0 ){
  tmp <-
    subset(
      cbind(text2_tokenized_prei, type="ignored"),
      !(text2_tokenized_prei$token_i_2 %in% alignment$token_i_2)
    )
  alignment <-
    rtext:::rbind_fill(alignment, tmp)
  }

  # original token
  if( dim1(alignment) > 0 ){
  alignment$token_1 <-
    dplyr::left_join(
      subset(alignment, select="token_i_1"),
      subset(text1_tokenized_prec, select=c("token_i_1", "token_1") ),
      by=c("token_i_1"="token_i_1")
    )$token_1

  alignment$token_2 <-
    dplyr::left_join(
      subset(alignment, TRUE, token_i_2),
      subset(text2_tokenized_prec, select=c("token_i_2", "token_2") ),
      by=c("token_i_2"="token_i_2")
    )$token_2
  }

  # column order and missing columns
  if( !("type" %in% names(alignment)) ){
    alignment <- cbind(alignment, type=character(0))
  }

  alignment <-
    subset(
      alignment,
      select=c(
        "token_i_1", "token_i_2", "distance", "type",
        "from_1", "to_1", "from_2", "to_2",
        "token_1",  "token_2"
      )
    )

  # return
  return(alignment)
}






















