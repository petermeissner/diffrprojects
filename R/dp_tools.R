#' function adding rtext objects to diffrprojects
#' @param self an object of class dp
#' @param rtext an object of class rtext
#' @param name an optional name for the text to stick to the text within the
#'        diffrproject corpus - if none is supplied the function will try to
#'        infere a reasonable name from the rtext$text_file field, if that is
#'        not given it will get the name noname_x where x is a running integer
#' @keywords internal
text_add_worker = function(self, rtext=NULL, name = NULL ){
  # input check
  stopifnot( "rtext"  %in% class(rtext) )
  # working variable creation
  names <- names(self$text)
  ids   <- vapply(self$text, `[[`, "", "id")
  id    <- rtext$id
  # doing-duty-to-do
  if( is.null(name) ){
    name <-
      tryCatch(
        basename(rtext$text_file), error=function(e){NA}
      )
    if( is.na(name) ){
      next_num <- max(c(as.numeric(text_extract(names, "\\d+")),0))+1
      name     <- text_c( "noname_", next_num)
    }
  }
  self$text[[name]]    <- rtext
  i <- 0
  while( rtext$id %in% ids ){
    rtext$id <- text_c(id, "_", i)
    i <- i+1
  }
}


#' function providing basic information on texts within diffrproject
#' @param dp a diffrproject object
#' @export
dp_text_base_data <- function(dp){
  df <- data.frame(NA)
  rt <- rtext$new("", verbose=FALSE)$info()
  names <- names(rt)
  for(i in seq_along(names) ){
    df[seq_along(dp$text), names[i]] <- NA
  }
  df <- df[,-1]
  for( i in seq_along(dp$text) ){
    df[i,] <- get("info", dp$text[[i]])()
  }
  df$name <- names(dp$text)
  if( all(is.na(df)) ){
    df <- subset(df, FALSE)
  }
  return(df)
}




#' as.data.frame method for for named lists of data.frames
#' @inheritParams base::as.data.frame
#' @param dfnamevar in which variable should list item names be saved
#' @method as.data.frame named_df_list
#' @export
as.data.frame.named_df_list <- function(x, row.names=NULL, optional=FALSE, dfnamevar="name", ...){
  if( any(unlist(lapply(x, class)) == "list") ){
    x <- lapply(x, as.data.frame.named_df_list)
  }
  # prepare variable
  each <- unlist(lapply(x, dim1))
  var <- names(x)
  var <- unlist(mapply(rep, var, each, SIMPLIFY=FALSE))
  # doing-duty-to-do
  if( class(x[[1]])!="data.frame" ){
    x<-as.data.frame(x)
  }else{
    names(x) <- NULL
    x <- do.call(rbind_fill, x)
    # add link variable
    x[[dfnamevar]] <- var
  }
  # return
  return(x)
}

#' as.data.frame method for for named lists of data.frames
#' @inheritParams as.data.frame.named_df_list
#' @method as.data.frame alignment_list
#' @export
as.data.frame.alignment_list <- function(x, row.names=NULL, optional=FALSE, ...){
  as.data.frame.named_df_list(
    x,
    row.names = row.names,
    optional = optional,
    dfnamevar = "link",
    ...
  )
}



#' as.data.frame method for for named lists of data.frames
#' @inheritParams as.data.frame.named_df_list
#' @method as.data.frame alignment_data_list
#' @export
as.data.frame.alignment_data_list <- function(x, row.names=NULL, optional=FALSE, ...){
  tmp <- as.data.frame.named_df_list(
    x,
    row.names = row.names,
    optional = optional,
    dfnamevar = "link",
    ...
  )
  cols <- which(names(tmp) %in% c("link", "alignment_i", "hl", "name"))
  val <- subset( tmp, select = -c(cols) )
  tmp <- subset( tmp, select = cols )
  tmp$val <- unlist(apply(val, 1, function(x){ x[!is.na(x)][1] } ))
  tmp
}


#' push char_data of one rtext objet to another
#'
#' Function that takes a rtext object pulls specific char_data from it and
#' pushes this information to another rtext object.
#'
#' Note, that this is an intelligent function.
#'
#' It will e.g. always decrease the hierarchy level (hl) found when pulling and
#' decrease it before pushing it forward therewith allowing that already present
#' coding might take priority over those pushed.
#'
#' Furthermore, the function will only push values if the pulled values are all
#' the same. Since, character index lengths that are used for pulling and
#' pushing might differ in length there is no straight forward rule to translate
#' non uniform value sequences in value sequnces of differing length. Note, that
#' of cause the values might differ between char_data variables but not within.
#' In case of non-uniformity the function will simply do nothing.
#'
#'
#' @param from_text text to pull data from
#' @param to_text text to push data to
#' @param from_token token of text to pull data from
#'        (e.g.: data.frame(from=1, to=4))
#' @param to_token token of text to push data to
#'        (e.g.: data.frame(from=1, to=4))
#' @param from_i index of characters to pull data from
#' @param to_i index of characters to push data to
#' @param x name of the char_data variable to pull and push -
#'        defaults to NULL which will result in cycling through all availible
#'        variables
#'
#' @return NULL
#' @export

push_text_char_data <-
  function(
    from_text  = NULL,
    to_text    = NULL,
    from_token = NULL,
    to_token   = NULL,
    from_i     = NULL,
    to_i       = NULL,
    x          = NULL,
    warn       = TRUE
  ){
    # check input
    stopifnot(
      !is.null(from_text), !is.null(to_text),
      !is.null(from_token) | is.null(from_i),
      !is.null(to_token) | is.null(to_i)
    )
    # prepare variables
    if( is.null(from_i) ){
      from_i <- stringb:::sequenize(from_token)
    }
    if( is.null(to_i) ){
      to_i <- stringb:::sequenize(to_token)
    }

    char_data <- from_text$get("char_data")

    if( is.null(x) ){
      from_names <- names(char_data)
    }else{
      from_names <- names(char_data)[names(char_data) %in% x]
    }

    # push data
    for(i in seq_along(from_names) ){
      iffer <- char_data[[from_names[i]]]$i %in% from_i
      name  <- from_names[i]
      value <-
        subset(
          char_data[[from_names[i]]],
          subset = iffer,
          select = name
        ) %>%
        unlist()

      if( length(unique(value)) ==1 ){
        to_text$char_data_set(
          x   = from_names[i],
          i   = to_i,
          val = value[1],
          hl  = min(char_data[[from_names[i]]]$hl)-1
        )
      }else if( warn & length(unique(value)) > 1 ){
          warning("push_text_char_data() pulled non uniform values, nothing pushed")
      }
    }
    # return
    return(invisible(NULL))
  }














