#' function writing numerous parts of table to database
#'
#' @param x parts to be written
#' @param table_name of the table
#' @param meta additional information to be attachesd to table parts
#' @param con connection to database
#'
#' @export
#'
write_numerous_parts_to_table <- function(x, con, table_name, meta=data.frame() ){
  for( i in seq_along(x) ){
    if(i==1){
      overwrite <- TRUE
      append    <- FALSE
    }else{
      overwrite <- FALSE
      append    <- TRUE
    }
    for(k in seq_len(ncol(meta)) ){
      x[[i]][[ names(meta)[k] ]] <- meta[ i, k ]
    }
    RSQLite::dbWriteTable(
      con,
      table_name,
      x[[i]],
      overwrite=overwrite,
      append=append
    )
  }
}

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
      next_num <- max(c(as.numeric(stringb::text_extract(names, "\\d+")),0))+1
      name     <- stringb::text_c( "noname_", next_num)
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
  if( is.null(var) ){ var <- ""}
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
  x <- x[ !unlist(lapply(x, is.null)) ]
  if( length(x) > 0 ){
    tmp <- as.data.frame.named_df_list(
      x,
      row.names = row.names,
      optional = optional,
      dfnamevar = "link",
      ...
    )
  }else{
    tmp <-
      data.frame("",1,1,"") %>%
      dplyr::filter(FALSE) %>%
      stats::setNames(c("link", "alignment_i", "hl", "name"))
  }
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
#' @param warn should function warn about non-uniform pull values (those will
#'        not be pushed to the other text)
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



#' transform rtext text data into a data.frame
#' @param x rtext object
#' @keywords internal
rtext_char_data_to_data_frame <- function(x){
  cd <- x$get("char_data")
  for( i in seq_along(cd) ){
    names(cd[[i]])[3] <- "value"
    cd[[i]]$value <- as.character(cd[[i]]$value)
    cd[[i]]$variable <- names(cd)[i]
  }
  df <- rbind_list(cd)
  return(df)
}

#' transform alignment_data list into data.frame
#' @param x alignment_data list
#' @keywords internal
alignment_data_to_data_frame <- function(x){
  for( i in seq_along(x) ){
    names(x[[i]])[3] <- "value"
    x[[i]]$value <- as.character(x[[i]]$value)
    x[[i]]$variable <- names(x)[i]
  }
  df <- rbind_list(x)
  return(df)
}




#' function sorting alignment data according to token index
#'
#' @param x data.frame to be sorted
#' @param ti1 either NULL (default): first column of x is used as first token
#'        index for sorting; a character vector specifying the column to be used
#'        as first token index; or a numeric vector of length nrow(x) to be use
#'        as first token index
#' @param ti2 either NULL (default): second column of x is used as second token
#'        index for sorting; a character vector specifying the column to be used
#'        as second token index; or a numeric vector of length nrow(x) to be use
#'        as second token index
#' @param first should first text or second text be given priority
#'
#' @export
#'
sort_alignment <- function(x, ti1 = NULL, ti2 = NULL, first = TRUE){
  # processing input
  if( is.null(ti1) ){
    ti1 <- x[,1]
  }else if( is.numeric(ti1) ){
    ti1 <- ti1
  }else if( is.character(ti1) ){
    ti1 <- x[, ti1]
  }

  if( is.null(ti2) ){
    ti2 <- x[,2]
  }else if( is.numeric(ti2) ){
    ti2 <- ti2
  }else if( is.character(ti2) ){
    ti2 <- x[, ti2]
  }

  # preparing loop
  if ( first == T  ){
    var1 <- ti1
    var2 <- ti2
  }else{
    var1 <- ti2
    var2 <- ti1
  }

  looper   <- seq_len(max(ti1, ti2, na.rm=T))
  data_nr  <- seq_along(x[,1])
  sorter <- NULL

  # loop
  for ( i in looper ){
    sorter <- c(  sorter                                            ,
                  data_nr[ i==var1 & !is.na(var1) &   is.na(var2) ] ,
                  data_nr[ i==var1 & !is.na(var1) &  !is.na(var2) ] ,
                  data_nr[ i==var2 &  is.na(var1) &  !is.na(var2) ] )
  }
  # return
  return(x[sorter,])
}




























