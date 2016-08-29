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
    x <- lapply(x, as.data.frame)
  }
  # prepare variable
  each <- unlist(lapply(x, dim1))
  var <- names(x)
  var <- unlist(mapply(rep, var, each, SIMPLIFY=FALSE))
  # doing-duty-to-do
  names(x) <- NULL
  x <- do.call(rbind_fill, x)
  # add link variable
  x[[dfnamevar]] <- var
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
