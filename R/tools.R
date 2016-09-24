#' accessing private from R6 object
#'
#' @param x R6 object to access private from
#'
#' @source http://stackoverflow.com/a/38578080/1144966
#'
#' @export
#'
get_private <- function(x) {
  x[['.__enclos_env__']]$private
}


#' which are minima in vector
#' @param x vector to check
#' @param unique defaults to false
#' @keywords internal
is_minimum <- function(x, unique=FALSE){
  if(unique){
    return(
      min(x) == x & !duplicated(x)
    )
  }else{
    return(
      min(x) == x
    )
  }
}

#' checking if value is uniqe in set
#' @param x vector to check
#' @keywords internal
is_unique <- function(x){
  tmp <- !is_duplicate(x)
  tmp[is.na(x)] <- NA
  tmp
}

#' checking if value is duplicated in set
#' @param x vector to check
#' @keywords internal
is_duplicate <- function(x){
  x %in% x[duplicated(x)]
}

#' extract specific item from each list element
#' @param l list
#' @param item name or index of item to extract
#' @param unlist defaults to TRUE, whether to unlist results or leave as list
#' @keywords internal
get_list_item <- function(l, item, unlist=TRUE){
  tmp <-
    lapply(
      l,
      function(x, item){
        tryCatch(
          x[[item]],
          error = function(e){NULL}
        )
      },
      item
    )
  index <- vapply(tmp, is.null, TRUE)
  tmp[index] <- NA
  if( unlist ){
    return(unlist(tmp))
  }else{
    return(tmp)
  }
}



#' function rbinding list elements
#' @param l list
#' @keywords internal
rbind_list <- function(l){
  tmp <- do.call(rbind, l)
  rownames(tmp) <- NULL
  as.data.frame(tmp, stringsAsFactors = FALSE)
}


#' function that shifts vector values to right or left
#'
#' @param x Vector for which to shift values
#' @param n Number of places to be shifted.
#'    Positive numbers will shift to the right by default.
#'    Negative numbers will shift to the left by default.
#'    The direction can be inverted by the invert parameter.
#' @param default The value that should be inserted by default.
#' @param invert Whether or not the default shift directions
#'    should be inverted.
#' @keywords internal

shift <- function(x, n=0, default=NA, invert=FALSE){
  n <-
    switch (
      as.character(n),
      right    =  1,
      left     = -1,
      forward  =  1,
      backward = -1,
      lag      =  1,
      lead     = -1,
      as.numeric(n)
    )
  if( length(x) <= abs(n) ){
    if(n < 0){
      n <- -1 * length(x)
    }else{
      n <- length(x)
    }
  }
  if(n==0){
    return(x)
  }
  n <- ifelse(invert, n*(-1), n)
  if(n<0){
    n <- abs(n)
    forward=FALSE
  }else{
    forward=TRUE
  }
  if(forward){
    return(c(rep(default, n), x[seq_len(length(x)-n)]))
  }
  if(!forward){
    return(c(x[seq_len(length(x)-n)+n], rep(default, n)))
  }
}


#' function forcing value to fall between min and max
#' @param x the values to be bound
#' @param max upper boundary
#' @param min lower boundary
#' @keywords internal
bind_between <- function(x, min, max){
  x[x<min] <- min
  x[x>max] <- max
  return(x)
}


#' function for binding data.frames even if names do not match
#' @param df1 first data.frame to rbind
#' @param df2 second data.frame to rbind
#' @keywords internal
rbind_fill <- function(df1=data.frame(), df2=data.frame()){

  # get union of names
  names_df <- c(names(df1), names(df2))

  # prepare empty data.frame
  empty_frame <- data.frame(lapply(names_df, as.data.frame))
  names(empty_frame) <- names_df
  if(length(names_df)>0){
    empty_frame <- subset(empty_frame, FALSE)
  }

  # filling up
  if( dim1(df1) > 0 ){
    df1[, names_df[!(names_df %in% names(df1))]] <- rep(NA, dim1(df1))
  }else{
    df1 <- empty_frame
  }

  if( dim1(df2) > 0 ){
    df2[, names_df[!(names_df %in% names(df2))]] <- rep(NA, dim1(df2))
  }else{
    df2 <- empty_frame
  }

  # doing-duty-to-do
  rbind(df1, df2)
}




#' function that checks is values are in between values
#' @param x input vector
#' @param y lower bound
#' @param z upper bound
#' @keywords internal
is_between <- function(x,y,z){
  return(x>=y & x<=z)
}


#' function that extracts elements from vector
#'
#' @param vec the chars field
#' @param length number of elements to be returned
#' @param from first element to be returned
#' @param to last element to be returned
#' @keywords internal
get_vector_element <-
  function(vec, length=NULL, from=NULL, to=NULL){
    # helper functions
    bind_to_vecrange <- function(x){bind_between(x, 1, length(vec))}
    bind_length       <- function(x){bind_between(x, 0, length(vec))}
    return_from_to    <- function(from, to, split){
      res  <- vec[seq(from=from, to=to)]
      return(res)
    }
    # only length
    if( !is.null(length) & ( is.null(from) & is.null(to) ) ){
      length <- max(0, min(length, length(vec)))
      length <- bind_length(length)
      if(length==0){
        return("")
      }
      from   <- 1
      to     <- length
      return(return_from_to(from, to, split))
    }
    # from and to (--> ignores length argument)
    if( !is.null(from) & !is.null(to) ){
      from <- bind_to_vecrange(from)
      to   <- bind_to_vecrange(to)
      return(return_from_to(from, to, split))
    }
    # length + from
    if( !is.null(length) & !is.null(from) ){
      if( length<=0 | from + length <=0 ){
        return("")
      }
      to   <- from + length-1
      if((to < 1 & from < 1) | (to > length(vec) & from > length(vec) )){
        return("")
      }
      to   <- bind_to_vecrange(to)
      from <- bind_to_vecrange(from)
      return(return_from_to(from, to, split))
    }
    # length + to
    if( !is.null(length) & !is.null(to) ){
      if( length<=0 | to - (length-1) > length(vec) ){
        return("")
      }
      from <- to - length + 1
      if((to < 1 & from < 1) | (to > length(vec) & from > length(vec) )){
        return("")
      }
      from <- bind_to_vecrange(from)
      to   <- bind_to_vecrange(to)
      return(return_from_to(from, to, split))
    }
    stop("get_vector_element() : I do not know how to make sense of given length, from, to argument values passed")
  }



#' get first dimension or length of object
#' @param x object, matrix, vector, data.frame, ...
#' @keywords internal
dim1 <- function(x){
  ifelse(is.null(dim(x)[1]), length(x), dim(x)[1])
}


#' get first dimension or length of object
#' @param x object, matrix, vector, data.frame, ...
#' @keywords internal
dim2 <- function(x){
  dim(x)[2]
}


#' seq along first dimension / length
#' @param x x
#' @keywords internal
seq_dim1 <- function(x){
  seq_len(dim1(x))
}





#' function giving back the mode

#' @param x vector to get mode for
#' @param multimodal wether or not all modes should be returned in case of more than one
#' @param warn should the function warn about multimodal outcomes?
#' @keywords internal
modus <- function(x, multimodal=FALSE, warn=TRUE) {
  x_unique <- unique(x)
  tab_x    <- tabulate(match(x, x_unique))
  res      <- x_unique[which(tab_x==max(tab_x))]
  if( identical(multimodal, TRUE) ){
    return(res)
  }else{
    if( warn & length(res) > 1 ){
      warning("modus : multimodal but only one value returned (use warn=FALSE to turn this off)")
    }
    if( !identical(multimodal, FALSE) & length(res) > 1 ){
      return(multimodal)
    }else{
      return(res[1])
    }
  }
}





#' function to get classes from e.g. lists

#' @param x list to get classes for
#' @keywords internal
classes <- function(x){
  tmp <- lapply(x, class)
  data.frame(name=names(tmp), class=unlist(tmp) , row.names = NULL)
}








#' function to sort df by variables
#' @param df data.frame to be sorted
#' @param ... column names to use for sorting
#' @keywords internal
dp_arrange <- function(df, ...){
  sorters    <- as.character(as.list(match.call()))
  if( length(sorters)>2 ){
    sorters    <- sorters[-c(1:2)]
    sorters    <- paste0("df['",sorters,"']", collapse = ", ")
    order_call <- paste0("order(",sorters,")")
    res        <- df[eval(parse(text=order_call)), ]
    if( is.data.frame(df) & !is.data.frame(res) ){
      res <- as.data.frame(res)
      names(res) <- names(df)
    }
    return(res)
  }else{
    return(df)
  }
}























