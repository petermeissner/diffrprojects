
#' class for dp_align
#'
#' @docType class
#'
#' @name dp_align
#'
#' @export
#'
#' @keywords data
#'
#' @return Object of \code{\link{dp_align}}
#'
#' @format \code{\link{R6Class}} object.
#'
#' @seealso \code{\link{diffrproject}}
#'
dp_align <-
  R6::R6Class(

    #### class name ============================================================
    classname    = "dp_align",

    #### misc ====================================================================
    active       = NULL,
    inherit      = dp_export,
    lock_objects = TRUE,
    class        = TRUE,
    portable     = TRUE,
    lock_class   = FALSE,
    cloneable    = TRUE,
    parent_env   = asNamespace('diffrprojects'),

    #### public ================================================================
    public = list(

      #### data ================================================================

      #### methods =============================================================


      #### [ text_align() ] ====================================================

      text_align = function(
          t1=NULL,
          t2=NULL,
          tokenizer = NULL,
          ignore = NULL,
          clean = NULL,
          distance = c("lv", "osa", "dl", "hamming", "lcs", "qgram", "cosine", "jaccard", "jw", "soundex"),
          useBytes = FALSE,
          weight = c(d = 1, i = 1, s = 1, t = 1),
          maxDist = 0,
          q = 1,
          p = 0,
          nthread = getOption("sd_num_thread"),
          verbose = self$options$verbose,
          ...
        ){

        if( is.null(t1) & is.null(t2) ){
          # check again
          # if(interactive() & self$options$ask){
          #   y <- readline("Alignment for ALL files? \nyes / no : ")
          #   if( !any(grepl("y", y)) ){
          #     return(FALSE)
          #   }
          # }
          for(i in seq_along(self$link) ){
            self$text_align(
              self$link[[i]]$from,
              self$link[[i]]$to,
              tokenizer = tokenizer,
              ignore = ignore,
              clean=clean,
              distance=distance,
              useBytes = useBytes,
              weight=weight,
              maxDist = maxDist,
              q=q,
              p=p,
              nthread = nthread,
              verbose = verbose,
              ...
            )
          }
        }else{
          self$message("- doing alignment")

          tt1 <-
            self$text[[t1]]$text_get()
          tt2 <-
            self$text[[t2]]$text_get()

          alignment <-
            diff_align(
              tt1, tt2,
              tokenizer = tokenizer,
              ignore = ignore,
              clean=clean,
              distance=distance,
              useBytes = useBytes,
              weight=weight,
              maxDist = maxDist,
              q=q,
              p=p,
              nthread = nthread,
              verbose = verbose,
              ...
            )

          self$alignment_add(
            alignment,
            link = stringb::text_c(t1, "~", t2)
          )
        }
        # return
        return(invisible(self))
      },


    #### [ alignment_add() ] ==============================================

    alignment_add = function(x, link){

      # fetching link name if necessary
      if( !is.character(link) ){
        link <- names(self$link)[link]
      }

      # alignment_i
      alignment_i <- self$alignment[[link]]$alignment_i
      if( length(alignment_i) > 0){
        max_a <- max(alignment_i)
        alignment_i <-
          seq_len( max(alignment_i) )[ !(seq_len( max(alignment_i)) %in% alignment_i)]
      }else{
        max_a <- 0
      }
      x$alignment_i <-
        c( alignment_i, seq_len( dim1(x) - dim1(alignment_i)) + max_a )

      selection <-
        c(
          "alignment_i",
          "token_i_1", "token_i_2",
          "distance", "type",
          "from_1", "to_1",
          "from_2", "to_2"
        )
      x <- subset(x, select = selection[selection %in% names(x)] )

      # adding alignments
      self$alignment[[link]]
      tmp <-
        rbind_fill(
          self$alignment[[link]],
          x
        )
      self$alignment[[link]] <-
        subset(
          tmp,
          !duplicated(
            subset(tmp, select=c(from_1, to_1, from_2, to_2))
          )
        )

      # return for piping
      return(invisible(self))
    },


    #### [ alignment_delete() ] ==============================================

    alignment_delete =
      function(
        link=NULL, alignment_i=NULL, from_1=NULL, to_1=NULL, from_2=NULL, to_2=NULL, type=NULL
      ){

      # check input
      stopifnot( !is.null(link) )
      stopifnot(
        !is.null(alignment_i) |
        !is.null(from_1) | !is.null(to_1) |
        !is.null(from_2) | !is.null(to_2) |
        !is.null(type)
      )

      # recursion
      if( length(link)>1 ){
        for(i in seq_along(link)){
          self$alignment_delete(
            link        = link[i],
            alignment_i = alignment_i,
            from_1      = from_1,
            to_1        = to_1,
            from_2      = from_2,
            to_2        = to_2,
            type        = type
          )
        }
      }else{ # no recursion

        if( !is.null(alignment_i) & (!is.null(from_1) | !is.null(to_1) | !is.null(from_2) | !is.null(to_2)) ){
            self$warning("alignment_i and other arguments supplied - I cannot use bot groups at the same time - I will discard the others and carry on")
          }

        # fetching link name if necessary
        if( !is.character(link) ){
          link <- names(self$link)[link]
        }

        # finish because link does not exist
        if(is.null(link)){
          self$warning("link not found")
          return(invisible(self))
        }

        # doing-duty-to-do
        iffer <- list()
        if( !is.null(alignment_i)){
          iffer[[1]] <- self$alignment[[link]]$alignment_i %in% alignment_i
        }
          iffer[[2]] <- self$alignment[[link]]$from_1 <= from_1
          iffer[[3]] <- self$alignment[[link]]$to_1   >= to_1
          iffer[[4]] <- self$alignment[[link]]$from_2 <= from_2
          iffer[[5]] <- self$alignment[[link]]$to_2   >= to_2
          iffer[[6]] <- as.character(self$alignment[[link]]$type)   == type

          f <- function(x){ if( length(x) == 0 ){ x<-rep(NA, dim1(self$alignment[[link]])) }; return(x) }
          g <- function(x){
            if( all( is.na(x) ) ){
              return(FALSE)
            }
            if( all( is.na(x) | x ) ){
              return(TRUE)
            }
            FALSE
          }

        iffer  <- iffer %>% lapply(f) %>%  as.data.frame() %>% apply(1,g)
        wiffer <- self$alignment[[link]][!iffer, ]$alignment_i


        # update alignment_data
        for(i in seq_along(self$alignment_data[[link]]) ){
          iffer_tmp <- self$alignment_data[[link]][[i]]$alignment_i %in% wiffer
          self$alignment_data[[link]][[i]] <- self$alignment_data[[link]][[i]][iffer_tmp,]
        }

        # update alignments
        self$alignment[[link]] <- self$alignment[[link]][!iffer, ]
      }

      # update hashes
      private$hash("alignment")

      # return
      return(invisible(self))
    },


    #### [ alignment_code() ] ==============================================

    alignment_code =
      function(
        link=NULL, alignment_i=NULL, x=NULL, val=NA, hl = 0,
        pattern=NULL, pattern1=NULL, pattern2=NULL, invert=FALSE,
        from_1=NULL, to_1=NULL,
        from_2=NULL, to_2=NULL,
        type=NULL
      ){
        # check inputs
        stopifnot(!is.null(link), !is.null(x))

        # fetching link name if necessary
        if( !is.character(link) ){
          link <- names(self$link)[link]
        }

        # doing-duty-to-do
        iffer <- list()
        if( !is.null(alignment_i)){
          iffer[[1]] <- self$alignment[[link]]$alignment_i %in% alignment_i
        }
        iffer[[2]] <- self$alignment[[link]]$from_1 <= from_1
        iffer[[3]] <- self$alignment[[link]]$to_1   >= to_1
        iffer[[4]] <- self$alignment[[link]]$from_2 <= from_2
        iffer[[5]] <- self$alignment[[link]]$to_2   >= to_2
        iffer[[6]] <- as.character(self$alignment[[link]]$type)   == type

        if( !is.null(pattern) ){
          token_1 <-
            text_sub(
              self$text[[self$link[[link]]$from]]$text_get(),
              self$alignment[[link]]$from_1,
              self$alignment[[link]]$to_1
            )
          token_2 <-
            text_sub(
              self$text[[self$link[[link]]$to  ]]$text_get(),
              self$alignment[[link]]$from_2,
              self$alignment[[link]]$to_2
            )
          iffer[[7]] <-
            stringb::text_detect(token_1, pattern) |
            stringb::text_detect(token_2, pattern)
        }

        if( !is.null(pattern1) ){
          token_1 <-
            text_sub(
              self$text[[self$link[[link]]$from]]$text_get(),
              self$alignment[[link]]$from_1,
              self$alignment[[link]]$to_1
            )
          iffer[[8]] <-
            stringb::text_detect(token_1, pattern1)
        }

        if( !is.null(pattern2) ){
          token_2 <-
            text_sub(
              self$text[[self$link[[link]]$to  ]]$text_get(),
              self$alignment[[link]]$from_2,
              self$alignment[[link]]$to_2
            )
          iffer[[9]] <-
            stringb::text_detect(token_2, pattern2)
        }

        # combining iffer
        f <- function(x){ if( length(x) == 0 ){ x<-rep(NA, dim1(self$alignment[[link]])) }; return(x) }
        g <- function(x){
          if( all( is.na(x) ) ){
            return(FALSE)
          }
          if( all( is.na(x) | x ) ){
            return(TRUE)
          }
          FALSE
        }

        iffer  <- iffer %>% lapply(f) %>%  as.data.frame() %>% apply(1,g)
        if( invert ){
          wiffer <- self$alignment[[link]][!iffer, ]$alignment_i
        }else{
          wiffer <- self$alignment[[link]][iffer, ]$alignment_i
        }

        # setting values
        self$alignment_data_set(
          link        = link,
          alignment_i = wiffer,
          val         = val,
          x           = x,
          hl          = hl
        )

        # return
        return(invisible(self))
    },

    #### [ alignment_set ] #### ................................................
    alignment_data_set = function(
      link=NULL, alignment_i=NULL, x=NULL, val=NA, hl = 0
    ){
      # check input
      stopifnot( length(x) == 1 )
      if( any(x == c("alignment_i", "link", "hl", "x")) ){
        stop("Reserved names used: alignment_i, link, hl, and x are reserved names - use another name!")
      }
      if( is.null(x) | is.null(alignment_i) | is.null(link) ){
        warning("char_data_set : no sufficient information passed for x, i - nothing coded")
        return(invisible(self))
      }
      if(
        any(
          alignment_i > max(self$alignment[[link]]$alignment_i) |
          any( alignment_i < 1)
        )
      ){
        stop("text_alignement_set : alignment_i out of bounds")
      }

      # fetching link name if necessary
      if( !is.character(link) ){
        link <- names(self$link)[link]
      }

      # prepare input
      if( length(val)==1 ){
        val <- rep(val, length(alignment_i))
      }
      if( length(hl)==1 ){
        hl <- rep(hl, length(alignment_i))
      }
      # check for coresponding lengths
      stopifnot( length(alignment_i) == length(val) & length(val) == length(hl) )

      # make sure there is a data frame to fill
      if( is.null(self$alignment_data[[link]][[x]] ) ){
        self$alignment_data[[link]][[x]] <-
          subset(
            data.frame(
              alignment_i    = 1L,
              hl   = 0
            ),
            FALSE
          )
      }

      # split data
      # - new i in old i and level is less or equal to new level
      # -> already coded with lower level are discarded!
      i_in_data  <-
        merge(
          data.frame(alignment_i=alignment_i),
          subset(self$alignment_data[[link]][[x]], select=c("alignment_i", "hl")),
          all.x = TRUE,
          by="alignment_i"
        )$hl <= hl
      i_in_data[is.na(i_in_data)] <- FALSE


      # - adding those not already coded
      i_notin_data     <- !(alignment_i %in% self$alignment_data[[link]][[x]]$alignment_i)

      # assign data with i already in i
      input_to_data_matcher <-
        match(alignment_i[i_in_data], self$alignment_data[[link]][[x]]$alignment_i)

      self$alignment_data[[link]][[x]][input_to_data_matcher, "alignment_i"] <-
        alignment_i[i_in_data]

      self$alignment_data[[link]][[x]][input_to_data_matcher, "hl"]   <-
        hl[i_in_data]

      self$alignment_data[[link]][[x]][input_to_data_matcher, x]   <-
        val[i_in_data]

      # code for i not already in char_data
      add_df <-
        data.frame(
          alignment_i  = alignment_i[i_notin_data],
          hl = hl[i_notin_data]
        )

      add_df[[x]] <-
        val[i_notin_data]

      self$alignment_data[[link]][[x]] <-
        rbind_fill(
          self$alignment_data[[link]][[x]],
          add_df
        ) %>%
        dp_arrange("alignment_i")

      # necessary updates
      private$hash("alignment_data")

      # return for piping
      return(invisible(self))
    },


    #### [ text_code_alignment_token() ] =====================================================

    text_code_alignment_token = function(link=NULL, alignment_i=NULL, text1=FALSE, text2=FALSE, x=NULL, val=NA, hl=0, ...){
      # fetching link name if necessary
      if( !is.character(link) ){
        link <- names(self$link)[link]
      }


      iffer <- self$alignment[[link]]$alignment_i %in% alignment_i
      tbc <-
        self$alignment[[link]] %>%
          subset(
            subset = iffer,
            select = c("from_1","to_1", "from_2", "to_2")
          )

      l <- dim1(tbc)
      if( l != length(val) ){ val <- rep(val, l)[seq_len(l)] }
      if( l != length(hl ) ){ hl  <- rep(hl,  l)[seq_len(l)] }

      tbc$val <- val
      tbc$hl  <- hl
      tbc_split <- split(tbc, seq_dim1(tbc))

       if( text1 ){
         res <-
           do.call(
              rbind,
              lapply(tbc_split, function(x){
                if( !is.na(x$from_1) & !is.na(x$to_1) ){
                  res <-
                    data.frame(
                    i   = seq(x$from_1, x$to_1),
                    val = x$val,
                    hl  = x$hl
                  )
                }else{
                  res <- subset(data.frame(i=0,val=NA,hl=0), FALSE)
                }
              return(res)
            })
           )

         self$text_code(self$link[[link]]$from, x=x, i=res$i, val=res$val, hl=res$hl)
      }

      if( text2 ){
        res <-
          do.call(
            rbind,
            lapply(tbc_split, function(x){
              if( !is.na(x$from_2) & !is.na(x$to_2) ){
                res <-
                  data.frame(
                    i   = seq(x$from_2, x$to_2),
                    val = x$val,
                    hl  = x$hl
                  )
              }else{
                res <- subset(data.frame(i=0,val=NA,hl=0), FALSE)
              }
              return(res)
            })
          )

        self$text_code(self$link[[link]]$to, x=x, i=res$i, val=res$val, hl=res$hl)
      }
      return(invisible(self))
    },



    #### [ alignment_data_full ] #### ................................................
    alignment_data_full = function(link=NULL, data_only=TRUE){


      # fetching link name if necessary
      if( is.null(link) ){
        link <- seq_along(self$link)
      }
      if( !is.character(link) ){
        link <- names(self$link)[link]
      }

      if(data_only){
        tmp <-
          self$alignment %>%
          as.data.frame() %>%
          dplyr::right_join(as.data.frame(self$alignment_data))
      }else{
        tmp <-
          self$alignment %>%
          as.data.frame() %>%
          dplyr::left_join(as.data.frame(self$alignment_data))
      }
      tmp <-
        tmp %>%
        dplyr::rename(
          var_name = name,
          var_value = val
        ) %>%
          dplyr::left_join(as.data.frame(self$link)) %>%
          dplyr::rename(
            text_from = from,
            text_to = to
          )

      for( i in seq_along(unique(tmp$text_from)) ){
        tf    <- unique(tmp$text_from)[i]
        iffer <- tmp$text_from == tf
        tmp[iffer, "token_1"] <-
          self$text[[tf]]$text_get() %>%
          stringb::text_sub(tmp$from_1[iffer],tmp$to_1[iffer])
      }
      for( i in seq_along(unique(tmp$text_to)) ){
        tf    <- unique(tmp$text_to)[i]
        iffer <- tmp$text_to == tf
        tmp[iffer, "token_2"] <-
          self$text[[tf]]$text_get() %>%
          stringb::text_sub(tmp$from_2[iffer],tmp$to_2[iffer])
      }

      if( !("token_2" %in% names(tmp)) ){
        tmp$token_1 <- rep(NA, nrow(tmp))
        tmp$token_2 <- rep(NA, nrow(tmp))
      }

      tmp <-
        dplyr::select(tmp, link, alignment_i, type, distance, alignment_i:token_2)

      # return
      return(tmp)
    }



    ) # closes public
  )# closes R6Class






































