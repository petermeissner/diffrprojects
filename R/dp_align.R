
#' class for dp_align
#'
#' @docType class
#'
#' @name diffrproject
#'
#' @export
#'
#' @keywords data
#'
#' @return Object of \code{\link{dp_align}}
#'
#' @format \code{\link{R6Class}} object.
#'
#'
dp_align <-
  R6::R6Class(

    #### class name ============================================================
    classname    = "dp_align",

    #### misc ====================================================================
    active       = NULL,
    inherit      = dp_base,
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
          t1=NULL, t2=NULL,
          tokenizer = NULL, ignore = NULL, clean = NULL,
          distance = c("lv", "osa", "dl", "hamming", "lcs", "qgram", "cosine", "jaccard", "jw", "soundex"),
          useBytes = FALSE,
          weight = c(d = 1, i = 1, s = 1, t = 1),
          maxDist = 0, q = 1, p = 0,
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

          self$text_alignment_add(
            alignment,
            link = stringb::text_c(t1, "~", t2)
          )
        }
        # return
        return(invisible(self))
      },


    #### [ text_alignment_add() ] ==============================================

    text_alignment_add = function(x, link){

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


    #### [ text_alignment_delete() ] ==============================================

    text_alignment_delete =
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
          self$text_alignment_delete(
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

          iffer <- iffer %>% lapply(f) %>%  as.data.frame() %>% apply(1,g)

        self$alignment[[link]] <- self$alignment[[link]][!iffer, ]
      }

      # return
      return(invisible(self))
    },


    #### [ text_alignment_code() ] ==============================================

    text_alignment_code =
      function(
        link=NULL, alignment_i=NULL,
        from_1=NULL, to_1=NULL,
        from_2=NULL, to_2=NULL,
        type=NULL
      ){

    },

    #### [ text_alignment_set ] #### ................................................
    text_alignment_data_set = function(
      link=NULL, alignment_i=NULL, x=NULL, val=NA, hl = 0
    ){
      # check input
      stopifnot( length(x) == 1 )
      stopifnot( x != c("alignment_i", "link", "hl", "x") )
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
    }

    ) # closes public
  )# closes R6Class






































