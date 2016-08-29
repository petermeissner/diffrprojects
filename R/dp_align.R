
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
            link = stringb::text_c(t1, "_", t2)
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

    text_alignment_delete = function(){

    },


    #### [ text_alignment_code() ] ==============================================

    text_alignment_code = function(){

    }

    ) # closes public
  )# closes R6Class






































