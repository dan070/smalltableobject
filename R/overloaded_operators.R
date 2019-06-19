
#~~~~~~~~~~~~~~~~~~~~~~~~
# Overloading
# [ and [<- operators.
#~~~~~~~~~~~~~~~~~~~~~~~~
#' Subsetting operator for \code{smalltableobject}
#'
#' S3-method to enable square brackets on this object.
#'
#' @param o Object this operates on.
#' @param x First argument in []. Missing is valid.
#' @param y Second argument in []. Missing is valid.
#'
#' @return Subsetted \code{smalltableobject} based on x and y.
#' @export
#'
#' @examples
#' sto[1, ] # Show first row of table.
"[.smalltableobject" <- function(o, x, y) {
  tmp <-
    o$subset_read(
      x = x,
      y = y,
      Nargs = nargs() - 1,
      missingx = missing(x),
      missingy = missing(y)
    )
  return(tmp)
}



#'  @rdname add {Assignment operator for \code{smalltableobject}}
#'
#' S3-method to enable square bracket assignment on this object.
#'
#' @param o Object to operate on.
#' @param x First argument  \code{[x, ]}. Missing is valid.
#' @param y Second argument  \code{[, y]}. Missing is valid.
#' @param value Object or value sent in from \code{<-}
#'
#' @return Updated \code{smalltableobject} .
#' @export
#'
#' @examples
#' no examples
#'
"[<-.smalltableobject" <- function(o, x, y, value) {
  tmp <-
    o$subset_write(
      x = x,
      y = y,
      value = value,
      Nargs = nargs() - 2,
      missingx = missing(x),
      missingy = missing(y),
      missingvalue = missing(value)
    )
  return(tmp)
}


