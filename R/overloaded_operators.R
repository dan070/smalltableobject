
#~~~~~~~~~~~~~~~~~~~~~~~~
# Overloading
# [ and [<- operators.
#~~~~~~~~~~~~~~~~~~~~~~~~
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
