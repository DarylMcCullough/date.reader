#' string.to.POSIXct
#' 
#' Convers character to POSIXct
#' 
#' @param x character; vector to convert to a POSIX date
#' 
#' @param orders character; name of the syntax for the values. If
#' this parameter is NA, then the function will try to guess the orders
#' for each value. Possible orders are: 
#' mdy, ymd, dmy
#' mdy_h, ymd_h, dmy_h
#' mdy_hm, ymd_hm, dmy_hm
#' mdy_hms, ymd_hms, dmy_hms
#' @param tz character; optional time zone
#' 
#'
#' @return 
#'   POSIXct vector (or NA, if a value cannot be parsed)
#'
#' @seealso 
#'   \code{\link[base]{as.POSIXct}} \cr
#'   \code{\link[base]{strptime}} for \code{format} strings
#'   
#' @examples 
#'   # -tk
#'  dts <- c( '20140210', '19791118', '19720329' )  
#'  string.to.POSIXct( dts ) 
#'      
#'  dts <- rep( '20140210', 10 )
#'  string.to.POSIXct( dts )  
#'   
#'  dts <- c( '14-02-10', '79-11-18' ) # FAIL(?)
#'  string.to.POSIXct( dts )  
#'  
#'  string.to.POSIXct( dts, orders='ymd' )
#'    
#' @export

string.to.POSIXct <- function( x, orders=NA, tz=NULL ) {
  if (is.null(tz)) {
    tz <- getOption("date.reader.tz", default="UTC")
  }
  if (is.na(orders)) {
    check.regex <- TRUE
  } else {
    check.regex <- FALSE
  }
  
  f <- function(txt) {
    if ( is.na(orders) ) {
      orders <- which.format(txt)
    }
    if ( is.na(orders) ) {
      return(NA)
    }
    result <- .parse.date(txt, orders, tz=tz, check.regex=check.regex)
    return(result)
  }
  x <- as.character(x)
  result <- lapply(x,f)
  result <- do.call(c,result)
  
  return(result)
}

#' as.POSIXct.character
#' 
#' Convers character to POSIXct
#' 
#' @param x character; vector to convert to a POSIX date
#' @param tz character; optional time zone
#' @param ... list; other optional arguments (from as.POSIXct, ignored)
#' 
#'
#' @return 
#'   POSIXct vector (or NA, if a value cannot be parsed)
#'
#' @seealso 
#'   \code{\link[base]{as.POSIXct}} \cr
#'   \code{\link[base]{strptime}} for \code{format} strings
#'   
#' @examples 
#'   # -tk
#'  dts <- c( '20140210', '19791118', '19720329' )  
#'  as.POSIXct( dts ) 
#'      
#'  dts <- rep( '20140210', 10 )
#'  as.POSIXct( dts )  
#'   
#'  dts <- c( '14-02-10', '79-11-18' ) # FAIL(?)
#'  as.POSIXct( dts )
#'    
#' @export
#' 

as.POSIXct.character <- function( x, tz="UTC", ...) {
  fmt <- which.format(x)
  if (is.na(fmt)) {
    return(NA)
  }
  return(string.to.POSIXct(x, tz=tz, orders=fmt))
}
