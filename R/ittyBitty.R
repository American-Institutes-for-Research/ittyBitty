setClass("ittyBitty",
         slots = list(fraction="numeric",exponent="integer64"))

setMethod("initialize", "ittyBitty", function(.Object, exponent, ...) {
  .Object@exponent <- as.integer64(exponent)
  callNextMethod(.Object, ...)
})

#' subset methods
setMethod("[", "ittyBitty", function(x, i, j, ..., drop=TRUE) {
  initialize(x, exponent=x@exponent[i], fraction=x@fraction[i])
})

setMethod("[[", "ittyBitty", function(x, i, j, ...) {
  initialize(x, exponent=x@exponent[i], fraction=x@fraction[i])
})

# length method
setMethod(length, "ittyBitty", function(x) length(x@exponent))

#' Setting subset and length methods
setMethod("[<-", "ittyBitty", function(x, i, j, ..., value) {
  x@exponent[i] <- value@exponent
  x@fraction[i] <- value@fraction
  return(x)
})

.ittyBitty.clean <- function(x) {
  # keep fraction between 1 and 0.5
  suppressWarnings(dw <- 1 + ifelse(x@fraction == 0, 0, ifelse(x@fraction < 0, floor(log2(-x@fraction)), floor(log2(x@fraction)))))
  x@exponent <- if_else(dw != 0, x@exponent + dw, x@exponent)
  x@fraction <- ifelse(dw != 0, x@fraction * 2^(-dw), x@fraction)
  return(x)
}

#' make a new ittyBitty number
#' 
#' @param exponent integer64, the exponent
#' @param fraction numeric, the mantista
#' 
#' An \code{ittyBitty} is a representation of a number as fraction * 2^exponent
#' 
#' A simple way to cast a number (e.g., \code{myNum})as an \code{ittyBitty} is to use \code{ittyBitty(myNum)}, 
#' The expected method of creating an \code{ittyBitty} is \code{exp_to_ittyBitty} which finds
#' \code{exp} of \code{x} as an \code{ittyBitty}. These can then be added together for numerical quadrature
#' and the results can be returned as a numeric with \code{log_as_numeric}. Doing this, far smaller
#' values can be represented than with doubles.
#' 
#' @importFrom methods new
#' @export
ittyBitty <- function(fraction=double(),exponent=as.integer64(0)) {
  kr <- new("ittyBitty", fraction=as.numeric(fraction), exponent=exponent)
  return(.ittyBitty.clean(kr))
}

#' @export
#' @method print ittyBitty
print.ittyBitty <- function(x, ...) {
  print(as.character(x))
}

format.ittyBitty <- function(x, ...) {
  paste0("2^",x@exponent, " * ", x@fraction)
}

setMethod("show", "ittyBitty", function(object) print(object))

setAs("ittyBitty", "numeric", function(from){
  return(2^from@exponent * from@fraction)
} )

setMethod("as.numeric", "ittyBitty", function(x) {
  2^as.numeric(x@exponent) * x@fraction
})

setMethod("as.character", "ittyBitty", function(x) {
  paste0("2^",x@exponent, " * ", x@fraction)
})

# make a ittyBitty as exp(x)
#' @export
exp_to_ittyBitty <- function(x) {
  exponent <- ceiling(x/log(2))
  fraction <- exp(x-exponent*log(2))
  # already clean
  new("ittyBitty", exponent=exponent, fraction=fraction)
}  

# returns a numeric
.log.ittyBitty <- function(x) {
  log(x@fraction) + as.numeric(x@exponent) * log(2)
}

#' @export
log_as_numeric <- function(x) {
  .log.ittyBitty(x)
}

.add.ittyBitty.ittyBitty <- function(x, y) {
  k3 <- 1 + pmax(x@exponent, y@exponent)
  w3 <- x@fraction/2^(as.numeric(k3-x@exponent)) + y@fraction/2^(as.numeric(k3-y@exponent))
  return(.ittyBitty.clean(new("ittyBitty", exponent=k3, fraction=w3)))
}

.mult.ittyBitty.ittyBitty <- function(x, y) {
  k3 <- x@exponent + y@exponent
  w3 <- x@fraction * y@fraction
  return(.ittyBitty.clean(new("ittyBitty", exponent=k3, fraction=w3)))
}

Arith.ittyBitty.ittyBitty <- function(e1, e2) {
  switch(.Generic,
         "+" = .add.ittyBitty.ittyBitty(e1, e2),
         "*" = .mult.ittyBitty.ittyBitty(e1, e2),
         stop("unknown method for ittyBitty:", .Generic)
  )
}

Arith.ittyBitty.numeric <- function(e1, e2) {
  e2 <- ittyBitty(exponent=0,fraction=e2)
  switch(.Generic,
         "+" = .add.ittyBitty.ittyBitty(e1, e2),
         "*" = .mult.ittyBitty.ittyBitty(e1, e2),
         stop("unknown method for ittyBitty:",.Generic)
  )
}

Arith.numeric.ittyBitty <- function(e1, e2) {
  e1 <- ittyBitty(exponent=0, fraction=e1)
  switch(.Generic,
         "+" = .add.ittyBitty.ittyBitty(e1, e2),
         "*" = .mult.ittyBitty.ittyBitty(e1, e2),
         stop("unknown method for ittyBitty:",.Generic)
  )
}

setMethod("Arith", signature(e1="ittyBitty", e2="ittyBitty"), Arith.ittyBitty.ittyBitty)
setMethod("Arith", signature(e1="ittyBitty", e2="numeric"), Arith.ittyBitty.numeric)
setMethod("Arith", signature(e1="numeric", e2="ittyBitty"), Arith.numeric.ittyBitty )

Prod.ittyBitty <- function(x) {
  res <- ittyBitty(exponent=0, fraction=1)
  for(i in 1:length(x)) {
    res <- res * x[i]
  }
  return(res)
}

summary.ittyBitty <- function(x, ..., na.rm=FALSE) {
  switch(.Generic,
         "prod" = Prod.ittyBitty(x),
         stop("unknown method for ittyBitty:",.Generic)
  )
}

setMethod("Summary", "ittyBitty", summary.ittyBitty)

