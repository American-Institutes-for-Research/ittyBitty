setClass("ittyBitty",
         slots = list(k="numeric", w="numeric"))

#' Setting subset and length methods
setMethod("[", "ittyBitty", function(x, i, j, ..., drop=TRUE) {
  initialize(x, k=x@k[i], w=x@w[i])
})

setMethod(length, "ittyBitty", function(x) length(x@k))

.ittyBitty.clean <- function(x) {
  # keep w between 1 and 0.5
  dw <- log2(x@w) + 1
  for(i in 1:length(dw)){
    if(dw[i] != 0) {
      x@k[i] <- x@k[i] + dw[i]
      x@w[i] <- x@w[i] * 2^(-dw[i])
    }
  }
  return(x)
}

#' make a new ittyBitty number
#' 
#' @k numeric, the exponent
#' @w numeric, the mantista
#' 
#' An \code{ittyBitty} is a representation of a numer as w * 2^k
#' 
#' A simply way to cast a number (e.g., \code{myNum})as an \code{ittyBitty} is to use \code{ittyBitty(k=1,w=myNum)}, 
#' The expected method of creating an \code{ittyBitty} is \code{exp_to_ittyBitty} which finds
#' \code{exp} of \code{x} as an \code{ittyBitty}. These can then be added together for numerical quadrature
#' and the results can be returned as a numeric with \code{log_as_numeric}. Doing this, far smaller
#' values can be represented than with doubles.
#' 
#' @importFrom methods new
#' @export
ittyBitty <- function(k=double(), w=double()) {
  kr <- new("ittyBitty", k=as.numeric(k), w=as.numeric(w))
  return(.ittyBitty.clean(kr))
}

#' @export
#' @method print ittyBitty
print.ittyBitty <- function(x, ...) {
  paste0("2^",x@k, " * ", x@w)
}

show.ittyBitty <- function(object) {
  print(object)
}

setMethod("show", "ittyBitty", show.ittyBitty)

setAs("ittyBitty", "numeric", function(from){
  return(2^from@k * from@w)
} )

# make a ittyBitty as exp(x)
#' @export
exp_to_ittyBitty <- function(x) {
  k <- ceiling(x/log(2))
  w <- exp(x-k*log(2))
  # already clean
  new("ittyBitty", k=k, w=w)
}  

# returns a numeric
.log.ittyBitty <- function(x) {
  log(x@w) + x@k * log(2)
}

#' @export
log_as_numeric <- function(x) {
  .log.ittyBitty(x)
}

.add.ittyBitty.ittyBitty <- function(x, y) {
  k3 <- 1 + max(x@k, y@k)
  w3 <- x@w/2^(k3-x@k) + y@w/2^(k3-y@k)
  return(.ittyBitty.clean(new("ittyBitty", k=k3, w=w3)))
}

.mult.ittyBitty.numeric <- function(x, y) {
  k2 <- x@k
  w2 <- y * x@w
  return(.ittyBitty.clean(new("ittyBitty", k=k2, w=w2)))
}

.add.ittyBitty.numeric <- function(x, y) {
  y <- ittyBitty(k=0, w=y)
  k2 <- x@k
  w2 <- y * x@w
  return(.ittyBitty.clean(new("ittyBitty", k=k2, w=w2)))
}

.mult.ittyBitty.ittyBitty <- function(x, y) {
  k3 <- x@k + y@k
  w3 <- x@w * y@w
  return(new("ittyBitty", k=k3, w=w3))
}

Arith.ittyBitty.ittyBitty <- function(e1, e2) {
  switch(.Generic,
    "+" = .add.ittyBitty.ittyBitty(e1, e2),
    "*" = .mult.ittyBitty.ittyBitty(e1, e2),
    stop("unknown method for ittyBitty:", .Generic)
  )
}

Arith.ittyBitty.numeric <- function(e1, e2) {
  switch(.Generic,
    "+" = .add.ittyBitty.numeric(e1, e2),
    "*" = .mult.ittyBitty.numeric(e1, e2),
    stop("unknown method for ittyBitty:",.Generic)
    )
}

Arith.numeric.ittyBitty <- function(e1, e2) {
  switch(.Generic,
    "+" = .add.ittyBitty.numeric(e2, e1),
    "*" = .mult.ittyBitty.numeric(e2, e1),
    stop("unknown method for ittyBitty:",.Generic)
    )
}

setMethod("Arith", signature(e1="ittyBitty", e2="ittyBitty"), Arith.ittyBitty.ittyBitty)
setMethod("Arith", signature(e1="ittyBitty", e2="numeric"), Arith.ittyBitty.numeric)
setMethod("Arith", signature(e1="numeric", e2="ittyBitty"), Arith.numeric.ittyBitty )

