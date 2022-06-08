setClass("ittyBitty",
         representation = representation(k="numeric", w="numeric"),
         prototype = list(k=numeric(),w=numeric()))

.ittyBitty.clean <- function(x) {
  # keep w between 1 and 0.5
  dw <- log2(x@w) + 1
  if(dw != 0) {
    x@k <- x@k + dw
    x@w <- x@w * 2^(-dw)
  }
  return(x)
}

#' make a new, clean ittyBitty
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

if(FALSE) {
.log.ittyBitty(aa <- .mult.ittyBitty.numeric(exp_to_ittyBitty(-40), 2))
.log.ittyBitty(aa <- exp_to_ittyBitty(-40) * 2)
.log.ittyBitty(aa <- 2*exp_to_ittyBitty(-40))
print.ittyBitty(aa)
.log.ittyBitty(bb <-  exp_to_ittyBitty(-40) + exp_to_ittyBitty(-40))

.log.ittyBitty(aa <- .mult.ittyBitty.ittyBitty(exp_to_ittyBitty(-40), exp_to_ittyBitty(-40)))


.log.ittyBitty(aa <- .mult.ittyBitty.numeric(exp_to_ittyBitty(-900),200))
900 + .log.ittyBitty( b <- .add.ittyBitty(a1 <- exp_to_ittyBitty(-900), a2 <- exp_to_ittyBitty(-910)))

require(Rmpfr)
900 + as.numeric(log(exp(mpfr(-900, 120)) + exp(mpfr(-910, 120))))

900 + .log.ittyBitty( b <- .add.ittyBitty(a1 <- exp_to_ittyBitty(-900), .mult.ittyBitty.numeric(exp_to_ittyBitty(-910),250)))
900 + as.numeric(log(exp(mpfr(-900, 120)) + 250*exp(mpfr(-910, 120))))

require(microbenchmark)
microbenchmark(
 .log.ittyBitty( b <- .add.ittyBitty(a1 <- exp_to_ittyBitty(-900), .mult.ittyBitty.numeric(exp_to_ittyBitty(-910),250))),
 as.numeric(log(exp(mpfr(-900, 32)) + 250*exp(mpfr(-910, 32))))
)

aseq <- -700 * 2^(-1:20)
bseq <- c(10, 20, 50)
K <- length(aseq) * length(bseq)
df <- data.frame(a=rep(0, K),
                 b=rep(0, K),
                 double=rep(0, K),
                 mpf120=rep(0, K),
                 mpf240=rep(0, K),
                 kr=rep(0, K),
                 diff = rep(0, K))
dfi <- 1
for(i in aseq) {
  for(j in bseq) {
    raw <- log(exp(i) + 1250*exp(i-j))
    kout <- .log.ittyBitty(.add.ittyBitty(exp_to_ittyBitty(i), .mult.ittyBitty.numeric(exp_to_ittyBitty(i-j),1250)))
    mpfrout <- as.numeric(log(exp(mpfr(i, 120)) + 1250*exp(mpfr(i-j, 120))))
    mpfrout240 <- as.numeric(log(exp(mpfr(i, 480)) + 1250*exp(mpfr(i-j, 480))))
    df[dfi,] <- c(i, j, i-raw, i - mpfrout, i - mpfrout240,
                  i - kout, NA)
    dfi <- dfi + 1
  }
}
df$true <- df$mpf240
df$true[64:nrow(df)] <- rep(df$true[61:63], length(64:nrow(df))/3)
df$diff <- (df$true - df$kr)/ pmax(abs(df$kr), .Machine$double.eps)
df
par(mfrow=c(3,3))
with(subset(df, b==10), plot(range(-1*a),range(kr, mpf240), log="x", type="n"))
with(subset(df, b==10), lines(-1*a,kr,col="blue"))
with(subset(df, b==10), lines(-1*a,mpf240, col="red"))

with(subset(df, b==20), plot(range(-1*a),range(kr, mpf240), log="x", type="n"))
with(subset(df, b==20), lines(-1*a,kr,col="blue"))
with(subset(df, b==20), lines(-1*a,mpf240, col="red"))

with(subset(df, b==50), plot(range(-1*a),range(kr, mpf240), log="x", type="n"))
with(subset(df, b==50), lines(-1*a,kr,col="blue"))
with(subset(df, b==50), lines(-1*a,mpf240, col="red"))



with(subset(df, b==10 & mpf240 < 100), plot(-1*a,kr-mpf240, log="x"))

with(subset(df, b==20 & mpf240 < 100), plot(-1*a,kr-mpf240, log="x"))

with(subset(df, b==50 & mpf240 < 100), plot(-1*a,kr-mpf240, log="x"))


with(subset(df, b==10 & mpf240 < 100), plot(-1*a,(kr-mpf240)/mpf240, log="x"))

with(subset(df, b==20 & mpf240 < 100), summary((kr-mpf240)/mpf240))
with(subset(df, b==20 & mpf240 < 100), plot(-1*a,(kr-mpf240)/mpf240, log="x"))

with(subset(df, b==50 & mpf240 < 100), summary((kr-mpf240)/mpf240))
with(subset(df, b==50 & mpf240 < 100), plot(-1*a,(kr-mpf240)/mpf240, log="x"))
}