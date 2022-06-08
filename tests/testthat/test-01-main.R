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
