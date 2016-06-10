































x <- c(1,2,3,4)

squared <- function(x){x^2}
exponential <- function(x){exp(x)}

squaredvector <- squared(x)
exponentialvector <- exponential(x)

plot(squaredvector, type = "o")
plot(exponentialvector, type = "o")

plot(c(1,2,4,8))
