##########################################
#
# Compute exponential model (cf. Melnick et al., 2013)
#

# 1. Create some x and y values -----------------------------------------------
# (these are values from Duje Tadin's Email, 11/02/16)
x <- c(1.8, 3.6, 7.2)
y <- c(36.03355284, 45.90567406, 82.96282295)
plot(x,y)

# 2. Define exponential function ---------------------------------------------
expFunction <- function(x,intercept,slope){I(intercept*exp(slope*x))}

# 3. Fit the model to the data -----------------------------------------------
nlsFit <- nls(y ~ expFunction(x, intercept, slope), start = list(intercept = 20, slope = 0))
summary(nlsFit)
lines(x, predict(nlsFit, list(x = x)), col = "red")

# 4. Compute R-squared  -------------------------------------------------------
(SSres <- sum(residuals(nlsFit)^2))          # Residual sum of squares
(SStot <- sum((y - mean(y))^2))              # Total sum of squares
# 1 - (SSres/SStot)                          # R-squared measure  
(SSmod <- (SStot-SSres))                     # Model sum of squares
SSmod/SStot                                  # R-squared measure
