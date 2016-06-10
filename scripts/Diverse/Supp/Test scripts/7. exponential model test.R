dat <- data.frame(size = c(1.8, 3.6, 7.2), y = c(36.03355284, 45.90567406, 82.96282295))
str(dat)
plot(dat$size, dat$y)


# fit non-linear model
m1 <- nls(y ~ exp(a + b * size), data = dat, start = list(a = 1, b = 1))

summary(m1)

# add fitted curve
lines(dat$size, predict(m1, list(x = dat$size)))


# 9.2 Simple exponential model --------------------------------------------
xvalues <- c(1.8, 3.6, 7.2)
yvalues <- c(36.03355284, 45.90567406, 82.96282295)

nlsFit <- nls(yvalues ~ I(intercept*exp(slope*xvalues)), start = list(intercept = 30, slope = .10))

plot(xvalues,yvalues)
lines(xvalues, predict(nlsFit, list(x = xvalues)))
coef(nlsFit)

# -----
x <- seq(min(xvalues), max(xvalues), length=100)
y <- predict(nlsFit, list(xvalues=x))
points(x, y, type='l', col='blue')

# 9.3 Simple exponential model --------------------------------------------
xvalues <- c(1.8, 3.6, 7.2)
yvalues <- c(36.03355284, 45.90567406, 82.96282295)

expFunction <- function(x,intercept,slope){I(intercept*exp(slope*x))}

nlsFit <- nls(yvalues ~ expFunction(xvalues, intercept, slope), start = list(intercept = 20, slope = .01))

plot(xvalues,yvalues)
lines(xvalues, predict(nlsFit, list(x = xvalues)))
coef(nlsFit)
coef(nlsFit)[1]
coef(nlsFit)[2]
# -----
x <- seq(min(xvalues), max(xvalues), length=100)
y <- predict(nlsFit, list(xvalues=x))
points(x, y, type='l', col='blue')



