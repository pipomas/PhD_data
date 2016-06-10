# Überprüfung der 'plogis' Funktion ---------------------------------------

logistisch <- function(x, m, s){1 / (1 + exp(-(x-m)/s))}
logistisch(-3, 0, .8)

# Funktionen für Faktorladungen der Hick Aufgabe --------------------------
plogis(-3, scale = .8)
plogis(-1, scale = .8)
plogis( 1, scale = .8)
plogis( 3, scale = .8)

y <- plogis(c(-3,-1,1,3), scale = .8)
x <- 1:4
plot(x,y, type = "o", col = 1)


# Darstellung der logistischen Funktion ---------------
y <- plogis(seq(-5,5,1), scale = .8)+5
y <- plogis(seq(-5,5,1), scale = .8)
x <- seq(-5,5,1)+5
plot(x, lty = 1, col = 2)

pnorm(c(1:4))

plot(x,y, type = "o", col = 1)


