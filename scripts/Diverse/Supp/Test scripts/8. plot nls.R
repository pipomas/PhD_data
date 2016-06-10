inter <- with(res, seq(0,200,1))
slope <- with(res, seq(-.1,.7, .005))

f <- function(inter,slope) {r <- 0.02387  -0.24985*inter  -0.18157*slope-0.04110*inter*slope}
# ... und Berechnen
z <- outer(inter, slope, f)

persp(x=inter, y=slope, z=z, theta = 30, phi = 30, expand = 0.5, col = "lightblue")

library(rgl)
with(res , plot3d(x=Sinter, y=Sslope, z=zTotal, size=1,
                       type='s',col="yellow"))

persp3d(x=inter, y=slope,z=z,
        col = "blue",
        aspect = c(1,1,.5),
        smooth=FALSE,border="black",
        front="line",
        line_antialias = F,
        add=TRUE)
  