require( tikzDevice )

tikz("output/tikz/myPlot.tex")

plot( 1, 1, main = "hello")

dev.off()
