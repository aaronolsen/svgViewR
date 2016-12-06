svgViewR:::svg.new(file='svgviewr.html', animate.duration=1)

# Create points with varying sin phase
n <- 100
x <- array(NA, dim=c(40, 2, n))
x_seq <- seq(-pi, pi, length=dim(x)[1])
n_seq <- seq(0, 2*pi, length=n)
for(i in 1:dim(x)[3]) x[, , i] <- cbind(x_seq, sin(x_seq + n_seq[i]))

# Draw points
svgViewR:::svg.points(x, cex=2, lwd=1, col="blue")

# Close viewer connection
svgViewR:::svg.close()

library(xml2)
library(rvest)
htm <- read_html("svgviewr.html")
svg <-  as.character(html_nodes(htm, "svg_doc"))


svgviewr(svg)
