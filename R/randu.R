library("rgl")
library("fields")
library(scatterplot3d)

makePoints <- function(n, seed = 1)
{
  M <- 2^31
  s <- seed
  points <- numeric(3*n)
  for (i in 1:(3*n))
  {
    s = (65539*s) %% M
    points[i] <- s
  }
  is <- 3*(1:n)
  data.frame(
    x = points[is-2],
    y = points[is-1],
    z = points[is]
  )
}
makePoints2 <- function(n, seed = 1)
{
  M <- 2^31
  s <- seed
  points <- numeric(n+2)
  for (i in 1:(n+2))
  {
    s = (65539*s) %% M
    points[i] <- s
  }
  is <- 1:n
  data.frame(
    x = points[is],
    y = points[is+1],
    z = points[is+2]
  )
}

makeChart3D <- function(xs, ys, zs, xlab, ylab, zlab)
{
  small_zs <- zs / 1000000
  val_lim <- range(small_zs)
  vlen <- ceiling(val_lim[2] - val_lim[1] + 1)
  colorlut <- topo.colors(vlen)
  col <- colorlut[ small_zs - val_lim[1]+1 ]
  open3d()
  persp3d(xs, ys, zs, color=col, axes=FALSE, back="lines", xlab = xlab, ylab = ylab, zlab = "")
  axes3d( edges=c("x--", "y--") )
  drape.plot(xs, ys, zs, r = 1, xlab = xlab, ylab = ylab, zlab = zlab,
             theta=-30, phi=30, ticktype="detailed", horizontal=FALSE)
  image.plot( legend.only=TRUE, zlim= c(min(zs), max(zs)), nlevel=vlen, col=colorlut)
}

makePlot <- function()
{
  p <- makePoints2(10000)
  c <- p$y - p$x
  c <- c - min(c)
  c <- c / max(c)
  #c <- rgb(0, abs(1-2*c), 1 - c)
  col <- rgb(c, 1-abs(2*c-1), abs(1-2*c))
  scatterplot3d(p, color=col, angle=140, tick.marks = FALSE, xlab="", ylab="", zlab="", pch=19)
  col <- rgb(c, abs(1-2*c), 1-abs(2*c-1))
  scatterplot3d(p, color=col, angle=140, tick.marks = FALSE, xlab="", ylab="", zlab="", pch=19)
  col <- rgb(1-abs(2*c-1), c, abs(1-2*c))
  scatterplot3d(p, color=col, angle=140, tick.marks = FALSE, xlab="", ylab="", zlab="", pch=19)
  col <- rgb(1-abs(2*c-1), abs(1-2*c), c)
  scatterplot3d(p, color=col, angle=140, tick.marks = FALSE, xlab="", ylab="", zlab="", pch=19)
  col <- rgb(abs(1-2*c), c, 1-abs(2*c-1))
  scatterplot3d(p, color=col, angle=140, tick.marks = FALSE, xlab="", ylab="", zlab="", pch=19)
  col <- rgb(abs(1-2*c), 1-abs(2*c-1), c)
  scatterplot3d(p, color=col, angle=140, tick.marks = FALSE, xlab="", ylab="", zlab="", pch=19)
  plus <- function(x) {
    x[x < 0] <- 0
    x
  }
  one <- function(x) {
    if (x < 1/3) return(1-3*x)
    else if (x < 2/3) return(3*x-1)
    else return(3 - 3*x)
  }
  two <- function(x) {
    if (x < 1/3) return(0)
    else return (1.5*x - 0.5)
  }
  three <- function(x) {
    if (x < 2/3) return(1 - 1.5*x)
    else return(0)
  }
  col <- rgb(sapply(c,one), sapply(c,two), sapply(c, three))
  scatterplot3d(p, color=col, angle=140, tick.marks = FALSE, xlab="", ylab="", zlab="", pch=19)
  col <- rgb(sapply(c,one), sapply(c, three), sapply(c,two))
  scatterplot3d(p, color=col, angle=140, tick.marks = FALSE, xlab="", ylab="", zlab="", pch=19)
  col <- rgb(sapply(c,two), sapply(c,one), sapply(c, three))
  scatterplot3d(p, color=col, angle=140, tick.marks = FALSE, xlab="", ylab="", zlab="", pch=19)
  col <- rgb(sapply(c,two), sapply(c,three), sapply(c, one))
  scatterplot3d(p, color=col, angle=140, tick.marks = FALSE, xlab="", ylab="", zlab="", pch=19)
  col <- rgb(sapply(c,three), sapply(c,one), sapply(c, two))
  scatterplot3d(p, color=col, angle=140, tick.marks = FALSE, xlab="", ylab="", zlab="", pch=19)
  col <- rgb(sapply(c,three), sapply(c,two), sapply(c, one))
  scatterplot3d(p, color=col, angle=140, tick.marks = FALSE, xlab="", ylab="", zlab="", pch=19)
}




