
library("reshape")
library("ggplot2")


fmt <- function(digits)
{
  function(x) format(x, nsmall = digits,scientific = FALSE)
}

my_theme <- function(base_size = 14, base_family = "")
{
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(
      panel.grid.minor.y = element_line(size=3),
      panel.grid.major = element_line(colour = "grey")
    )
}

rand <- function(n=1, a=0, b=1)
{
  runif(n) * (b-1) + a
}

randInts <- function(n, a=0, b=1)
{
  floor(runif(n) * (b - a + 0.999999) + a)
}

trajectory <- function(length, step=1, noise=0)
{
  ups <- rbinom(length/step, step, 0.5)
  diff <- 2*ups - step
  c(0,cumsum(diff)) + noise
}

makeSeveralRW <- function(num=10, length=50)
{
 data <- sapply(1:num, function(i) trajectory(length, 1, (i-num/2)/10))
 data <- data.frame(Krok=0:length, data)
 #colnames(data) <- c("krok", sapply(1:num, function(i) paste("sim", i)))
 data <- melt(data, 1, variable_name = "sim")
 ggplot(data, aes(x=Krok, y=value, color=sim)) +
   my_theme(18) +
   geom_line(size=1.5) +
   ylab("Stan") +
   scale_y_continuous(breaks=seq(from=-16, to=16, by=2)) +
   theme(legend.position = "none")
}


plotTrajectories <- function(n, length, step, alpha=0.2)
{  
  points <- seq(from=0, to=length, by=step)
  trs <- sapply(1:n, function(foo) trajectory(length, step))
  
  trs <- cbind( points, trs)  # adding labels of x axis
  colnames(trs) <- c("time", sapply(1:n, function(i) paste("tr", i)))
  trs <- data.frame(trs)
  trs <- melt(trs, id="time")
  
  itlogfun <- function(n) {
    n[n < 3] <- 3
    sqrt(2 * n * log(log(n)))
  }
  itlog <- data.frame(time=points, up=itlogfun(points), down=-itlogfun(points))
  itlog <- melt(itlog, id="time")
  
  sqrtfr <- data.frame(time=points, up2=sqrt(points), down2=-sqrt(points))
  sqrtfr <- melt(sqrtfr, id="time")
  
  ggplot(data=trs) + xlab("Krok")  + ylab("Stan") +
    geom_line( aes(x=time, y=value, group=variable), alpha=alpha ) +
    geom_line( data=itlog, aes(x=time, y=value, group=variable), size=2, color="#FF8080" ) +
    geom_line( data=sqrtfr, aes(x=time, y=value, group=variable), size=2, color="#007FFF" ) +
    theme_bw(base_size=18)+
    theme(legend.position = "none")
}


plotForLemma <- function()
{
  data <- data.frame(Krok=1:9, val1= c(1,2,1,2,1,0,1,2,1)+0.05, val1= c(-1,-2,-1,-2,-1,0,1,2,1)-0.05)
  data <- melt(data, 1, variable_name = "version")
  ggplot(data, aes(x=Krok, y=value, colour=version)) +
    my_theme(18) +
    geom_line(size=2) +
    ylab("Stan") +
    scale_y_continuous(breaks=seq(from=-3, to=3, by=1)) +
    scale_x_continuous(breaks=seq(from=0, to=10, by=2)) +
    geom_point(aes(x=1,y=1), color="#FF7373", size=5) +
    geom_point(aes(x=1,y=-1), color="#00C0C0", size=5) +
    geom_point(aes(x=9,y=1), color="black", size=5) +
    geom_hline(aes(yintercept=0)) +
    theme(legend.position = "none")
}

plotDiscArcSine <- function(n=10)
{
  p <- function(k,n) #p(k,n) to wartosc p_{2k,2n} z pracy!
  {
    choose(2*k, k) * choose(2*n - 2*k, n-k) / 2^(2*n)
  }
  data <- data.frame(k = 2*(0:n), prob = sapply(0:n, function(k) p(k, n)))
  ggplot(data=data, aes(x=k, y=prob)) +
    my_theme(18) + xlab("Czas przewagi")  + ylab("Prawdopodobieństwo") +
    geom_bar(stat="identity", fill="lightblue") +
    scale_x_continuous(breaks=seq(from=0, to=2*n, by=2))
}
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

plotArcSine <- function(n=10)
{
  f <- function(t)
  {
    1 / ( pi * sqrt(t * (1 - t)) )
  }
  
  F <- function(t)
  {
    2 / pi * asin(sqrt(t))
  }
  xs <- c(seq(from=0.005, to=0.05, by=0.001), seq(from=0.06, to=0.94, by=0.01), seq(from=0.95, to=0.995, by=0.001))
  xs2 <- c(0,xs,1)
  den <- data.frame(x=xs, y=f(xs))
  dis <- data.frame(x=xs2, y=F(xs2))
  g1 <- ggplot(data=den, aes(x=x, y=y)) +
    my_theme(18) + xlab(NULL) +
    ylab(NULL) + ggtitle("Gęstość") +
    geom_line()
  g2 <- ggplot(data=dis, aes(x=x, y=y)) +
    my_theme(18) + xlab(NULL)  +
    ylab(NULL) + ggtitle("Dystrybuanta") +
    geom_line()
  multiplot(g1, g2, cols=2)
}
