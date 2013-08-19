

# Plot distributions of data points, similar to boxplot but using jitter.
# Inspired by Graphpad prism plot.

#
# jitterplot(dlist) # df = data.frame
# jitterplot(x,y,data=df) # plots distributions for the specified variables.
# jitterplot(df, col=c(), pch=c()) # specifies the color of the data points.
# jitterplot(df, stats=c()) # specifies the placement of horizontal lines; default=25, 50, 75] quantiles.

# Q: which should be used? data.frame vs. list?
# It is possible that number of data points may differ between two sets.

# Features: Uses R-base plotting. So par() and png() works.
# Allow variable number of distribtions to plot. range = c(1, n); n=number of distributions.

get_jitter <- function(x, y) {
  #Internal function.
  # Experimental;
  # Add jitter that dependes on neighbouring point.
  # For each x, amount of jitter depends on how many neighbouring point it has.
  ystd <- sd(y)
  j <- rep(0,length(x))  # Will store jitter amount.
  for (i in 1:length(x)) {
    count <- 0
      for (k in 1:length(x)) {
        deviation <- abs(x[i] - x[k])
        if (deviation < ystd) {  
	       count <- count + 1
	}
      }
    j[i] <- count
  }
  j <- j/sum(j)
  j
}

add_box <- function(x, pos, width) {
  #Internal function.
  # Boxes for 25/50th quantiles.
  # pos =  position on the x-axis to place the distributions.
  # width = width of the jitter and the median line.
  # 1) Add median.
  # 2) Add lower/upper lines for 25/50th percentiles.
   #print(c('x', length(x)))
   
  xmedian <- median(x)
  xqt <- quantile(x)
  x25 <- xqt[2]
  x75 <- xqt[4]

  wl <- pos - width*0.5
  wr <- pos + width*0.5
  wsl <- pos - 0.25*width
  wsr <- pos + 0.25*width

  #Get current plotting parameter.
  h <- par()
  flwd <- 4.0
  print(c('xmedian', xmedian, pos))
  segments(wl, xmedian, wr, xmedian, lwd=flwd*h$lwd)
  segments(wsl, xqt[2], wsr, xqt[2], lwd=flwd*h$lwd )
  segments(wsl, xqt[4], wsr, xqt[4], lwd=flwd*h$lwd)
  #Line between lower/upper lines:
  segments(pos, xqt[2], pos, xqt[4], lwd=flwd*h$lwd)
}

get_jitter_gaussian <- function(x, amount) {
  # Add noise sampled from a gaussian distribution using specified sd.
    #Jitter based on normal distribution.
    xnorm <- rnorm(length(x),mean=0.0, sd=amount)
    #Re-sample if sampled point outside of specified width.
    width <- 2.2*amount
    for (i in 1:length(xnorm)) {
      if (abs(xnorm[i]) > width) {
          repeat {
          xnorm[i] <- rnorm(1, mean=0.0, sd=amount)
          if (abs(xnorm[i]) < width) { break }
          }
      }
    }
    
    #dfactor <- 1/(abs(xnorm)^1)
    dfactor <- 1
    xj <- x + dfactor*xnorm
    xj
}

get_jitter_exponential <- function(x, amount) {
  # Spread of data points sampled from an exponential distribution.
   xdistr <- rexp(length(x), rate=1/amount)
   xdistr <- xdistr^1.2
   xsign <- rep(c(-1,1),length(x))
   xsign <- xsign[1:length(x)]
   xdistr <- xdistr*xsign  # To get both +/- numbers
    #Re-sample if sampled point outside of specified width.
    width <- 2.2*amount
    for (i in 1:length(xdistr)) {
      if (abs(xdistr[i]) > width) {
          repeat {

          xsign <- 1
          if (rnorm(1) < 0) {
              xsign <- -1
          }
          xdistr[i] <- xsign*rexp(1, rate=1/amount)

          if (abs(xdistr[i]) < width) { break }
          }
      }
    }
    
    #dfactor <- 1/(abs(xnorm)^1)
    dfactor <- 1
    xj <- x + dfactor*xdistr
    xj
}

get_jitter_kernel <- function(x, amount) {
  # Add noise sampled from gaussian.
  # Also take into account how many neighboring points there are.
  # The width of the distribution will depend on the size of neighbors.
}

jitterplot <- function(xlist, xlabel_list, ylabel) {
  # jitterplot(xlist)
  #This version allows plotting of variable number of distribuitons.
  # Plots two distributions.
  # For each distribution, x-axis values will be jittered.
  # For each distribution, a median horizontal line placed.
  xnames <- names(xlist)

  #Apply transparency to the dots.
  xcol <- rgb(0,0,0,100, maxColorValue=255)

  #Get min/max for the entire data.
  xcombined <- unlist(xlist)
  yrange <- c(min(xcombined),max(xcombined))
  xrange <- c(-0.5, length(xnames)-0.5)

  #Create the plotting region.
  plot(c(), c(), xlim=xrange, ylim=yrange, xaxt='n', frame.plot=T, xlab='', ylab=ylabel, col=xcol, pty='n')

  #Get current plotting parameters.
  h <- par()
  
  #Plot each distribution.
  for (i in 1:length(xnames)) {
    pos <- i - 1.0
    x <- xlist[[xnames[i]]]
    print(c('Debug', xnames[i], pos))
    #xj <- jitter(rep(pos, length(x)), amount=0.2)
    #xj <- get_jitter_gaussian(rep(pos,length(x)), amount=0.1)
    xj <- get_jitter_exponential(rep(pos,length(x)), amount=0.1)

    
    points(xj, x, col=xcol,pch=19 )
    add_box(x, pos, 0.5)
    mtext(xlabel_list[i], at=pos, side=1,line=1, cex=2*h$cex) # cex should be same as cex.axis 
  }
  
}



