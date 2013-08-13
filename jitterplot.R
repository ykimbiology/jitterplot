

# Plot distributions of data points, similar to boxplot but using jitter.
# Inspired by Graphpad prism plot.

# jitterplot(df) # df = data.frame
# jitterplot(x,y,data=df) # plots distributions for the specified variables.
# jitterplot(df, col=c(), pch=c()) # specifies the color of the data points.
# jitterplot(df, stats=c()) # specifies the placement of horizontal lines; default=25, 50, 75] quantiles.

# Q: which should be used? data.frame vs. list?
# It is possible that number of data points may differ between two sets.

# Features: Uses R-base plotting. So par() and png() works.

get_jitter <- function(x, y) {
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
  # Boxes for 25/50th quantiles.
  # pos =  position on the x-axis to place the distributions.
  # width = width of the jitter and the median line.
  # 1) Add median.
  # 2) Add lower/upper lines for 25/50th percentiles.
   print(c('x', length(x)))
   
  xmedian <- median(x)
  xqt <- quantile(x)
  x25 <- xqt[2]
  x75 <- xqt[4]

  wl <- pos - width*0.5
  wr <- pos + width*0.5
  wsl <- pos - 0.25*width
  wsr <- pos + 0.25*width

  print(c('xmedian', xmedian, pos))
  segments(wl, xmedian, wr, xmedian)
  segments(wsl, xqt[2], wsr, xqt[2])
  segments(wsl, xqt[4], wsr, xqt[4])
  #Line between lower/upper lines:
  segments(pos, xqt[2], pos, xqt[4])
}


#http://stackoverflow.com/questions/7418386/dot-plot-in-r-using-r-plotting/
jitterplot <- function(xa, xb, xalabel, xblabel, ylabel) {
    # Plots two distributions.
    # For each distribution, x-axis values will be jittered.
    # For each distribution, a median horizontal line placed.
    xacol <- rgb(0,0,0,70,maxColorValue=255)
    xbcol <- rgb(0,0,0,70,maxColorValue=255)
    
    xcombined <- c(xa,xb)
    range <- c(min(xcombined),max(xcombined))
    #range <- c(0,0.4)
    plot(jitter(rep(0,length(xa)), amount=0.2), xa, xlim=range(-0.5,1.5),ylim=range, xaxt='n', frame.plot=T, xlab='', ylab=ylabel, col=xacol )
    points(jitter(rep(1,length(xb)), amount=0.2), xb, col=xbcol)

    #Place median lines.
    posa <- 0.0
    posb <- 1.0
    add_box(xa, posa, 0.5)
    add_box(xb, posb, 0.5)

    mtext(xalabel,at=posa, side=1, cex=2.0) # cex should be same as cex.axis
    mtext(xblabel,at=posb, side=1, cex=2.0)
    abline(h=0.0)    
}



test_jitter_version1 <- function() {
  n <- 50
  x <- rnorm(n)
  y <- rnorm(n)
  df <- data.frame(x=x,y=y)
  jitterplot(x,y,'SampleA','SampleB','Effect')
  df
}

df <- test_jitter_version1()
