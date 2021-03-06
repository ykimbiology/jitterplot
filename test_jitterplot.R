
source('jitterplot.R')

test_jitter_version1 <- function() {
  n <- 500
  x <- rnorm(n)
  y <- rnorm(2*n)
  z <- rnorm(3*n)
  xlabel_list <- c('SampleA','SampleB')
  #xlabel_list <- c('SampleA','SampleB', 'SampleC')
  ylabel <- 'Effect'
  dlist <- list(x=x,y=y)
  #dlist <- list(x=x,y=y,z=z)
  jitterplot(dlist, xlabel_list, ylabel)
  dlist
}

png(filename='test.png',width=1200,height=1200)
par(cex=2.0, lwd=2.0)
dlist <- test_jitter_version1()
dev.off()
