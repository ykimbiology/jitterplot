cutoff_dist_min_repl <- 1


get_distance <- function(xi, xj, cutoff_dist_min) {
    #cutoff_dist_min <- 0.01
    #cutoff_dist_max <- 1
    
    xd <- xi - xj
    distance <- sqrt(sum(xd*xd))
    if (is.na(cutoff_dist_min) == F) {
      if (distance < cutoff_dist_min) { distance <- cutoff_dist_min}
    }
    #if (distance > cutoff_dist_max) { distance <- cutoff_dist_max}
    
    distance
}


get_energy_attr_pair <- function(xi, xc, alpha) {
  #Attractive energy between two points.
  cutoff_dist_min <- NA
  d <- get_distance(xi,xc, cutoff_dist_min)
  energy <- d^alpha
  energy
}

get_energy_attr <- function(x, xc, alpha) {
  energy <- 0
  for (i in 1:length(x[,1])) {
    xi <- x[i,]
    energy_i <- get_energy_attr_pair(xi, xc, alpha)
    energy <- energy + energy_i
  }
  energy
}

get_d_energy_attr <- function(x,xc,alpha){
  # Return derivatives with erspect to all components of the points. Actually only x for now.
  # dx: [n x p]
    cutoff_dist_min <- NA
    nrow <- length(x[,1])
    ncol <- length(x[1,])
    dx <- matrix(nrow=nrow, ncol=ncol)
    for (i in 1:nrow) {
        xi <- x[i,]
        distance <- get_distance(xi,xc,cutoff_dist_min)
        xk <- alpha*distance^((alpha-2)/2)
        dx[i,] <- xk*(xi-xc)
    }
    dx
}

get_energy_repl_pair <- function(xi, xj, radius, alpha) {
    #wa <- (2*radius)^alpha
    wa <- 2*radius
    distance <- get_distance(xi, xj,    cutoff_dist_min_repl)
    wb <- distance^alpha
    energy <- wa/wb
    energy
}

get_energy_repl <- function(x, radius, alpha) {
    # x = [nxp]; n = number of points; p = dimension of the coordinate.
    energy <- 0
    nrow <- length(x[,1])
    for (i in 1:(nrow-1)) {
        for (j in (i+1):nrow) {
            #print(c(i,j))
            xi <- x[i,]
            xj <- x[j,]
            energy_i <- get_energy_repl_pair(xi,xj,radius,alpha)
            energy <- energy +energy_i
        }
    }
    energy
}

get_d_energy_repl <- function(x, radius, alpha) {
    nrow <- length(x[,1])
    ncol <- length(x[1,])
    dx <- matrix(nrow=nrow,ncol=ncol)
    
    wa <- 2*radius
    for (i in 1:nrow) {
        xi <- x[i,]
        dxi <- rep(0, ncol)
        #Derivative with respect to xi:
        for (j in 1:nrow) {
            xj <- x[j,]
            distance <- get_distance(xi,xj, cutoff_dist_min_repl)
            if (distance < 2.0) {
              xk <- -1*alpha*distance^((-1*alpha-3)/2)
              if (i == j) { xk = 0 }
              dxi <- dxi + wa*xk*(xi - xj)
            }
        }
        dx[i,] <- dxi
    }
    dx
}

get_energy <- function(x, xc, radius, alpha) {
    #x = a matrix of n data points, each with p coordiantes: [nxp]
    #xc = position of the center.

    energy <- 0.0
    en_attr <- get_energy_attr(x, xc, alpha)
    en_repl <- get_energy_repl(x, radius, alpha)
    #print('DEBUG')
    #print(x)
    energy <- en_attr + en_repl
    #energy <- en_repl
    energy
}

get_d_energy <- function(x,xc,radius,alpha) {
    nrow <- length(x[,1])
    ncol <- length(x[1,])
    dx <- matrix(nrow=nrow, ncol=ncol)
    dx_attr <- get_d_energy_attr(x,xc,alpha)
    dx_repl <- get_d_energy_repl(x,radius,alpha)
    dx_attr[,2] <- 0  # set y-axis to zero
    dx_repl[,2] <- 0  # set y-axis to zero
    dx <- dx_attr + dx_repl
    
    print('x')
    print(x)
    print('dx_attr')
    print(dx_attr)
    print('dx_repl')
    print(dx_repl)
    dx
}

get_points_random <- function(n) {
  # Get random data points.
  x <- rnorm(n)
  #x <- c(10, -10) # This is wehre min is located.
  #y <- rep(0,n)
  y <- rnorm(n, mean=0)
  xy <- cbind(x,y)
  xy
}

test_minimization <- function() {

  xc <- c(0.0, 0.0)
  radius <- 1.0
  alpha <- 1.0 # 2.0


  n <- 5 # 5
  k <- 1.0/n  # Scaling factor for the gradients.
  x <- get_points_random(n)
  plot(x[,1],x[,2])


  niter <- 100
  for (i in 1:niter) {
    #plot(x,y)
    plot(x[,1],x[,2],type='n')
    #for (j in 1:length(x[,1])) {
    #    draw.circle(x[j,1], x[j,2], radius)
    #}
    points(x[,1], x[,2])
    abline(v=0)
    
    energy <- get_energy(x,xc,radius,alpha)
    dx <- get_d_energy(x,xc,radius,alpha)
    x <- x - k*dx
    #x <- x +1
    print(c('energy', i,k,energy))
    print('Main x')
    print(x)
    print('Main dx')
    print(dx)
    #readline()
  }
  x
}

library("plotrix")
x <- test_minimization()

