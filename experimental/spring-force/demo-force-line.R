

get_energy_attr <- function(x, xc) {
    d <- (x - xc)
    energy <- sum(d*d)
    energy
}

get_d_energy_attr <- function(x,xc){
    dx <- 2.0*(x - xc)
    dx
}

get_energy_repl <- function(x, radius, alpha) {
    energy <- 0
    xmin <- 0.01*sd(x)
    xlen <- length(x)
    for (i in 1:(xlen-1)) {
        for (j in (i+1):xlen) {
            #print(c(i,j))
            wa <- (2*radius)^alpha
            wb <- (x[i] - x[j])^alpha
            if (wb < xmin) {
                wb <- xmin
            }
            energy <- energy + wa/wb
        }
    }
    energy
}

get_d_energy_repl <- function(xorig, radius, alpha) {
    konst <- -1.0*alpha*2*radius
    dx <- rep(0, length(xorig))
    for (i in 1:length(xorig)) {
        xtemp <- xorig[i] - xorig
        #Remove the ith entry
        idx <- rep(T, length(xorig))
        idx[i] <- F
        xtemp <- xtemp[idx]
                  
        xtemp <- xtemp^(-1.0*alpha - 1)
        xtemp <- konst*xtemp
        dx[i] <- sum(xtemp)
    }
    dx
}

get_energy <- function(x, xc, radius, alpha) {
    #x = a vector of positions.
    #xc = position of the center.
    #k=scaling factor
    energy <- 0.0
    en_attr <- get_energy_attr(x, xc)
    en_repl <- get_energy_repl(x, radius, alpha)
    energy <- en_attr + en_repl
    energy
}

get_d_energy <- function(x,xc,radius,alpha) {
    xlen <- length(x)
    dx <- rep(0,xlen)
    dx <- get_d_energy_attr(x,xc) + get_d_energy_repl(x,radius,alpha)
    dx
}

library("plotrix")

xc <- 0.0
radius <- 0.1
alpha <- 2.0 # 2.0
k <- 0.01

n <- 5 # 5
x <- rnorm(n)
y <- rep(0,n)
plot(x,y)
abline(v=0)

#x <- c(1,3)
#energy <- get_energy(x,xc,radius,alpha)
#d_energy <- get_d_energy(x,xc,radius,alpha)

niter <- 1000
for (i in 1:niter) {
    #plot(x,y)
    plot(x,y,type='n')
    for (j in 1:length(x)) {
        draw.circle(x[j],0,radius)
    }
    energy <- get_energy(x,xc,radius,alpha)
    dx <- get_d_energy(x,xc,radius,alpha)
    x <- x - k*dx
    print(c(i,energy))
    #readline()
}
