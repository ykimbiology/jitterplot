

# Show how the energy function looks.

# For a gien energy funciton, show energy plot for two points.

# energy = repulsion + attraction to center.

# Allow 3D coordinates

source('demo-force-line.R')

n <- 100
step <- 0.02
x <- seq(0,n-1)
x <- x*step
xdist <- rep(0, n)
radius <- 1
alpha <- 1
energy <- rep(0,n)
for (i in 1:n) {
    xi <- c(-x[i], 0)
    xj <- c(x[i], 0)
    distance <- get_distance(xi,xj)
    xdist[i] <- distance
    #energy[i] <- get_energy_repl_pair(xi, xj, radius, alpha)
    xtemp <- matrix(nrow=2, ncol=2)
    xtemp[1,] <- xi
    xtemp[2,] <- xj
    xc <- xi
    print(xtemp)
    energy[i] <- get_energy(xtemp, xc, radius, alpha)
    
}

png(filename='plot-energy-function.png',width=1200,height=1200)
par(cex=4.0,lwd=4.0)
i=3:length(xdist)
plot(xdist[i], energy[i], xlab='Distance', ylab='Total Energy')
dev.off()


