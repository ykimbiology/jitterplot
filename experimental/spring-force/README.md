

GOALS:
1) use electron repulsion strategy to layout dots.
2) Find out whether this approach results in significantly better
      plotting of data.

Balls are placed on a line.
Spring forces are applied to each pair of balls.
Balls want to be placed in the middle of the line.
Run the simulation.

Energy
1) Repulsive between two balls
   energy = (abs(xi - xj))^(-alpha)
2) Attraction to the center.
   energy = (xi - xc)^2


Problems

1) Those orphan balls shoudl be attraced to the middle line.
This means beyond certain dstiance, replsive should not be calcualted.
In the extereme case, all balls should be placed on the middle line.
Here, no repulsive force.