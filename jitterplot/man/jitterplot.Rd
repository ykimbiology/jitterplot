\name{jitterplot}
\alias{jitterplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
jitterplot
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
jitterplot(xlist, xlabel_list, ylabel)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xlist}{
%%     ~~Describe \code{xlist} here~~
}
  \item{xlabel_list}{
%%     ~~Describe \code{xlabel_list} here~~
}
  \item{ylabel}{
%%     ~~Describe \code{ylabel} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (xlist, xlabel_list, ylabel) 
{
    xnames <- names(xlist)
    xcol <- rgb(0, 0, 0, 100, maxColorValue = 255)
    xcombined <- unlist(xlist)
    yrange <- c(min(xcombined), max(xcombined))
    xrange <- c(-0.5, length(xnames) - 0.5)
    plot(c(), c(), xlim = xrange, ylim = yrange, xaxt = "n", 
        frame.plot = T, xlab = "", ylab = ylabel, col = xcol, 
        pty = "n")
    for (i in 1:length(xnames)) {
        pos <- i - 1
        x <- xlist[[xnames[i]]]
        print(c("Debug", xnames[i], pos))
        points(jitter(rep(pos, length(x)), amount = 0.2), x, 
            col = xcol)
        add_box(x, pos, 0.5)
        mtext(xlabel_list[i], at = pos, side = 1, line = 1, cex = 2 * 
            h$cex)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
