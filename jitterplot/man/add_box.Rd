\name{add_box}
\alias{add_box}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
jitterplot
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
add_box(x, pos, width)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{pos}{
%%     ~~Describe \code{pos} here~~
}
  \item{width}{
%%     ~~Describe \code{width} here~~
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
function (x, pos, width) 
{
    xmedian <- median(x)
    xqt <- quantile(x)
    x25 <- xqt[2]
    x75 <- xqt[4]
    wl <- pos - width * 0.5
    wr <- pos + width * 0.5
    wsl <- pos - 0.25 * width
    wsr <- pos + 0.25 * width
    h <- par()
    flwd <- 4
    print(c("xmedian", xmedian, pos))
    segments(wl, xmedian, wr, xmedian, lwd = flwd * h$lwd)
    segments(wsl, xqt[2], wsr, xqt[2], lwd = flwd * h$lwd)
    segments(wsl, xqt[4], wsr, xqt[4], lwd = flwd * h$lwd)
    segments(pos, xqt[2], pos, xqt[4], lwd = flwd * h$lwd)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
