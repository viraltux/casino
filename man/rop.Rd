\name{rop}
\alias{\%==\%}
\alias{\%!=\%}
\alias{\%>=\%}
\alias{\%<=\%}
\alias{\%>\%}
\alias{\%<\%}
\title{
  Relational operators between poker hands
}
\description{
  Given two objects of class 'hand' these operators will tell which hand
  is the higher.
}
\usage{
 left \%==\% right
 left \%>\% right
 left \%<\% right
 left \%>=\% right
 left \%<=\% right
 left \%!=\% right
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{left, right}{
    poker hands
  }
}

\value{
  Returns a logical value indicating the result of the hand comparison.
}

\author{
Fran Urbano <viraltux@gmail.com>
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~misc }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~poker }% __ONLY ONE__ keyword per line
