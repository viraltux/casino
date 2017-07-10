\name{toutf}
\alias{toutf}
\alias{toascii}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  ASCII game objects to UTF
}
\description{
  touft transforms game ASCII codes into its UTF equivalents. At the moment
  just for poker cards.

  toascii performs the inverse process.
}
\usage{
   toutf(token)
   toascii(token)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{token}{
    Game objects
}
}
\value{
  Returns the UTF version of the ASCII input or the ASCII version of a
  UTF input.
}
\author{
Fran Urbano <viraltux@gmail.com>
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
#example
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
% \keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
