\name{bhand}
\alias{bhand}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
 Best poker hand
}
\description{
 Returns the best poker hand within a set of cards. However, the best poker hand itself is not yet implemented.
}
\usage{
bhand(cards)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cards}{
   vector containing poker cards following the regular expresion:
       ^(([2-9]|10)|[AJQK])[♣♦♥♠]$
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
hand: Object of class 'hand' containing information about the best hand found withing the set of cards. class hand has four attributes:
     name : Best hand name
     level: Number indicating how the hand levels among other hands
     high : Number indicating how high is the hand within its category
     score: Number that allow to compare two hands level*100 + high
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Fran Urbano
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
  #Three of a kind
  cards = c("2♦","J♦","4♠","A♣","A♠","A♥","K♠")
  bh <- bhand(cards)
  names(bh)
  bh
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
