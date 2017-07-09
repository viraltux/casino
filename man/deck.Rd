\name{deck}
\alias{deck}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Standard 52 poker card deck
}
\description{
  This functions returns an array with n number of suffled standard 52
  poker card decks. 
  
  The ASCII format for card ranks is A,1,2,3,4,5,6,7,8,9,10,J,Q,K
  The ASCII format for card suits is:
  Clubs: C, Diamonds: D, Hearts: H, Spades: S
}
\usage{
deck(n = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{
    Number of decks
  }
}

\value{
%%  ~Describe the value returned
An array with n number of suffled standard 52 poker card decks.
}

\author{
Fran Urbano <viraltux@gmail.com>
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
## 

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
