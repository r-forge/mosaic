\name{sample}
\alias{sample}
\alias{sample.matrix}
\alias{sample.default}
\alias{sample.data.frame}
\alias{deal}
\alias{shuffle}
\alias{flip}

\title{ Random Samples and Permutations}
\description{
These extensions to the \code{base::sample} make resampling and
randomization tests easier.
}

\usage{
sample(x, size, replace = FALSE, prob = NULL, 
	groups = NULL, orig.ids = FALSE) 
sample.matrix(x, size, replace = FALSE, prob = NULL, 
	groups = NULL, orig.ids = FALSE)
sample.data.frame(x, size, replace = FALSE, prob = NULL, 
	groups = NULL, orig.ids = FALSE)
sample.default(x, size, replace = FALSE, prob = NULL, groups = NULL, 
    orig.ids = FALSE) 

shuffle(x, replace = TRUE, prob = NULL, 
	groups = NULL, orig.ids = FALSE)
deal(x, size, replace = FALSE, prob = NULL, 
	groups = groups, orig.ids = FALSE)

flip(n=1, prob=.5, quiet=FALSE, verbose=!quiet)
qflip(n=1, prob=.5)

}

\arguments{
  \item{x}{ an object (typically a data frame, matrix, or vector)
}
  \item{size}{ size of the sample
}
\item{n}{ number of coins to toss}

  \item{replace}{ a logical indicating whether sampling is to be done with
  replacement.
}
  \item{prob}{ A vector of probability weights for obtaining the elements of the object being sampled, or the probability of heads for \code{flip} and \code{qflip}.
}
  \item{groups}{ if present, shuffling is done within groups given by
  this vector ans \code{size}, if present, is ignored
}
  \item{orig.ids}{ if \code{TRUE} and \code{x} is a data frame or a matrix,
  then the original locations of the data is recorded in the object returned.
}
	\item{quiet}{ If \code{TRUE}, suppress output.}
	\item{verbose}{ If \code{FALSE}, suppress output.}
}
\value{
an object of the same type as \code{x} unless \code{x} is an integer, which
case a vector is returned.
}
\author{
Daniel Kaplan (\email{kaplan@macalester.edu})
and Randall Pruim (\email{rpruim@calvin.edu})
}

\examples{
denominations <- c(2:10,c("J","Q","K","A"))
suits <- c("C","D","H","S")
cards <- as.vector(outer(denominations, suits, paste, sep="") )
shuffle(cards)
set.seed(52)
sample(cards, 13)
set.seed(52)
deal(cards, 13)      # same as sample(cards, 13)
resample(cards, 20)  # same as sample(..., replace=TRUE)
sample(cards, groups=rep(1:4, each=13))
flip()               # flip 1 coin
flip(20)             # flip 20 coins
qflip(20)            # flip 20 coins and only show the number of heads
table(do(1000) * qflip(20))   # a table based on flipping 20 coins 1000 times
table(do(1000) * qflip(20,.25))   # a table based on flipping 20 biased coins 1000 times
}
\keyword{ random }
