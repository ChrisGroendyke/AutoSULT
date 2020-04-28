\name{AutoSULT-package}
\alias{AutoSULT-package}
\alias{AutoSULT}
\docType{package}
\title{
\packageTitle{AutoSULT}
}
\description{
\packageDescription{AutoSULT}
}
\details{

The DESCRIPTION file:
\packageDESCRIPTION{AutoSULT}
\packageIndices{AutoSULT}
}
\author{
\packageAuthor{AutoSULT}

Maintainer: \packageMaintainer{AutoSULT}
}
\references{
Dickson, D.C.M., Hardy, M.R., Waters, H.R., 2013 \emph{Actuarial Mathematics for Life Contingent Risks}, Second Edition, Cambridge, UK: Cambridge University Press.
}
\keyword{package}
\examples{
# Generate 15 problems total from Ch.3 (1 of type 1, 4 of type 2, etc.)
Ch3QA(probspec = c(1, 4, 2, 0, 3, 5))

# Generate 2 problems of each type from Ch.4.
Ch4QA(probspertype = 2)

# Generate 10 problems from among the 13 Ch. 5 problem types.
probvec <- sample(1:13, 10, TRUE)
for (i in 1:13) probvec[i] <- sum(probvec == i)
Ch5QA(probspec = probvec)
}
