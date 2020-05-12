\name{Ch5QA}
\alias{Ch5QA}
\title{Chapter 5 Problem and Solution Generation
}
\description{
Randomly generates and typesets problems of 13 specified types from Chapter 5 of ALMCR3e.  All problems use the Standard Ultimate Life Table (SULT) a.k.a. Standard Ultimate Survival Model (SUSM).  This function generates two PDF files, one listing the problems, and another with the step-by-step solutions. 
}
\usage{
Ch5QA(probspertype = 5, probspec = NULL, randomize = FALSE, probsummary = FALSE,
  qfile = "qfilech5", afile = "afilech5", keeprmd = FALSE)
}
\arguments{
  \item{probspertype}{
Number of problems of each type to generate.  Use this argument (instead of \code{probspec})to generate the same number of problems for each of the 13 types of Chapter 5 SULT problems.
}
  \item{probspec}{
Vector of length 13 giving the number of each of the 13 types of Chapter 5 SULT problems to generate.  Use this argument (instead of \code{probspertype}) to generate different numbers of problems for the 13 types of Chapter 5 SULT problems.
}
  \item{randomize}{
Boolean variable controlling whether the problems will be randomized (TRUE), or appear in problem type order (FALSE).
}
  \item{probsummary}{
Boolean variable controlling whether a summary of the problem types generated will be placed at the end of the question file.
}
  \item{qfile}{
Name (and path) to write question PDF file.
}
  \item{afile}{
Name (and path) to write solution PDF file.
}
  \item{keeprmd}{
Boolean variable controlling whether the Rmd files compiled to produce the PDFs are kept (TRUE) or deleted (FALSE) after the PDF is rendered.
}
}
\details{
Specify the types of problems generated using either the (scalar) \code{probspertype} parameter or the (vector) \code{probspec} parameter.  See package vignette for further information.
}
\value{
Invoked for the side effect of producing the two PDF files.
}
\references{
Dickson, D.C.M., Hardy, M.R., Waters, H.R., 2020 \emph{Actuarial Mathematics for Life Contingent Risks}, Third Edition, Cambridge, UK: Cambridge University Press.}
\author{
Chris Groendyke \email{groendyke@rmu.edu}
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{Ch3QA}}, \code{\link{Ch4QA}}, and \code{\link{Ch9QA}} for analogous functions for generating problems and solutions for material in chapters 3, 4, and 9, respectively.
}
\examples{
# Generate 2 problems of each type.
Ch5QA(probspertype = 2) 

# Generate 10 total problems from among the 13 Ch. 5 problem types.
Ch5QA(probspec = tabulate(sample(1:13, 10, TRUE), nbins=13))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
