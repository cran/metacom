\name{TestMatrices}
\alias{TestMatrices}
\docType{data}
\title{
Interaction Matrices Used to Test Package Functions
}


\description{
A list of 5 interaction matrices for demonstration.
}


\usage{data(TestMatrices)}


\format{
A list containing artificial interaction matrices for test purposes

	[[1]]  \code{dim=(20 x 20)} Randomly generated matrix (\code{rbinom(400,1,0.4)})

	[[2]]  \code{dim=(20 x 20)} Checkerboard matrix

	[[3]]  \code{dim=(20 x 20)} Nested subset matrix

	[[4]]  \code{dim=(15 x 10)} Clementsian matrix

	[[5]]  \code{dim=(20 x 20)} Gleasonian matrix

}



\source{
Leibold, M. A., & Mikkelson, G. M. (2002). Coherence, species turnover, and boundary clumping: elements of metacommunity structure. Oikos, 97(2), 237-250.

Presley, S. J., C. L. Higgins, and M. R. Willig. 2010. A comprehensive framework for the evaluation of metacommunity structure. Oikos 119:908-917
}



\examples{
#load list containing interaction matrices
data(TestMatrices)

str(TestMatrices)

}


\keyword{datasets}
