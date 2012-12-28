\name{OrderMatrix}
\alias{OrderMatrix}

\title{
Ordinates interaction matrix
}
\description{
'OrderMatrix' ordinates an interaction matrix scores derived from reciprocal averaging (Gauch et al. 1977). These scores represent a latent environmental gradient along which species distributions are structured.   
}



\usage{
OrderMatrix(matrix, scores = 1)
}




\arguments{
  \item{matrix}{
community data in the form of a presence absence matrix
}
  \item{scores}{
axis scores to ordinate matrix. 1: primary axis scores (default) 2: secondary axis scores
}
}



\value{
An ordinated matrix is returned. This function is already contained within functions calculating coherence, species turnover & boundary clumping, but may be useful for visualizations. 
}



\references{
Gauch, H.G., R.H. Whittaker, and T.R. Wentworth. 1977. A comparative study of reciprocal averaging and other ordination techniques. Journal of Ecology 65:157-174.

Leibold, M.A. and G.M. Mikkelson. 2002. Coherence, species turnover, and boundary clumping: elements of meta-community structure. Oikos 97: 237 - 250.

Oksanen,J., F.G. Blanchet, R. Kindt, P. Legendre, P.R. Minchin, R.B. O'Hara, G.L. Simpson, P. Solymos, M.H.H. Stevens and H. Wagner (2012). vegan: Community Ecology Package. R package version 2.0-4. http://CRAN.R-project.org/package=vegan

}

\author{
Tad Dallas
}

\note{
'OrderMatrix', like many of these functions, relies heavily on the 'vegan' package.
}



\examples{
#define an interaction matrix
data(testmatrices)
pres3c=testmatrices[[6]]


#rotate the matrix 90 degrees right and plot
image(apply(pres3c,2,rev), col=c(0,'dodgerblue3'), axes=FALSE, xlab='species', ylab='sites', cex.lab=1.4)
  box(lwd=2,col=1)
}



\keyword{ordination}
