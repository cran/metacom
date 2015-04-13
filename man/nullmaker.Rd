\name{NullMaker}
\alias{NullMaker}
\title{
Null Matrix Creator
}

\description{
Creates null matrices based on the constraints of the null model algorithm (\code{method}). Also allows for null matrices with a species that occurs at no sites, or a site without any species to be removed from the suite of simulated null matrices. This function borrows heavily from the \code{commsimulator} function in the \code{vegan} package, but also allows for the fixed-fixed null model. 
}

\usage{
NullMaker(comm, sims = 1000, method = "r1", 
	ordinate=TRUE, scores=1, allow.empty = FALSE, 
	verbose=FALSE)
}

\arguments{
  \item{comm}{
community data in the form of a presence absence matrix
}
  \item{sims}{
number of simulated null matrices to use in analysis
}
  \item{method}{
null model randomization method. See details below. 
}

	\item{ordinate}{
logical. Would you like to ordinate the null matrices? Default is TRUE.
}

  \item{scores}{
axis scores to ordinate matrix. 1: primary axis scores (default) 2: secondary axis scores. See Details.
}
		\item{allow.empty}{
logical argument indicating whether to allow null matrices to have empty rows or columns
}

 \item{verbose}{
logical. Prints a graphical progress bar that tracks the creation of null matrices. Useful for conservative null models on large and/or sparse data.
}
}

\details{
\code{method} is an argument handed to functions in the \code{vegan} package. Leibold & Mikkelson advocated the use of equiprobable rows and columns (provided that rows and columns had at least one entry). This method is called \code{r00}. Methods maintaining row (site) frequencies include \code{r0},\code{r1}, and \code{r2}. The default method argument is \code{r1}, which maintains the species richness of a site (row totals) and fills species ranges (columns) based on their marginal probabilities. Arguably the most conservative null algorithm is the fixed row - fixed column total null, which can be attained using many of swap algorithms described in the vegan package (sequential methods like \code{tswap}, \code{swap}, and non-sequential \code{quasiswap} and \code{backtracking}). See the help file for \code{commsim} or Wright et al. 1998 for more information. 


}

\value{
\item{}{
A list of \code{length(sim)} containing the null matrices}
}


\references{
J. Oksanen, F.G. Blanchet, R. Kindt, P. Legendre, P.R. Minchin, R.B. O'Hara, G.L. Simpson, P. Solymos, M.H.H. Stevens and H. Wagner (2012). vegan: Community Ecology Package. R package version 2.0-4. http://CRAN.R-project.org/package=vegan
}

\author{
Tad Dallas and John Lefcheck
}

\seealso{
\code{commsimulator}, \code{nullmodel}, \code{permatfull}, \code{commsim}
}

\examples{
# define an interaction matrix
data(TestMatrices)
intmat=TestMatrices[[5]]

# creation of the null matrices
nulls=NullMaker(intmat, sims=100, method='r1')

# output is a list of length sims with each list element 
#  corresponding to a null matrix
str(nulls)
}



\keyword{ordination}
