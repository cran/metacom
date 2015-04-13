\name{IdentifyStructure}
\alias{IdentifyStructure}

\title{
Classifies Metacommunities
}

\description{
Identifies structure and outputs a classification. Currently only supports the 7 original metacommunity structures, and ignores the quasi-structures. 
}



\usage{
IdentifyStructure(metacom.obj)
}




\arguments{
 
 \item{metacom.obj}{
The result of the \code{Metacommunity} function, containing a list of 4 elements; the empirical matrix being tested, and results for coherence, turnover, and boundary clumping.
}

}



\value{
Returns the metacommunity classification based on the three EMS statistics. 
}


\author{
Jon Lefcheck & Tad Dallas
}

\note{
Quasi structures, as well as "random" and "Gleasonian" structures, may not strictly be discernable through the EMS approach, as they rely on inferring a result from a non-significant test ("accepting the null"), which is typically a bad idea. As such, we exclude the classification of quasi-structures in this function. Also, we encourage you to examine the \code{metacom.object} thoroughly, and not to rely on this function for classification. 
}



\examples{
# Define interaction matrices
data(TestMatrices)

# Analyze metacommunity & Identify the structure
IDmetacom = IdentifyStructure(Metacommunity(TestMatrices[[3]], 
   sims=100, method="r1", order=TRUE, allow.empty=FALSE))
}




