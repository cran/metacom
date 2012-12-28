metacommunity <-function(matrix, scores=1, method='r1', sims=1000, order=TRUE){
	require(lattice)
	require(vegan)

	if(order==TRUE){mat=OrderMatrix(matrix,scores=scores)
	}else{mat=matrix}

	comat=Coherence(mat,method=method,sims=sims,scores=scores)
	boundmat=BoundaryClump(mat)
  ranmat=Turnover(mat,method=method,sims=sims,scores=scores)
	ret=list(Matrix=mat, Coherence=comat, Turnover=ranmat, Boundary=boundmat)
	return(ret)
}
