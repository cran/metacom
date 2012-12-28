OrderMatrix <-
function(matrix, scores=1){
	require(vegan)
	matrix=(matrix>0)+0
#reciprocal averaging
	temp=decorana(matrix,ira=1)
#ordering of matrix
	ret=matrix[order(temp$rproj[,scores], decreasing=FALSE), order(temp$cproj[,scores],decreasing=FALSE)]
	ret=as.matrix(ret)	
  ret=apply(ret,2,rev)
	return(ret)
}
