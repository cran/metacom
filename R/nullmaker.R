nullmaker <-function(matrix, sims=1000,method='r1', allow.empty=FALSE){
	require(vegan)	
	i=1; ret=list()

	if(allow.empty==FALSE){
	while(i < sims+1){
		temp=commsimulator(matrix,method=method)
		if(any(rowSums(temp) < 1) || 
			any(colSums(temp) <  1)){ 
			
		}else{
		ret[[i]] = OrderMatrix(temp)
		i=i+1}
		}
	}

	if(allow.empty==TRUE){
	i=1		
	while(i < sims+1){
		temp=commsimulator(matrix,method=method)
		ret[[i]] = OrderMatrix(temp)
		i=i+1}
		}
	return(ret)
}
