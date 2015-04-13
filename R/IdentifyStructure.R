IdentifyStructure = function(metacom.obj) {
  #Random
		coh=as.numeric(as.character(unlist(metacom.obj$Coherence)[1:5]))
		turn=as.numeric(as.character(unlist(metacom.obj$Turnover)[1:5]))
		if(names(metacom.obj)[4] == 'Boundary'){bnd=as.numeric(metacom.obj$Boundary)}
    if(names(metacom.obj)[4] == 'Modularity'){bnd=as.numeric(metacom.obj$Modularity); bnd=c(bnd[1], bnd[3])}

		if(coh[3] >= 0.05){return("Random")}

	#Checkerboard
		if(coh[3] < 0.05 & (coh[1] > coh[4])){ return("Checkerboard")}
		
  # Nested subsets
		if(coh[3] < 0.05 & (coh[1] < coh[4]) & (turn[3] < 0.05) & (turn[2] > 0)){ return("Nested Subsets")}

	# Evenly spaced gradient
	if(coh[3] < 0.05 & (coh[1] < coh[4]) & (turn[3] < 0.05) & (turn[2] < 0) & (bnd[2] <0.05) & (bnd[1] < 1)){ return("Evenly Spaced Gradients")}

  # Gleasonian
	if(coh[3] < 0.05 & (coh[1] < coh[4]) & (turn[3] < 0.05) & (turn[2] < 0) & (bnd[2] > 0.05)){ return("Gleasonian")}

  # Clementsian
	if(coh[3] < 0.05 & (coh[1] < coh[4]) & (turn[3] < 0.05) & (turn[2] < 0) & (bnd[2] <0.05) & (bnd[1] > 1)){ return("Clementsian")}
	
}





