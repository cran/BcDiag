#----------------------

#' a supportive function for profileplot.bic,
#' indexedBic: a function to check the name of the method
#' and returns the list parameter of the required indcies of the biclust
#' @ bres, the biclust object
#' @ mname, name of the method to be applied for the biclust
#' @ dset,bres,mname,bnum has similar explanation as summary.bic function
#' outcome : returns the two indcies; the indg and indc
#' @ indg; index for the biclust genes.
#' @ indc; index for the biclust conditions.

#----------------------
indexedBic<-function(dset,bres,mname=c("fabia","isa2","biclust"),bnum){
	# which biclust object is it; 
	check<-match.arg(mname)
	l<-bnum
	if(check=="fabia"){
		#Extract biclusters:
		#get the biclust index inside the dset 
		if(!require(fabia)){
			stop("The `fabia' package is required for this")
		}

		resf <- extractBic(bres)
		bg<-resf$numn[1,]$numng
		bc<-resf$numn[1,]$numnp
		# the two indecies	
		indg<-bg
		indc<-bc
	}
	if(check=="isa2"){
		#convert to biclust and get the biclust indecies
		if(!require(isa2)& !require(biclust)){
			stop("The `biclust' and 'isa2' packages are required for this")
		}

		resi<-isa.biclust(bres)
		indg<-which(resi@RowxNumber[,l])
		indc<-which(resi@NumberxCol[l,])
	
	}
	if(check=="biclust"){
		if(!require(biclust)){
			stop("The `biclust' package is required for this")
		}

		indg<-which(bres@RowxNumber[,l])
		indc<-which(bres@NumberxCol[l,])
	
	}
	return(list(indg,indc))
}
