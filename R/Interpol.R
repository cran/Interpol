Interpol <-
function(data,dims,method="linear"){

	## catch missing or bad input
	if(missing(data))
          stop("data argument missing")
        else
          if(!is.vector(data))
            stop("invalid argument: data argument is required to be a vector of D features")
        if(!is.numeric(dims) | (dims < 2))
          stop("invalid argument: target dimension is required to be a positive integer value > 1")
	if(!is.character(method) | !(method=="linear" | method=="spline" | method=="natural" | method =="fmm" | method =="periodic"))
	  stop("invalid argument: method is required to be linear, spline, natural, fmm or periodic")
	if(missing(method))
		method = "linear"
	
	################################linear interpolation
	if(method == "linear"){
		list = data

		length = dims
 		if(length == 2){return(c(list[1], list[length(list)]))}
		factor = (length(list) - 1) / (length - 1)

		output = list[1]

		for(i in 1:(length-2)){
			normIndex = factor * i
			floorIndex = floor(normIndex)+1
			tmp = list[floorIndex]*(floorIndex+1-normIndex) + list[floorIndex+1] * (normIndex-floorIndex)
			output = c(output, tmp)			
		}
		output = c(output, list[length(list)])	
		print(paste("data is ", method, " interpolated to length ", dims, sep=""))
		return(output)
	}
	################################spline interpolation
	if(method == "spline"){
		y = data	
		x = seq(1:length(y))
		f_fmm = splinefun(x,y, method="monoH.FC")
		factor = length(y)/dims
		tmp = c()	
		
		for(n in 1:dims){
			tmp = c(tmp, n*factor)
		}
		output = f_fmm(tmp)
		print(paste("data is ", method, " interpolated to length ", dims, sep=""))
		return(output)
	}
	################################fmm interpolation
	if(method == "fmm"){
		y = data	
		x = seq(1:length(y))
		f_fmm = splinefun(x,y, method="fmm")
		factor = length(y)/dims
		tmp = c()	
		
		for(n in 1:dims){
			tmp = c(tmp, n*factor)
		}
		output = f_fmm(tmp)
		print(paste("data is ", method, " interpolated to length ", dims, sep=""))
		return(output)
	}
	################################natural interpolation
	if(method == "natural"){
		y = data	
		x = seq(1:length(y))
		f_fmm = splinefun(x,y, method="natural")
		factor = length(y)/dims
		tmp = c()	
		
		for(n in 1:dims){
			tmp = c(tmp, n*factor)
		}
		output = f_fmm(tmp)
		print(paste("data is ", method, " interpolated to length ", dims, sep=""))
		return(output)
	}
	################################periodic interpolation
	if(method == "periodic"){
		y = data	
		x = seq(1:length(y))
		f_fmm = splinefun(x,y, method="periodic")
		factor = length(y)/dims
		tmp = c()	
		
		for(n in 1:dims){
			tmp = c(tmp, n*factor)
		}
		output = f_fmm(tmp)
		print(paste("data is ", method, " interpolated to length ", dims, sep=""))
		return(output)
	}
}

