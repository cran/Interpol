AAdescriptor <-
function(data,descriptor=151,normalize=0){

	#################################################catch missing or bad input
	if(missing(data))
          stop("data argument missing")
        else
          if(!(is.character(data)))
            stop("invalid argument: data argument is required to be a vector of amino acids")
	if(!is.numeric(descriptor) | descriptor > 532 | descriptor < 1)
	  stop("invalid argument: descriptor is required to be 1 <= n <= 532")
	if(!is.numeric(normalize) | normalize > 2 | normalize < 0)
	  stop("invalid argument: descriptor is required to be 1 <= n <= 532")
	if(missing(descriptor))
		descriptor = 151
	if(missing(normalize))
		normalize = 0

	################################################Encoding
	
	#load("descriptors.RData")
	data(list)
	indices <- list[,1]
	list = list[,-1]

	descriptor = round(descriptor)
	normalize = round(normalize) 

	max_value = max(list[descriptor,])

	data = unlist(strsplit(data,""))
	data.encoded = c()

	for(n in 1:length(data)){
		data.encoded = c(data.encoded, list[descriptor, data[n]])		
	}
	################################################Normalization

	if(normalize==0){
		print(paste("data encoded with ", indices[descriptor], sep=""))
		return(data.encoded)	
	}
	if(normalize==1){
		tmp = c()
		for(i in 1:length(data.encoded)){
			tmp = c(tmp, (data.encoded[i]/max_value))
		}
		print(paste("data encoded with ", indices[descriptor], sep=""))
		return(tmp)
	}
	if(normalize==2){
		tmp = c()
		for(i in 1:length(data.encoded)){
			tmp = c(tmp, ((data.encoded[i]+max_value)/(2*max_value)))
		}
		print(paste("data encoded with ", indices[descriptor], sep=""))
		return(tmp)
	}
}

