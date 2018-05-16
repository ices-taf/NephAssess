poly.areas.intersect<- function(files, years)
{
	if( length(files) != length(years) )
	{
		stop("Mismatch between number of files and years")
	}
	
	if( length(files) == 1) 
	{
		stop("At least 2 files needed for intersection")
	}

	pol<- vector("list", length(files))
	
	for(i in 1:length(files) )
	{
		load(files[i])
		pol[[i]]<- res.list$Polygon
	}	
		
	for(i in 1:(length(pol)-1) )
	{
		if(i == 1)
		{
			int<- intersect(pol[[i]], pol[[i+1]])
		}
		
		if(i > 1)
		{
			int<- intersect(int, pol[[i+1]])
		}
	}
	return(int)
}	
	
	
	