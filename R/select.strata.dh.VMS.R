select.strata.dh.VMS <-
function (obj, vms.poly)
{
assign.type <-
  function (x)
  {
    if (any(x %in% 1:12)) return( "MUDDY SAND" )
    if (any(x %in% 13:22)) return( "FALSE" )
    return ("FALSE")
  }       

   obj$strata_type1 <- sapply( which.poly( obj, devils.hole.poly ), assign.type )
   obj$strata_type2 <- rep(NA, dim(obj)[1])
	
	for(i in 1:dim(obj)[1])
	{
		cond1<- sapply(get.pts(vms.poly), function(v) point.in.polygon(obj $ lon[i], obj $ lat[i], v$x, v$y)  > 0  &  v$hole == FALSE)
		cond2<- sapply(get.pts(vms.poly), function(v) point.in.polygon(obj $ lon[i], obj $ lat[i], v$x, v$y)  > 0  &  v$hole == TRUE)
		
		if(any(cond1) == TRUE & all(cond2) == FALSE)
		{
		obj[i,"strata_type2"]<- "VMS"
		} else {obj[i,"strata_type2"]<- "FALSE"}
	}
   return(obj)  
}