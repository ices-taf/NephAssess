poly.areas.plot<- function(FU=c("nm", "sm", "cl", "dh"), polyg)
{
	if(FU == "nm")
	{
		lon.lim<- c(-7, -5)
		lat.lim<- c(57.5, 59)
	}

	if(FU == "sm")
	{
		lon.lim<- c(-8, -5)
		lat.lim<- c(56, 57.5)
	}

	if(FU == "cl")
	{
		lon.lim<- c(-6, -4.5)
		lat.lim<- c(55, 56)
	}
		
	if(FU == "dh")
	{
		lon.lim<- c(0, 2)
		lat.lim<- c(56, 57.5)
	}

	plot(uk.coast,  type="l", xlab="Lon", ylab="Lat",xlim=c(lon.lim[1]-0.2, lon.lim[2]+0.2), ylim=c(lat.lim[1]-0.2, lat.lim[2]+0.2))
	polygon(x=c(-10,10,10,-10), y=c(50,50,70,70), col="skyblue")
	polygon(x=c(lon.lim[1],lon.lim[1],lon.lim[2],lon.lim[2]), y=c(lat.lim[1],lat.lim[2],lat.lim[2],lat.lim[1]), lwd=2, lty=2)
	polygon(uk.coast, col="grey")
	plot(polyg, add=T, poly.args = list(col=2, border=NA))
	
	#plot holes
	pts<- get.pts(polyg)
	logic.pts.hole<- lapply(1:length(pts), function(x) 
	{
		pts[[x]]$hole == T
	})
	
	pts.hole<- pts[logic.pts.hole==T]
	pts.hole<- new("gpc.poly", pts=pts.hole)
	plot(pts.hole, add=T, poly.args = list(col="skyblue", border=NA))
}