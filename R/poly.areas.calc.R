poly.areas.calc<- function(polyg)
{
	pts<- get.pts(polyg)
	for(k in 1:length(pts))
	{
		dist<- lat.lon.distances(cbind(pts[[k]]$x, pts[[k]]$y))
		pts[[k]]$x<- dist[,1]
		pts[[k]]$y<- dist[,2]
	}
	area<- area.poly(new("gpc.poly", pts=pts))
	return(area)
}