poly.areas.vms<- function(FU=c("nm", "sm", "cl", "dh"), vms.file, output.dir, years, gears, coast.file, alpha.value)
{
	load(vms.file)
	vms.trips<- subset(vms.trips, Year %in% years & Mesh.Size>=70 & Mesh.Size<=99 & Gear.Code %in% gears & NEP.per>=75 & Knots.SUM<=4.5)
	vms_points<- vms.trips[, c("Longitude", "Latitude")]
	names(vms_points)<- c("lon", "lat")
	yr<- paste(years, collapse="_")
	
	uk.coast<- coast.file
	
	#Function to exclude points arround ports (1 mile radius)
	circle <- function(x,y,r,n=100){t=seq(0,2*pi,len=n+1)[-1];return(cbind(x+r*sin(t),y+r*cos(t)))}
	R<- 2/60	#Radius is 2 nautical mile

	if(FU == "nm")
	{
		lon.lim<- c(-7, -5)
		lat.lim<- c(57.5, 59)
		
		ullapool<- c(-5.159368515014648,57.89487315527452)
		stornoway<- c(-6.389193534851074 , 58.20739448850014)
		gairloch<- c(-5.684802532196045 , 57.71086489297098)
		
		ports<- rbind(ullapool, stornoway, gairloch)
		
		for(p in 1: dim(ports)[1])
		{
			port.temp<- circle(x=ports[p,1], y=ports[p,2], r=R, n=100)
			p.in.p.temp<- point.in.polygon(point.x = vms_points$lon, point.y = vms_points$lat, pol.x = port.temp[,1], pol.y = port.temp[,2])
			vms_points<- vms_points[p.in.p.temp!=1,1:2]	#remove points that lie inside a 1 nautical mile circle around ports
		}
		
		#Exclude sea loch area in gruynard bay (included in the sea loch work)
		gruynard.bay<- list(x=c(-5.461, -5,-5,-5.612), y=c(58.077, 58,57.5,57.925))
		#polygon(gruynard.bay$x, gruynard.bay$y)
		p.in.p.temp<- point.in.polygon(point.x = vms_points$lon, point.y = vms_points$lat, pol.x = gruynard.bay$x, pol.y = gruynard.bay$y)
		vms_points<- vms_points[p.in.p.temp!=1,1:2]	#remove points that lie inside a 1 nautical mile circle around ports
	}

	if(FU == "sm")
	{
		lon.lim<- c(-8, -5)
		lat.lim<- c(56, 57.5)

		mallaig<- c(-5.827367, 57.006434)
		oban<- c(-5.476985, 56.411622)
		barra<- c(-7.403026, 57.008987)

		ports<- rbind(mallaig, oban, barra)
		
		for(p in 1: dim(ports)[1])
		{
			port.temp<- circle(x=ports[p,1], y=ports[p,2], r=R, n=100)
			p.in.p.temp<- point.in.polygon(point.x = vms_points$lon, point.y = vms_points$lat, pol.x = port.temp[,1], pol.y = port.temp[,2])
			vms_points<- vms_points[p.in.p.temp!=1,1:2]	#remove points that lie inside a 1 nautical mile circle around ports
		}
	}

	if(FU == "cl")
	{
		lon.lim<- c(-6, -4.5)
		lat.lim<- c(55, 56)

		troon<- c(-4.678974, 55.547572)
		girvan<- c(-4.863167, 55.24502)
		largs<- c(-4.871294, 55.795027)
		campbelltown<- c(-5.601343, 55.424435)
		tarbert<- c(-5.833397, 55.969668)
		carradale<- c(-5.469818, 55.596389)
		brodick<- c(-5.133619, 55.580431)

		ports<- rbind(troon, girvan, largs, campbelltown, tarbert, carradale, brodick)
		
		for(p in 1: dim(ports)[1])
		{
			port.temp<- circle(x=ports[p,1], y=ports[p,2], r=R, n=100)
			p.in.p.temp<- point.in.polygon(point.x = vms_points$lon, point.y = vms_points$lat, pol.x = port.temp[,1], pol.y = port.temp[,2])
			vms_points<- vms_points[p.in.p.temp!=1,1:2]	#remove points that lie inside a 1 nautical mile circle around ports
		}
	}
	
	if(FU == "dh")
	{
		lon.lim<- c(0, 2)
		lat.lim<- c(56, 57.5)
	}
	
	vms_points<- subset(vms_points, (lon > lon.lim[1] & lon < lon.lim[2]) & (lat > lat.lim[1] & lat < lat.lim[2]))
	p.in.p.uk.coast<- point.in.polygon(point.x = vms_points$lon, point.y = vms_points$lat, pol.x = uk.coast$lons, pol.y = uk.coast$lats)
	vms_points<- vms_points[p.in.p.uk.coast!=1,1:2]	#remove points that lie (strictly) inside uk.coast
	vms_points<- vms_points[!duplicated(vms_points),]
	set.seed(1234)
	rand<- rnorm(dim(vms_points)[1], 0, sd=0.00001)
	vms_points[,1]<- vms_points[,1] + rnorm(dim(vms_points)[1], 0, sd=0.00001)
	vms_points[,2]<- vms_points[,2] + rnorm(dim(vms_points)[1], 0, sd=0.00001)
		
	time<- system.time({	polygs<- alpha.hull.polygons(lons=vms_points$lon, lats=vms_points$lat, a=alpha.value)	})[3]
	print(paste("alpha hull run time: ", round(time/60,1), "mins"))
	cat("\n")
	
	groups.list<- polygs
	polygon.list<- vector("list", length(groups.list))
		
	uk.coast[17815,]<- c(NA, NA)
	nas<- rownames(subset(uk.coast, is.na( lons) == T))
	n.polys<- length(nas) - 1
	uk.poly.list<- vector("list", length=n.polys)

	for(i in 1:n.polys)
	{
		uk.coast[nas[i+1], ]
		rows<- seq(as.numeric(nas[i]) + 1, as.numeric(nas[i+1]) - 1)
		uk.poly.list[[i]]<- uk.coast[rows, ]
	}

	jpeg(paste("vms_points_", yr, "_", FU, ".jpeg", sep=""), width = 600, height = 700, pointsize = 12, bg = "white", res = NA, quality=100)
	plot(uk.coast,  type="l", xlab="Lon", ylab="Lat",xlim=c(lon.lim[1]-0.2, lon.lim[2]+0.2), ylim=c(lat.lim[1]-0.2, lat.lim[2]+0.2))
	polygon(x=c(-10,10,10,-10), y=c(50,50,70,70), col="skyblue")
	polygon(x=c(lon.lim[1],lon.lim[1],lon.lim[2],lon.lim[2]), y=c(lat.lim[1],lat.lim[2],lat.lim[2],lat.lim[1]), lwd=2, lty=2)
	polygon(uk.coast, col="grey")
	points(vms_points, pch=16, cex=0.1)
	dev.off()
	
	jpeg(paste("poly_", yr, "_", FU, ".jpeg", sep=""), width = 600, height = 700, pointsize = 12, bg = "white", res = NA, quality=100)
	plot(uk.coast,  type="l", xlab="Lon", ylab="Lat",xlim=c(lon.lim[1]-0.2, lon.lim[2]+0.2), ylim=c(lat.lim[1]-0.2, lat.lim[2]+0.2))
	polygon(x=c(-10,10,10,-10), y=c(50,50,70,70), col="skyblue")
	polygon(x=c(lon.lim[1],lon.lim[1],lon.lim[2],lon.lim[2]), y=c(lat.lim[1],lat.lim[2],lat.lim[2],lat.lim[1]), lwd=2, lty=2)
	polygon(uk.coast, col="grey")

	time<- system.time({	
	area.fu<- lapply(1:length(groups.list), function(v)
	{
		temp<- as(groups.list[[v]], "gpc.poly")
		for(i in 1:length(uk.poly.list))
		{
			temp.uk<- as(uk.poly.list[[i]], "gpc.poly")
			temp<- setdiff(temp, temp.uk)
		}

		v.vec<- 1:length(groups.list)
		v.vec<- v.vec[v.vec != v]

		for(j in 1:length(v.vec))
		{
			temp<- setdiff(temp, as(groups.list[[v.vec[j]]], "gpc.poly"))
		}

		pts<- get.pts(temp)

		if(length(pts)>0)
		{
			for(k in 1:length(pts))
			{
				dist<- lat.lon.distances(cbind(pts[[k]]$x, pts[[k]]$y))
				pts[[k]]$x<- dist[,1]
				pts[[k]]$y<- dist[,2]
			}
			temp2<- new("gpc.poly", pts=pts)
			area.p<- area.poly(temp2)
		
			if( area.p<10 )
			{
				area.p<- 0
			}
			
			if( area.p>=10 )
			{
				plot(temp, add=T, poly.args = list(col=2, border=NA))
			}
			
			#plot holes
			pts<- get.pts(temp)
			logic.pts.hole<- lapply(1:length(pts), function(x) 
			{
				pts[[x]]$hole == T
			})
				
			pts.hole<- pts[logic.pts.hole==T]
			
			for(i in 1:length(uk.poly.list))
			{
				temp.uk<- as(uk.poly.list[[i]], "gpc.poly")
				temp.pts<- setdiff(new("gpc.poly", pts=pts.hole), temp.uk)
				plot(temp.pts, add=T, poly.args = list(col="skyblue", border=NA))
			}
		}
		
		if(length(pts)==0)
		{
			area.p<- 0
		}
		area.p
	})
	})[3]
	print(paste("Polygon manipulation run time: ", round(time/60,1), "mins"))
	cat("\n")
	
	dev.off()
	
	polygon.list<- lapply(1:length(groups.list), function(v)
	{
		temp<- as(groups.list[[v]], "gpc.poly")
		for(i in 1:length(uk.poly.list))
		{
			temp.uk<- as(uk.poly.list[[i]], "gpc.poly")
			temp<- setdiff(temp, temp.uk)
		}

		v.vec<- 1:length(groups.list)
		v.vec<- v.vec[v.vec != v]

		for(j in 1:length(v.vec))
		{
			temp<- setdiff(temp, as(groups.list[[v.vec[j]]], "gpc.poly"))
		}
		temp
	})
	
	polygon.list<- polygon.list[which(area.fu>0)]
		
	#Union of all polygons for the year
	for(i in 1:(length(polygon.list)-1) )
	{
		if(i == 1)
		{
			pol<- union(polygon.list[[i]], polygon.list[[i+1]])
		}
		
		if(i > 1)
		{
			pol<- union(pol, polygon.list[[i+1]])
		}
	}
	
	final.polygon<- pol
		
	area.tot<- sum(sapply(area.fu, sum))
	res.list<- list(Year=paste(years, collapse="_"), Area=area.tot, Polygon=final.polygon)
	save(res.list, file=paste("poly_", yr, "_", FU, ".rda", sep=""))
	return(res.list)
}
