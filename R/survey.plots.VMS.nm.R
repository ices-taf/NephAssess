survey.plot.VMS.nm <-

function(wk.dir, survey.year, coast.file, fu.dens, f.u = c("north minch"), bubble.scale = 1, legend = NULL)
{
	old.dir <- setwd(wk.dir)
	on.exit(setwd(old.dir), add = TRUE)

	uk.coast <- uk.coast

	zero.dens<- subset(fu.dens, average.density == 0)
	
	
	get.fname <-
	function (base.name)
	{
		fname <- function(i) paste(base.name, i, ".jpg", sep="")
		out <- fname(i <- 1)
		while (file.exists(out))
		out <- fname(i <- i + 1)
		return ( out )
	}
	
	jpeg(get.fname(paste(wk.dir, f.u, "_survey.plot.VMS", sep = "")), width = 640, height = 640, 
		pointsize = 12, bg = "white", res = NA, quality=100)
	
	
	if (f.u== "north minch")  
	{
		plot(uk.coast,  type="l", xlab="Lon", ylab="Lat",xlim=c(-7.2, -4.8), ylim=c(57.3, 59), cex.axis=1.5, cex.lab=1.5)
		#, main="North Minch")
		#mtext(side=3, text="FU 11 Management Unit C", cex=0.7, line=0.5)
		polygon(x=c(-10,10,10,-10), y=c(50,50,70,70), col="skyblue")

		lines(x=c(-7,-5), y=c(57.5,57.5), lwd=2, lty=2)
		lines(x=c(-7,-5), y=c(59,59), lwd=2, lty=2)
		lines(x=c(-7,-7), y=c(57.5,59), lwd=2, lty=2)
		lines(x=c(-5,-5), y=c(57.5,59), lwd=2, lty=2)

		plot(poly_union_2007_2011_nm.rda, add=T, poly.args = list(col=2, border=NA))
		
		#plot holes
		pts<- get.pts(poly_union_2007_2011_nm.rda)
		logic.pts.hole<- lapply(1:length(pts), function(x) 
		{
			pts[[x]]$hole == T
		})
		
		pts.hole<- pts[logic.pts.hole==T]
		pts.hole<- new("gpc.poly", pts=pts.hole)
		plot(pts.hole, add=T, poly.args = list(col="skyblue", border=NA))
				
		#lapply(north.minch.poly[18:35],polygon,col="olivedrab",border=NA)
		#lapply(north.minch.poly[1:7],polygon,col="skyblue",border=NA)
		#lapply(north.minch.poly[36:56],polygon,col="green",border=NA)
		#lapply(north.minch.poly[8:17],polygon,col="darkgreen",border=NA)
		
		polygon(uk.coast, col="grey45")
		points(fu.dens$lons, fu.dens$lats, cex=bubble.scale*fu.dens$average.density, lwd=2)
		points(zero.dens$lons, zero.dens$lats, pch=4, lwd=1, col=4, cex=1)
		title(survey.year, cex.main=3.5)
		box()
		
		if(legend == TRUE)
		{
			x=rep(-5, 5)
			y=seq(57.3, 57.9, length=5)
			ds<- bubble.scale*fu.dens$average.density[fu.dens$average.density>0]
			z= c(0.1*max(ds),0.25*max(ds),0.5*max(ds),0.75*max(ds),max(ds) )
			z.txt<- round(z/bubble.scale,1)
			points(x, y, cex=z, lwd=2)
			text(x+0.2, y, z.txt, font=2)
		}
		
	} 
	
	dev.off()

}
	
	
	