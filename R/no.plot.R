no.plot <-

function(wk.dir, survey.year, coast.file, fu.dens, f.u = c("devils hole"), bubble.scale = 1)
{
	old.dir <- setwd(wk.dir)
	on.exit(setwd(old.dir), add = TRUE)

#	uk.coast <- read.table("scottish_coast.txt", header=T)

	get.fname <-
	function (base.name)
	{
		fname <- function(i) paste(base.name, i, ".jpg", sep="")
		out <- fname(i <- 1)
		while (file.exists(out))
		out <- fname(i <- i + 1)
		return ( out )
	}
	
	jpeg(get.fname(paste(wk.dir, f.u, "_survey.plot_",survey.year,".", sep = "")), width = 640, height = 640, 
		pointsize = 12, bg = "white", res = NA, quality=100)
	
	if (f.u== "noup")  
	{
    plot(uk.coast,  type="l", xlab="Lon", ylab="Lat",xlim=c(-4.1,-2.9), ylim=c(59,59.6))
		#, main="Fladen")
		#mtext(side=3, text="FU 07 Management Unit G", cex=0.7, line=0.5)
		polygon(x=c(-10,10,10,-10), y=c(50,50,70,70), col="skyblue")
  
	lines(x=c(-4,-3), y=c(59,59), lwd=2, lty=2)
	lines(x=c(-3,-3), y=c(59,59.5), lwd=2, lty=2)
	lines(x=c(-4,-3), y=c(59.5,59.5), lwd=2, lty=2)
	lines(x=c(-4,-4), y=c(59,59.5), lwd=2, lty=2)
		
    lapply(noup.poly[1:2],polygon,col="olivedrab",border=NA)
	lapply(noup.poly[3:5],polygon,col="skyblue",border=NA)
    
#		points(fu.dens$lons, fu.dens$lats, cex=bubble.scale*fu.dens$average.density, lwd=3)
    points(fu.dens$lons[fu.dens$average.density>0], fu.dens$lats[fu.dens$average.density>0], 
        cex=bubble.scale*fu.dens$average.density[fu.dens$average.density>0], lwd=3)
    points(fu.dens$lons[fu.dens$average.density==0], fu.dens$lats[fu.dens$average.density==0], 
        pch="x",cex=2, lwd=2,col="red")
		polygon(uk.coast, col="grey75")
		title(survey.year, cex.main=2.5)
		box()
		
	} 
	
	dev.off()

}
	
	
	
	
	
	