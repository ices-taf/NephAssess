survey.plots_clyde_jura <-

function(wk.dir, densities, coast.file, bubble.scale = 1)
{
	old.dir <- setwd(wk.dir)
	on.exit(setwd(old.dir), add = TRUE)

	uk.coast <- uk.coast

	dens<- read.table(densities, header=T, sep=",")
	
	year.vector<- sort(unique(dens$year))
	
	for(i in 1:length(year.vector))
	{
	
		jpeg(paste(wk.dir, "clyde", "_survey.plot_", year.vector[i], ".jpeg", sep = ""), width = 640, height = 640, 
			pointsize = 12, bg = "white", res = NA, quality=100)
	
		plot(uk.coast,  type="l", xlab="Lon", ylab="Lat",xlim=c(-6.1,-4.5), ylim=c(54.9,56.1), cex.axis=1.5, cex.lab=1.5)
		#, main="Firth of Clyde & Sound of Jura")
		#mtext(side=3, text="FU 13 Management Unit C", cex=0.7, line=0.5)
		polygon(x=c(-10,10,10,-10), y=c(50,50,70,70), col="skyblue")

		lines(x=c(-6,-6), y=c(56,55), lwd=2, lty=2)
		lines(x=c(-6,-5), y=c(55,55), lwd=2, lty=2)
		lines(x=c(-6,-5.5), y=c(56,56), lwd=2, lty=2)
			
		#Clyde
		lapply(clyde.poly[26:35],polygon,col="olivedrab",border=NA)
		lapply(clyde.poly[1:16],polygon,col="skyblue",border=NA)
		lapply(clyde.poly[44:49],polygon,col="green",border=NA)
		lapply(clyde.poly[17:25],polygon,col="darkgreen",border=NA)

		#Sound of Jura
		lapply(jura.poly[3:4],polygon,col="olivedrab",border=NA)
		lapply(jura.poly[7],polygon,col="green",border=NA)
		lapply(jura.poly[5:6],polygon,col="olivedrab",border=NA)
		lapply(jura.poly[1],polygon,col="skyblue",border=NA)
		lapply(jura.poly[2],polygon,col="darkgreen",border=NA)
		
		zero.dens<- subset(dens, year==year.vector[i] & average.density == 0)
		
		y<- subset(dens, year==year.vector[i])$lats
		x<- subset(dens, year==year.vector[i])$lons
		d<- subset(dens, year==year.vector[i])$average.density
		
		polygon(uk.coast, col="grey45")
		points(x, y, cex=bubble.scale*d, lwd=2)
		points(zero.dens$lons, zero.dens$lats, pch=4, lwd=1, col=2, cex=2)
		
		title(year.vector[i], cex.main=3.5)
		box()
	
	dev.off()
	
	}


}	
	
	